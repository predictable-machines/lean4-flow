import Flow.Core.Collector
import Flow.Core.Flow
import Flow.Core.Flows
import Flow.Internal.Uuid
import Std

namespace Flow.Core

open Flow.Internal

/-! # SharedFlow - Hot, multicast streams

A SharedFlow is a hot stream that broadcasts values to multiple subscribers.
Unlike cold Flow, SharedFlow exists independently of collectors and can have
multiple concurrent subscribers receiving the same emissions.

## Key Characteristics

1. **Hot**: Emissions occur regardless of subscribers
2. **Multicast**: All active subscribers receive the same emissions
3. **Replay**: New subscribers can receive the last N emitted values
4. **Buffer**: Handles overflow when emission rate exceeds collection rate

## Comparison to Kotlin SharedFlow

```kotlin
// Kotlin
val sharedFlow = MutableSharedFlow<Int>(replay = 2)
sharedFlow.emit(1)
val job = sharedFlow.subscribe { println(it) }
```

```lean
-- Lean
let sharedFlow ← MutableSharedFlow.create (replay := 2)
sharedFlow.emit 1
let cancel ← sharedFlow.subscribe IO.println
```

## Concurrency

Uses `Std.CloseableChannel` for per-subscriber message passing, `Std.Mutex` for
thread-safe state access, and `IO.Promise` for flush synchronization.
Each subscriber gets a dedicated processing task that blocks on channel recv.
Closing the channel unblocks recv (returns `none`), terminating the task.
-/

/-- Buffer overflow strategy when buffer is full -/
inductive BufferOverflow where
  | dropOldest : BufferOverflow  -- Remove oldest item from buffer
  | dropLatest : BufferOverflow  -- Drop the new item being emitted
  deriving Repr, BEq

/-- Internal message type for subscriber channels -/
inductive SubMsg (α : Type) where
  | item : α → SubMsg α
  | flush : IO.Promise Unit → SubMsg α

/-- A subscriber backed by a dedicated closeable channel and processing task -/
structure Subscriber (α : Type) where
  id : Uuid
  channel : Std.CloseableChannel (SubMsg α)
  task : Task (Except IO.Error Unit)
  completion : IO.Promise Unit
  bufferSize : Nat
  onBufferOverflow : BufferOverflow

namespace Subscriber

/-- Processing loop: blocks on channel recv, dispatches messages.
    Returns when channel is closed (recv returns `none`). -/
partial def processingLoop
    (ch : Std.CloseableChannel.Sync (SubMsg α))
    (action : α → IO Unit)
    : IO Unit := do
  match ← ch.recv with
  | some (.item value) =>
    try action value catch e => IO.eprintln s!"Subscriber error: {e}"
    processingLoop ch action
  | some (.flush promise) =>
    IO.Promise.resolve () promise
    processingLoop ch action
  | none => return ()

/-- Create a new subscriber with a dedicated processing task. -/
def create
    (id : Uuid)
    (action : α → IO Unit)
    (bufferSize : Nat)
    (onBufferOverflow : BufferOverflow)
    : IO (Subscriber α) := do
  let channel ← Std.CloseableChannel.new (α := SubMsg α)
  let completion ← IO.Promise.new (α := Unit)
  let task ← IO.asTask (prio := .default) do
    processingLoop channel.sync action
    IO.Promise.resolve () completion
  pure { id, channel, task, completion, bufferSize, onBufferOverflow }

/-- Send a value to the subscriber's channel. No-op if channel is closed. -/
def enqueue (sub : Subscriber α) (value : α) : IO Unit :=
  (sub.channel.sync.send (.item value)).catchExceptions fun _ => pure ()

/-- Wait until all queued items are processed. No-op if channel is closed. -/
def flush (sub : Subscriber α) : IO Unit := do
  if ← sub.channel.isClosed then return
  let promise ← IO.Promise.new (α := Unit)
  let sent ← (sub.channel.sync.send (.flush promise) *> pure true).catchExceptions fun _ => pure false
  if sent then
    discard <| IO.wait promise.result!

/-- Close the channel, which terminates the processing loop. -/
def close (sub : Subscriber α) : IO Unit :=
  sub.channel.close.catchExceptions fun _ => pure ()

end Subscriber

/-- Internal state of a SharedFlow -/
structure SharedFlowState (α : Type) where
  /-- Active subscribers with unique IDs -/
  subscribers : Array (Subscriber α) := #[]
  /-- Replay cache (last N emissions for new subscribers) -/
  replayCache : Array α := #[]
  /-- Maximum replay cache size -/
  replay : Nat := 0
  /-- Maximum buffer size -/
  bufferSize : Nat := 64
  /-- What to do when buffer is full -/
  onBufferOverflow : BufferOverflow := .dropOldest
  /-- Whether the flow has been closed -/
  isClosed : Bool := false
  /-- Close actions for subscriber channels and child flows.
      Executed on close to tear down all managed resources. -/
  closeActions : Array (IO Unit) := #[]

namespace SharedFlowState

/-- Add a value to the replay cache, respecting the replay limit -/
def addToReplay (state : SharedFlowState α) (value : α) : SharedFlowState α :=
  if state.replay == 0 then
    state
  else
    let newCache := state.replayCache.push value
    let trimmedCache :=
      if newCache.size > state.replay then
        newCache.extract (newCache.size - state.replay) newCache.size
      else
        newCache
    { state with replayCache := trimmedCache }

end SharedFlowState

/-- Hot stream that broadcasts to multiple subscribers.

    Use `MutableSharedFlow.create` to create instances.
-/
structure SharedFlow (α : Type) where
  /-- Mutable state protected by mutex for thread-safe access -/
  state : Std.Mutex (SharedFlowState α)
  /-- Parent flow's flush function for cascading flush up the chain -/
  parentFlush : Option (IO Unit) := none

namespace SharedFlow

/-- Unsubscribe a subscriber by their ID.

    This is typically called via the cancellation function returned by `subscribe`,
    but can also be called directly if you have the subscriber ID.
    Cancels any pending work for the subscriber before removing it.
-/
def unsubscribe (flow : SharedFlow α) (subscriberId : Uuid) : IO Unit :=
  flow.state.atomically do
    let state ← get
    match state.subscribers.find? (·.id == subscriberId) with
    | some s => s.close
    | none => pure ()
    set { state with subscribers := state.subscribers.filter (·.id != subscriberId) }

/-- Create and register a subscriber without acquiring the mutex.
    Caller must hold the mutex. Returns updated state and a Subscription handle. -/
def addSubscriber
    (flow : SharedFlow α)
    (state : SharedFlowState α)
    (action : α → IO Unit)
    (skipReplay : Bool := false)
    : IO (SharedFlowState α × Subscription) := do
  if state.isClosed then throw (IO.userError "Cannot subscribe to closed SharedFlow")
  let subscriberId ← Uuid.v4
  let subscriber ← Subscriber.create subscriberId action state.bufferSize state.onBufferOverflow
  unless skipReplay do
    for value in state.replayCache do
      subscriber.enqueue value
  let newState := { state with
    subscribers := state.subscribers.push subscriber
    closeActions := state.closeActions.push subscriber.close }
  let subscription : Subscription :=
    { unsubscribe := flow.unsubscribe subscriberId
      waitForCompletion := do
        let _ ← IO.wait subscriber.completion.result! }
  pure (newState, subscription)

/-- Subscribe to emissions from a SharedFlow. Returns a cancellation function.

    This is the main way to consume a SharedFlow. The action is called for
    each emission until you invoke the returned cancellation function.

    Example:
    ```lean
    let flow ← MutableSharedFlow.create (α := Nat)
    let cancel ← flow.subscribe IO.println
    flow.emit 1  -- prints "1"
    flow.emit 2  -- prints "2"
    cancel       -- stop subscribing
    ```
-/
def subscribe (flow : SharedFlow α) (action : α → IO Unit) : IO (Subscription) :=
  flow.state.atomically do
    let state ← get
    let (newState, subscription) ← addSubscriber flow state action
    set newState
    pure subscription

/-- Get the number of active subscribers -/
def subscriberCount (flow : SharedFlow α) : IO Nat :=
  flow.state.atomically do return (← get).subscribers.size

/-- Check if the flow is closed -/
def isClosed (flow : SharedFlow α) : IO Bool :=
  flow.state.atomically do return (← get).isClosed

/-- Get the current replay cache -/
def replayCache (flow : SharedFlow α) : IO (Array α) :=
  flow.state.atomically do return (← get).replayCache

/-- Wait until all subscribers have finished processing their buffers.
    If this flow was derived from a parent, flushes the parent first to ensure
    all buffered items have propagated through the transformation chain. -/
def flush (flow : SharedFlow α) : IO Unit := do
  -- Flush parent first to ensure all items have propagated
  match flow.parentFlush with
  | some parentFlush => parentFlush
  | none => pure ()
  -- Then flush own subscribers
  flow.state.atomically do
    for subscriber in (← get).subscribers do
      subscriber.flush

end SharedFlow

/-- Mutable SharedFlow that can emit values.

    This is the producer side of SharedFlow.
-/
structure MutableSharedFlow (α : Type) extends SharedFlow α where

namespace MutableSharedFlow

/-- Create a new MutableSharedFlow.

    Parameters:
    - `replay`: Number of values to replay to new subscribers (default: 0)
    - `bufferSize`: Maximum buffer size (default: 64)
    - `onBufferOverflow`: What to do when buffer is full (default: dropOldest)

    Example:
    ```lean
    let flow ← MutableSharedFlow.create (replay := 2) (bufferSize := 32)
    ```
-/
def create
    (replay : Nat := 0)
    (bufferSize : Nat := 64)
    (onBufferOverflow : BufferOverflow := .dropOldest)
    (parentFlush : Option (IO Unit) := none)
    : IO (MutableSharedFlow α) := do
  let initialState : SharedFlowState α := {
    replay := replay
    bufferSize := bufferSize
    onBufferOverflow := onBufferOverflow
  }
  let state ← Std.Mutex.new initialState
  pure { state, parentFlush }

/-- Emit a value to all current subscribers.

    Sends the value to each subscriber's channel for processing.

    Example:
    ```lean
    let flow ← MutableSharedFlow.create (α := String)
    let cancel1 ← flow.subscribe IO.println
    let cancel2 ← flow.subscribe (fun s => IO.println s!"Sub2: {s}")
    flow.emit "Hello"  -- both subscribers receive "Hello"
    ```
-/
def emit (flow : MutableSharedFlow α) (value : α) : IO Unit :=
  flow.state.atomically do
    let state ← get
    if state.isClosed then throw (IO.userError "Cannot emit to closed SharedFlow")
    for subscriber in state.subscribers do
      subscriber.enqueue value
    set (state.addToReplay value)

/-- Emit multiple values in sequence.

    Example:
    ```lean
    flow.emitAll [1, 2, 3, 4, 5]
    ```
-/
def emitAll (flow : MutableSharedFlow α) (values : List α) : IO Unit := do
  for value in values do
    flow.emit value

/-- Close the flow, cancelling all subscribers and preventing new emissions and subscriptions.
    Also cancels any child flows created via map/filter/filterMap. -/
def close (flow : MutableSharedFlow α) : IO Unit := do
  let actions ← flow.state.atomically do
    let state ← get
    set { state with isClosed := true, subscribers := #[], closeActions := #[] }
    pure state.closeActions
  for action in actions do
    action

def filterMap (flow : SharedFlow α) (f : α → Option β) : IO (MutableSharedFlow β) := do
  flow.state.atomically do
    let state ← get
    let chosen ← MutableSharedFlow.create
      (α := β)
      (replay := state.replay)
      (bufferSize := state.bufferSize)
      (onBufferOverflow := state.onBufferOverflow)
      (parentFlush := flow.flush)

    let (stateWithSub, _) ← SharedFlow.addSubscriber flow state fun a => do
      match f a with
      | some b => chosen.emit b
      | none => pure ()

    set { stateWithSub with closeActions := stateWithSub.closeActions.push chosen.close }
    pure chosen

def combine (flow1 : MutableSharedFlow α) (flow2 : MutableSharedFlow β) : IO (MutableSharedFlow (Sum α β)) := do
  flow1.state.atomically do
    let state1 ← get
    let (combined, state1') ← flow2.state.atomically do
      let state2 ← get
      let combined ← MutableSharedFlow.create
        (α := Sum α β)
        (replay := state1.replay + state2.replay)
        (bufferSize := state1.bufferSize + state2.bufferSize)
        (onBufferOverflow := state1.onBufferOverflow) -- TODO: Solve conflict
        (parentFlush := some (do flow1.toSharedFlow.flush; flow2.toSharedFlow.flush))
      let (state1', _) ← SharedFlow.addSubscriber flow1.toSharedFlow state1 (fun a => combined.emit (Sum.inl a))
      let (state2', _) ← SharedFlow.addSubscriber flow2.toSharedFlow state2 (fun b => combined.emit (Sum.inr b))
      set { state2' with closeActions := state2'.closeActions.push combined.close }
      pure (combined, state1')
    set { state1' with closeActions := state1'.closeActions.push combined.close }
    pure combined

end MutableSharedFlow

namespace SharedFlow

def combine (flow1 : SharedFlow α) (flow2 : SharedFlow β) : IO (SharedFlow (Sum α β)) := do
  flow1.state.atomically do
    let state1 ← get
    let (combined, state1') ← flow2.state.atomically do
      let state2 ← get
      let combined ← MutableSharedFlow.create
        (α := Sum α β)
        (replay := state1.replay + state2.replay)
        (bufferSize := state1.bufferSize + state2.bufferSize)
        (onBufferOverflow := state1.onBufferOverflow) -- TODO: Solve conflict
        (parentFlush := some (do flow1.flush; flow2.flush))
      let (state1', _) ← addSubscriber flow1 state1 (fun a => combined.emit (Sum.inl a))
      let (state2', _) ← addSubscriber flow2 state2 (fun b => combined.emit (Sum.inr b))
      set { state2' with closeActions := state2'.closeActions.push combined.close }
      pure (combined, state1')
    set { state1' with closeActions := state1'.closeActions.push combined.close }
    pure combined.toSharedFlow

end SharedFlow

instance : DerivedFlow SharedFlow where
  derive source handler :=
    source.state.atomically do
      let state ← get
      let derived ← MutableSharedFlow.create
        (replay := state.replay)
        (bufferSize := state.bufferSize)
        (onBufferOverflow := state.onBufferOverflow)
        (parentFlush := source.flush)
      let (s, _) ← SharedFlow.addSubscriber source state (handler derived.emit)
      set { s with closeActions := s.closeActions.push derived.close }
      pure derived.toSharedFlow

instance : DerivedFlow MutableSharedFlow where
  derive source handler :=
    source.state.atomically do
      let state ← get
      let derived ← MutableSharedFlow.create
        (replay := state.replay)
        (bufferSize := state.bufferSize)
        (onBufferOverflow := state.onBufferOverflow)
        (parentFlush := source.toSharedFlow.flush)
      let (s, _) ← SharedFlow.addSubscriber source.toSharedFlow state (handler derived.emit)
      set { s with closeActions := s.closeActions.push derived.close }
      pure derived

instance : Flows SharedFlow IO Id where
  combine := SharedFlow.combine
  subscribe := fun flow action => SharedFlow.subscribe flow (action ·)
  flush := SharedFlow.flush
  toList := fun flow => do
    let list ← IO.mkRef ([] : List _)
    -- `Id` is @[reducible] so Lean reduces `a : Id α` to `α` in the callback,
    -- but the list ref keeps type `List (Id α)` from the return type, causing an
    -- HAppend mismatch. Annotating `a : Id _` preserves the wrapper so both sides match.
    let sub ← SharedFlow.subscribe flow fun (a : Id _) => list.modify (a :: ·)
    SharedFlow.flush flow
    sub.unsubscribe
    (← list.get).reverse |> pure

instance : Flows MutableSharedFlow IO Id where
  combine := MutableSharedFlow.combine
  subscribe := fun flow action => SharedFlow.subscribe flow.toSharedFlow (action ·)
  flush := (SharedFlow.flush ·.toSharedFlow)
  toList := fun flow => do
    let list ← IO.mkRef ([] : List _)
    -- See SharedFlow instance above for why `Id _` annotation is needed.
    let sub ← SharedFlow.subscribe flow.toSharedFlow fun (a : Id _) => list.modify (a :: ·)
    SharedFlow.flush flow.toSharedFlow
    sub.unsubscribe
    (← list.get).reverse |> pure

end Flow.Core
