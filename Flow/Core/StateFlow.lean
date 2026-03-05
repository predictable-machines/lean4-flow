import Flow.Core.SharedFlow
import Flow.Core.Flows

namespace Flow.Core

/-! # StateFlow - Hot, stateful streams

A StateFlow is a hot, stateful stream that always holds a current value.
It's implemented as a specialized SharedFlow with replay=1 and additional
value access capabilities.

Key differences from SharedFlow:

1. **Always has a value**: Must be initialized with a value
2. **Conflated**: New emissions replace the current value
3. **No replay parameter**: Fixed at 1 (new subscribers get current value)
4. **Read access**: Can read current value without subscribing

## Comparison to Kotlin StateFlow

```kotlin
// Kotlin
val stateFlow = MutableStateFlow(0)
println(stateFlow.value)  // 0
stateFlow.value = 42
stateFlow.collect { println(it) }
```

```lean
-- Lean
let stateFlow ← MutableStateFlow.create 0
let v ← stateFlow.value  -- 0
stateFlow.emit 42
let cancel ← stateFlow.subscribe IO.println
```

## Key Properties

- **Conflation**: Only the most recent value matters
- **Immediate value**: New subscribers get current value immediately
- **Thread-safe**: Value updates are atomic via IO.Ref
- **No buffer**: StateFlow doesn't buffer, just holds one value
-/

/-- Hot stream that holds a current value and emits updates.

    Implemented as a wrapper around SharedFlow with replay=1.
    Use `MutableStateFlow.create` to create instances.
-/
structure StateFlow (α : Type) where
  sharedFlow : SharedFlow α
  initialState : Option α
  value : IO (Option α)

namespace StateFlow


/-- Get the number of active subscribers -/
def subscriberCount (flow : StateFlow α) : IO Nat :=
  flow.sharedFlow.subscriberCount

/-- Check if the flow is closed -/
def isClosed (flow : StateFlow α) : IO Bool :=
  flow.sharedFlow.isClosed

/-- Wait until all subscribers have finished processing -/
def flush (flow : StateFlow α) : IO Unit :=
  flow.sharedFlow.flush


def combine (flow1 : StateFlow α) (flow2 : StateFlow β) : IO (StateFlow (Sum α β)) := do
  let combinedShared ← flow1.sharedFlow.combine flow2.sharedFlow
  pure
    { sharedFlow := combinedShared
      initialState := flow1.initialState.map Sum.inl
      value := (·.map Sum.inl) <$> flow1.value }

end StateFlow

instance : DerivedFlow StateFlow where
  derive flow handler := do
    let derivedShared ← DerivedFlow.derive flow.sharedFlow handler
    pure
      { sharedFlow := derivedShared,
        initialState := none,
        value := do
          let cache ← derivedShared.replayCache
          pure cache[cache.size - 1]? }

instance : Flows StateFlow IO Id where
  combine := StateFlow.combine
  subscribe := fun flow action => SharedFlow.subscribe flow.sharedFlow (action ·)
  flush := StateFlow.flush
  toList := fun flow => do
    let list ← IO.mkRef ([] : List _)
    -- `Id` is @[reducible] so Lean reduces `a : Id α` to `α` in the callback,
    -- but the list ref keeps type `List (Id α)` from the return type, causing an
    -- HAppend mismatch. Annotating `a : Id _` preserves the wrapper so both sides match.
    let sub ← flow.sharedFlow.subscribe fun (a : Id _) => list.modify (a :: ·)
    StateFlow.flush flow
    sub.unsubscribe
    (← list.get).reverse |> pure

/-- Mutable StateFlow that can update its value.

    This is the producer side of StateFlow.
-/
structure MutableStateFlow (α : Type) where
  mutableSharedFlow : MutableSharedFlow α
  initialState : Option α
  value : IO (Option α)

namespace MutableStateFlow

/-- Create a new MutableStateFlow, optionally with an initial value.

    When `initial` is `some v`, the flow starts with value `v` and subscribers
    receive it immediately. When `none`, the flow starts empty and `value`
    returns `none` until the first emission.

    Example:
    ```lean
    let flow ← MutableStateFlow.create 0
    let stringFlow ← MutableStateFlow.create "hello"
    let empty ← MutableStateFlow.create (α := Nat)
    ```
-/
def create
    (initial : Option α := none)
    (parentFlush : Option (IO Unit) := none)
    : IO (MutableStateFlow α) := do
  let mutableSharedFlow ←
    MutableSharedFlow.create
      (α := α)
      (replay := 1)
      (bufferSize := 1)
      (parentFlush := parentFlush)

  let value := do
    let cache ← mutableSharedFlow.toSharedFlow.replayCache
    pure cache[cache.size - 1]?

  if let some v := initial then
    mutableSharedFlow.emit v
  pure { mutableSharedFlow, initialState := initial, value }

/-- Update the value and notify all subscribers.

    Example:
    ```lean
    let flow ← MutableStateFlow.create 0
    let cancel ← flow.subscribe IO.println
    flow.emit 42  -- prints "42"
    ```
-/
def emit (flow : MutableStateFlow α) (newValue : α) : IO Unit := do
  flow.mutableSharedFlow.emit newValue

/-- Update the value using a transformation function.

    Example:
    ```lean
    let flow ← MutableStateFlow.create 0
    flow.update (· + 1)  -- increment by 1
    let v ← flow.value  -- 1
    ```
-/
def update (flow : MutableStateFlow α) (f : α → α) : IO Unit := do
  match ← flow.value with
  | some currentValue => flow.emit (f currentValue)
  | none => pure ()

/-- Update the value using an IO transformation function.

    Example:
    ```lean
    let flow ← MutableStateFlow.create "file.txt"
    flow.updateIO IO.FS.readFile  -- read file and emit contents
    ```
-/
def updateIO (flow : MutableStateFlow α) (f : α → IO α) : IO Unit := do
  match ← flow.value with
  | some currentValue =>
    let newValue ← f currentValue
    flow.emit newValue
  | none => pure ()

/-- Close the flow, cancelling all subscribers and preventing new emissions and subscriptions. -/
def close (flow : MutableStateFlow α) : IO Unit :=
  flow.mutableSharedFlow.close

/-- Atomically update the value only if it matches the expected value.
    Returns true if the update was successful, false otherwise.

    Example:
    ```lean
    let flow ← MutableStateFlow.create 0
    let success ← flow.compareAndSet 0 42  -- true, value is now 42
    let fail ← flow.compareAndSet 0 100    -- false, value is still 42
    ```
-/
def compareAndSet [BEq α] (flow : MutableStateFlow α) (expected : α) (newValue : α) : IO Bool := do
  match ← flow.value with
  | some current =>
    if current == expected then
      flow.emit newValue
      pure true
    else
      pure false
  | none => pure false

/-- Get the number of active subscribers -/
def subscriberCount (flow : MutableStateFlow α) : IO Nat :=
  flow.mutableSharedFlow.toSharedFlow.subscriberCount

/-- Check if the flow is closed -/
def isClosed (flow : MutableStateFlow α) : IO Bool :=
  flow.mutableSharedFlow.toSharedFlow.isClosed

def toStateFlow (flow : MutableStateFlow α) : StateFlow α :=
  { sharedFlow := flow.mutableSharedFlow.toSharedFlow
    initialState := flow.initialState
    value := flow.value }

/-- Wait until all subscribers have finished processing -/
def flush (flow : MutableStateFlow α) : IO Unit :=
  flow.toStateFlow.flush

/-- Subscribe to value updates. Returns a cancellation function.

    Example:
    ```lean
    let state ← MutableStateFlow.create 0
    let sub ← state.subscribe IO.println
    state.emit 42  -- subscriber receives 42
    sub.unsubscribe  -- stop receiving updates
    ```
-/
def subscribe (flow : MutableStateFlow α) (action : α → IO Unit) : IO Subscription :=
  flow.mutableSharedFlow.subscribe action

def filter (flow : MutableStateFlow α) (pred : α → Bool) : IO (MutableStateFlow α) := do
  flow.mutableSharedFlow.state.atomically do
    let state ← get
    let currentValue := state.replayCache[state.replayCache.size - 1]? <|> flow.initialState
    let filteredInitial := currentValue.filter pred
    let filtered ← MutableStateFlow.create filteredInitial (parentFlush := flow.flush)
    let (stateWithSub, _) ← SharedFlow.addSubscriber flow.mutableSharedFlow.toSharedFlow state (fun a => if pred a then filtered.emit a else pure ()) (skipReplay := true)
    set { stateWithSub with closeActions := stateWithSub.closeActions.push filtered.close }
    pure filtered

def combine (flow1 : MutableStateFlow α) (flow2 : MutableStateFlow β) : IO (MutableStateFlow (α ⊕ β)) := do
  flow1.mutableSharedFlow.state.atomically do
    let state1 ← get
    let initialValue := (state1.replayCache[state1.replayCache.size - 1]? <|> flow1.initialState).map Sum.inl
    let (combined, state1') ← flow2.mutableSharedFlow.state.atomically do
      let state2 ← get
      let combined ← MutableStateFlow.create initialValue (parentFlush := some (do flow1.flush; flow2.flush))
      let (state1', _) ← SharedFlow.addSubscriber flow1.mutableSharedFlow.toSharedFlow state1 (fun a => combined.emit (Sum.inl a)) (skipReplay := true)
      let (state2', _) ← SharedFlow.addSubscriber flow2.mutableSharedFlow.toSharedFlow state2 (fun b => combined.emit (Sum.inr b)) (skipReplay := true)
      set { state2' with closeActions := state2'.closeActions.push combined.close }
      pure (combined, state1')
    set { state1' with closeActions := state1'.closeActions.push combined.close }
    pure combined

end MutableStateFlow

instance : DerivedFlow MutableStateFlow where
  derive flow handler := do
    flow.mutableSharedFlow.state.atomically do
      let state ← get
      let derivedInitial ← do
        match state.replayCache[state.replayCache.size - 1]? <|> flow.initialState with
        | some currentValue =>
          let ref ← IO.mkRef none
          handler (fun b => ref.set (some b)) currentValue
          ref.get
        | none => pure none
      let derived ← MutableStateFlow.create derivedInitial (parentFlush := flow.flush)
      let (s, _) ← SharedFlow.addSubscriber flow.mutableSharedFlow.toSharedFlow state
        (handler derived.emit) (skipReplay := true)
      set { s with closeActions := s.closeActions.push derived.close }
      pure derived

instance : Flows MutableStateFlow IO Id where
  filter := MutableStateFlow.filter
  combine := MutableStateFlow.combine
  subscribe := fun flow action => SharedFlow.subscribe flow.mutableSharedFlow.toSharedFlow (action ·)
  flush := (·.mutableSharedFlow.flush)
  toList := fun flow => do
    let list ← IO.mkRef ([] : List _)
    -- See StateFlow instance above for why `Id _` annotation is needed.
    let sub ← SharedFlow.subscribe flow.mutableSharedFlow.toSharedFlow fun (a : Id _) => list.modify (a :: ·)
    flow.mutableSharedFlow.flush
    sub.unsubscribe
    (← list.get).reverse |> pure

end Flow.Core
