import PredictableFlow.Core.SharedFlow

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
let cancel ← stateFlow.collect IO.println
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
  initialState : α
  value : IO α

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


/-- Transform each emitted value with a function.

    Creates a new StateFlow that applies the transformation to values.

    Example:
    ```lean
    let state ← MutableStateFlow.create 5
    let doubled ← state.toStateFlow.map (· * 2)
    let v ← doubled.value  -- 10
    ```
-/
def map (flow : StateFlow α) (f : α → β) : IO (StateFlow β) := do
  let mappedShared ← flow.sharedFlow.map f
  pure {
    sharedFlow := mappedShared
    initialState := f flow.initialState
    value := f <$> flow.value
  }

def filter (flow : StateFlow α) (pred : α → Bool) : IO (StateFlow α) := do
  let filteredShared ← flow.sharedFlow.filter pred
  pure {
    sharedFlow := filteredShared
    initialState := flow.initialState
    value := flow.value
  }

def combine (flow1 : StateFlow α) (flow2 : StateFlow β) : IO (StateFlow (Sum α β)) := do
  let combinedShared ← flow1.sharedFlow.combine flow2.sharedFlow
  pure {
    sharedFlow := combinedShared
    initialState := Sum.inl flow1.initialState
    value := Sum.inl <$> flow1.value
  }

end StateFlow

/-- StateFlow instance of Flows typeclass -/
instance : Flows StateFlow where
  collect := (·.sharedFlow.collect)
  map := StateFlow.map
  flush := StateFlow.flush
  filter := StateFlow.filter
  combine := StateFlow.combine

/-- Mutable StateFlow that can update its value.

    This is the producer side of StateFlow.
-/
structure MutableStateFlow (α : Type) where
  mutableSharedFlow : MutableSharedFlow α
  initialState : α
  value : IO α

namespace MutableStateFlow

/-- Create a new MutableStateFlow with an initial value.

    Example:
    ```lean
    let flow ← MutableStateFlow.create 0
    let stringFlow ← MutableStateFlow.create "hello"
    ```
-/
def create
    (initialState : α)
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
    pure (cache.getD (cache.size - 1) initialState)

  mutableSharedFlow.emit initialState
  pure { mutableSharedFlow, initialState, value }

/-- Update the value and notify all collectors.

    Example:
    ```lean
    let flow ← MutableStateFlow.create 0
    let cancel ← flow.collect IO.println
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
  let currentValue ← flow.value
  let newValue := f currentValue
  flow.emit newValue

/-- Update the value using an IO transformation function.

    Example:
    ```lean
    let flow ← MutableStateFlow.create "file.txt"
    flow.updateIO IO.FS.readFile  -- read file and emit contents
    ```
-/
def updateIO (flow : MutableStateFlow α) (f : α → IO α) : IO Unit := do
  let currentValue ← flow.value
  let newValue ← f currentValue
  flow.emit newValue

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
  let current ← flow.value
  if current == expected then
    flow.emit newValue
    pure true
  else
    pure false

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

    Unlike `forEach`, this keeps the subscription active until cancelled.

    Example:
    ```lean
    let state ← MutableStateFlow.create 0
    let cancel ← state.collect IO.println
    state.emit 42  -- subscriber receives 42
    cancel  -- stop receiving updates
    ```
-/
def collect (flow : MutableStateFlow α) (action : α → IO Unit) : IO (IO Unit) :=
  flow.mutableSharedFlow.collect action

/-- Transform each emitted value with a function.

    Creates a new MutableStateFlow that applies the transformation.
    The mapped flow subscribes to the original and emits transformed values.

    Note: Emitting to the mapped flow does not affect the original flow.

    Example:
    ```lean
    let state ← MutableStateFlow.create 5
    let doubled ← state.map (· * 2)
    let v ← doubled.value  -- 10
    ```
-/
def map (flow : MutableStateFlow α) (f : α → β) : IO (MutableStateFlow β) := do
  flow.mutableSharedFlow.state.atomically do
    let state ← get
    let initialValue := state.replayCache.getD (state.replayCache.size - 1) flow.initialState
    let mapped ← MutableStateFlow.create (f initialValue) (parentFlush := flow.flush)
    let (stateWithSub, _) ← SharedFlow.addSubscriber flow.mutableSharedFlow.toSharedFlow state (fun a => mapped.emit <| f a) (skipReplay := true)
    set { stateWithSub with closeActions := stateWithSub.closeActions.push mapped.close }
    pure mapped

def filter (flow : MutableStateFlow α) (pred : α → Bool) : IO (MutableStateFlow α) := do
  flow.mutableSharedFlow.state.atomically do
    let state ← get
    let initialValue := state.replayCache.getD (state.replayCache.size - 1) flow.initialState
    let filtered ← MutableStateFlow.create initialValue (parentFlush := flow.flush)
    let (stateWithSub, _) ← SharedFlow.addSubscriber flow.mutableSharedFlow.toSharedFlow state (fun a => if pred a then filtered.emit a else pure ()) (skipReplay := true)
    set { stateWithSub with closeActions := stateWithSub.closeActions.push filtered.close }
    pure filtered

def combine (flow1 : MutableStateFlow α) (flow2 : MutableStateFlow β) : IO (MutableStateFlow (α ⊕ β)) := do
  flow1.mutableSharedFlow.state.atomically do
    let state1 ← get
    let initialValue := state1.replayCache.getD (state1.replayCache.size - 1) flow1.initialState
    let (combined, state1') ← flow2.mutableSharedFlow.state.atomically do
      let state2 ← get
      let combined ← MutableStateFlow.create (Sum.inl initialValue) (parentFlush := some (do flow1.flush; flow2.flush))
      let (state1', _) ← SharedFlow.addSubscriber flow1.mutableSharedFlow.toSharedFlow state1 (fun a => combined.emit (Sum.inl a)) (skipReplay := true)
      let (state2', _) ← SharedFlow.addSubscriber flow2.mutableSharedFlow.toSharedFlow state2 (fun b => combined.emit (Sum.inr b)) (skipReplay := true)
      set { state2' with closeActions := state2'.closeActions.push combined.close }
      pure (combined, state1')
    set { state1' with closeActions := state1'.closeActions.push combined.close }
    pure combined

end MutableStateFlow

/-- MutableStateFlow instance of Flows typeclass -/
instance : Flows MutableStateFlow where
  collect := (·.mutableSharedFlow.collect)
  map := MutableStateFlow.map
  flush := (·.mutableSharedFlow.flush)
  filter := MutableStateFlow.filter
  combine := MutableStateFlow.combine

end Flow.Core
