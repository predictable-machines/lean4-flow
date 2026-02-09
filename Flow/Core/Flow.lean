import PredictableFlow.Core.Collector
import PredictableFlow.Core.Flows

namespace Flow.Core

/-! # Flow - Cold, lazy, unicast streams

A Flow is a cold stream that emits values sequentially when collected.
Each collection starts fresh execution - multiple collectors get independent streams.

## Why `unsafe`?

Flow is marked `unsafe` because it contains a self-referential function type.
The constructor stores `(Collector α → IO Unit)` which may reference Flow recursively
in builder patterns. This follows the same pattern as IOStream.

All operations are standard safe IO operations. The `unsafe` marker is a Lean
type system constraint for recursive structures, not a runtime safety concern.

## Comparison to Kotlin Flow

```kotlin
// Kotlin
val flow = flow {
    emit(1)
    emit(2)
    emit(3)
}
flow.collect { println(it) }
```

```lean
-- Lean
let flow := Flow.mk fun collector => do
    collector.emit 1
    collector.emit 2
    collector.emit 3
flow.collect IO.println
```
-/

/-- Cold stream that emits values when collected.
    Each collector gets independent execution. -/
inductive Flow (α : Type) where
  | mk : (Collector α → IO Unit) → Flow α

namespace Flow

/-- Collect values from a flow with an action.
    This is a terminal operation that triggers flow execution. -/
def forEach (flow : Flow α) (action : α → IO Unit) : IO Unit :=
  match flow with
  | .mk block => block (Collector.fromAction action)

/-- Subscribe to a flow with an action. Returns a cancellation function.

    For cold flows, the cancellation is a no-op since collection runs
    synchronously to completion. This signature enables uniform handling
    with hot streams like SharedFlow.
-/
def collect (flow : Flow α) (action : α → IO Unit) : IO (IO Unit) := do
  flow.forEach action
  pure (pure ())

/-- Collect with an explicit collector -/
def collectWith (flow : Flow α) (collector : Collector α) : IO Unit :=
  match flow with
  | .mk block => block collector

/-- Transform each emitted value with a function.

    Creates a new Flow that applies the transformation during collection.

    Example:
    ```lean
    let flow := Flow.mk fun c => do c.emit 1; c.emit 2; c.emit 3
    let doubled ← flow.map (· * 2)
    doubled.forEach IO.println  -- prints 2, 4, 6
    ```
-/
def map (flow : Flow α) (f : α → β) : IO (Flow β) :=
  pure <| Flow.mk fun collector =>
    flow.collectWith { emit := fun a => collector.emit (f a) }

/-- Flush is a no-op for cold flows since collection is synchronous. -/
def flush (_ : Flow α) : IO Unit := pure ()

/-- Filter values that satisfy a predicate.

    Creates a new Flow that only emits values passing the predicate.

    Example:
    ```lean
    let flow := Flow.mk fun c => do c.emit 1; c.emit 2; c.emit 3; c.emit 4
    let evens ← flow.filter (· % 2 == 0)
    evens.forEach IO.println  -- prints 2, 4
    ```
-/
def filter (flow : Flow α) (pred : α → Bool) : IO (Flow α) :=
  pure <| Flow.mk fun collector =>
    flow.collectWith { emit := fun a => if pred a then collector.emit a else pure () }

/-- Choose and transform values using an optional function.

    Creates a new Flow that applies the chooser function to each value.
    Values where the function returns `some` are emitted (transformed),
    values where it returns `none` are filtered out.

    Example:
    ```lean
    let flow := Flow.mk fun c => do c.emit 1; c.emit 2; c.emit 3; c.emit 4
    let chosen ← flow.choose fun n => if n % 2 == 0 then some (n * 10) else none
    chosen.forEach IO.println  -- prints 20, 40
    ```
-/
def choose (flow : Flow α) (f : α → Option β) : IO (Flow β) :=
  pure <| Flow.mk fun collector =>
    flow.collectWith { emit := fun a => do
      match f a with
      | some b => collector.emit b
      | none => pure ()
    }

def combine (flow1 : Flow α) (flow2 : Flow β) : IO (Flow (Sum α β)) :=
  pure <| Flow.mk fun collector => do
    flow1.collectWith { emit := fun a => collector.emit (Sum.inl a) }
    flow2.collectWith { emit := fun b => collector.emit (Sum.inr b) }

end Flow

/-- Flow instance of Flows typeclass -/
instance : Flows Flow where
  collect := Flow.collect
  map := Flow.map
  flush := Flow.flush
  filter := Flow.filter
  combine := Flow.combine

/-- Flow instance of Chooses typeclass -/
instance : Chooses Flow where
  choose := Flow.choose

end Flow.Core
