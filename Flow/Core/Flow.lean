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
flow.subscribe IO.println
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
def subscribe (flow : Flow α) (action : α → IO Unit) : IO (IO Unit) := do
  flow.forEach action
  pure (pure ())

/-- Collect with an explicit collector -/
def collectWith (flow : Flow α) (collector : Collector α) : IO Unit :=
  match flow with
  | .mk block => block collector

/-- Flush is a no-op for cold flows since collection is synchronous. -/
def flush (_ : Flow α) : IO Unit := pure ()

def combine (flow1 : Flow α) (flow2 : Flow β) : IO (Flow (Sum α β)) :=
  pure <| Flow.mk fun collector => do
    flow1.collectWith { emit := fun a => collector.emit (Sum.inl a) }
    flow2.collectWith { emit := fun b => collector.emit (Sum.inr b) }

end Flow

instance : DerivedFlow Flow where
  derive source handler := pure <| Flow.mk fun collector =>
    source.collectWith { emit := handler collector.emit }

instance : Flows Flow IO Id where
  combine := Flow.combine
  subscribe := Flow.subscribe
  flush := Flow.flush

/-- BEq instance for Id: since Id is the identity, delegate to inner type -/
instance [inst : BEq α] : BEq (Id α) where
  beq x y := inst.beq (x : α) (y : α)

/-- Repr instance for Id: since Id is the identity, delegate to inner type -/
instance [Repr α] : Repr (Id α) where
  reprPrec x p := reprPrec (x : α) p

end Flow.Core
