import Flow.Core.Collector
import Flow.Core.Flows

namespace Flow.Core

/-! # Flow - Cold, lazy, unicast streams

A Flow is a cold stream that emits values sequentially when collected.
Each collection starts fresh execution - multiple collectors get independent streams.

Flow is a regular `inductive` type whose single constructor stores
`(Collector α → IO Unit)`. The function does not reference `Flow` recursively
at the type level, so no `unsafe` marker is needed.

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
def collect (flow : Flow α) (action : α → IO Unit) : IO Unit :=
  match flow with
  | .mk block => block (Collector.fromAction action)

/-- Subscribe to a flow with an action. Returns a Subscription handle.

    For cold flows, unsubscribe and waitForCompletion are no-ops since
    collection runs synchronously to completion. This signature enables
    uniform handling with hot streams like SharedFlow.
-/
def subscribe (flow : Flow α) (action : α → IO Unit) : IO Subscription := do
  flow.collect action
  pure { unsubscribe := pure (), waitForCompletion := pure () }

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

/-- Fold all emissions into an accumulator. -/
def fold (flow : Flow α) (f : β → α → β) (init : β) : IO β := do
  let acc ← IO.mkRef init
  flow.collect fun a => acc.modify (f · a)
  acc.get

/-- Find the first emission matching a predicate.
    Does not short-circuit the source; remaining emissions are ignored. -/
def find (flow : Flow α) (pred : α → Bool) : IO (Option α) := do
  let result ← IO.mkRef (none : Option α)
  flow.collect fun a => do
    if (← result.get).isNone && pred a then
      result.set (some a)
  result.get

/-- Check if any emission matches a predicate. -/
def any (flow : Flow α) (pred : α → Bool) : IO Bool := do
  (← flow.find pred).isSome |> pure

/-- Check if all emissions match a predicate.
    Returns `true` for an empty flow. -/
def all (flow : Flow α) (pred : α → Bool) : IO Bool := do
  let failed ← IO.mkRef false
  flow.collect fun a => do
    if !(← failed.get) && !pred a then
      failed.set true
  (← failed.get) |> (!·) |> pure

/-- Count the number of emissions. -/
def count (flow : Flow α) : IO Nat :=
  flow.fold (fun n _ => n + 1) 0

/-- Emit only the first `n` values.
    Does not short-circuit the source; remaining emissions are ignored. -/
def take (flow : Flow α) (n : Nat) : Flow α :=
  Flow.mk fun collector => do
    let counter ← IO.mkRef 0
    flow.collect fun a => do
      let c ← counter.get
      if c < n then
        collector.emit a
        counter.set (c + 1)

/-- Append two flows sequentially. The second flow emits after the first completes. -/
def append (flow1 : Flow α) (flow2 : Flow α) : Flow α :=
  Flow.mk fun collector => do
    flow1.collectWith collector
    flow2.collectWith collector

instance : Append (Flow α) where
  append := Flow.append

end Flow

instance : DerivedFlow Flow where
  derive source handler := pure <| Flow.mk fun collector =>
    source.collectWith { emit := handler collector.emit }

instance : Flows Flow IO Id where
  combine := Flow.combine
  subscribe := fun flow action => Flow.subscribe flow (action ·)
  flush := Flow.flush

/-- BEq instance for Id: since Id is the identity, delegate to inner type -/
instance [inst : BEq α] : BEq (Id α) where
  beq x y := inst.beq (x : α) (y : α)

/-- Repr instance for Id: since Id is the identity, delegate to inner type -/
instance [Repr α] : Repr (Id α) where
  reprPrec x p := reprPrec (x : α) p

end Flow.Core
