import PredictableFlow.Core.Collector
import PredictableCore.Shared.PredictableProgram
import Std

open PredictableCore.Shared

namespace Flow.Core

/-! # Flow Operators and Flows Typeclass

`DerivedFlow` defines a single derivation mechanism for constructing transformed flows.
`Flows` extends `DerivedFlow` and provides `map`, `filter`, `filterMap` with defaults
derived from `derive`, plus `combine`, `subscribe`, `flush`, `forEach`, and `toList`.
-/

/-- Construct a derived flow from a source, given a handler that processes
    each emission and decides what to emit downstream. -/
class DerivedFlow (F : Type → Type) where
  /-- Create a derived flow where each source emission is processed by a handler
      that receives an `emit` callback and the source value. -/
  derive : F α → ((β → IO Unit) → α → IO Unit) → IO (F β)

instance : MonadLiftT (Except ε) Option where
  monadLift
    | .ok a => some a
    | .error _ => none

private def defaultMap [DerivedFlow F] (flow : F α) (f : α → β) : IO (F β) :=
  DerivedFlow.derive flow fun emit a => emit (f a)

private def defaultFilter [DerivedFlow F] (flow : F α) (pred : α → Bool) : IO (F α) :=
  DerivedFlow.derive flow fun emit a => if pred a then emit a else pure ()

private def defaultFilterMap [DerivedFlow F] (flow : F α) (f : α → Option β) : IO (F β) :=
  DerivedFlow.derive flow fun emit a =>
    match f a with
    | some b => emit b
    | none => pure ()

/-- Typeclass for stream types that can be subscribed to.

    Parameterized by:
    - `F` : the flow type constructor
    - `M` : the monad for callbacks and flush (e.g. `IO`, `PredictableProgram ε`)
    - `V` : value wrapper passed to callbacks (e.g. `Id` for plain values, `Except ε` for error-aware)

    `M` and `V` are `outParam`s — inferred automatically from `F`.

    Extends `DerivedFlow F` — instances must provide `derive`, which enables default
    implementations for `map`, `filter`, and `filterMap`.
-/
class Flows
    (f : Type → Type)
    (m : outParam (Type → Type))
    (v : outParam (Type → Type))
    [Monad m]
    [MonadLiftT IO m]
    [MonadLiftT v Option]
    extends DerivedFlow f where
  subscribe : f α → (v α → m Unit) → IO (IO Unit)
  flush : f α → m Unit
  combine : f α → f β → IO (f (α ⊕ β))
  map : f α → (α → β) → IO (f β) := defaultMap
  filter : f α → (α → Bool) → IO (f α) := defaultFilter
  filterMap : f α → (α → Option β) → IO (f β) := defaultFilterMap
  forEach : f α → (v α → m Unit) → m Unit :=
    fun flow f => do
      let unsub ← (subscribe flow f : IO _)
      flush flow
      unsub
  toList : f α → m (List α) :=
    fun flow => do
      let list ← (IO.mkRef ([] : List α) : IO _)
      let unsub ← (subscribe flow fun a => do
        match MonadLiftT.monadLift a with
        | some a' => ((list.modify (a' :: ·) : IO Unit) : IO _)
        | none => pure ())
      flush flow
      unsub
      (← (list.get : IO _)).reverse |> pure



namespace Flows

class Combinable (α : Type) where
  combine : α → α → α

instance : Combinable PredictableConfig where
  combine c _ := c

class MergeableState (α : Type) where
  merge (initial new existing : α) : α
  withoutMergeable (initial : α) : α

instance : MergeableState PredictableState where
  merge initial new existing :=
    let inputDelta := new.inputTokens - initial.inputTokens
    let outputDelta := new.outputTokens - initial.outputTokens
    { existing with
      inputTokens := existing.inputTokens + inputDelta
      outputTokens := existing.outputTokens + outputDelta }
  withoutMergeable existing := existing

/-- Bridge: run a Program action atomically using shared mutexes.

    Three-phase protocol:
    1. **Snapshot**: atomically read current state from the primary mutex
    2. **Execute**: run the program with a clean copy of the snapshot
       (non-mergeable fields stripped via `MergeableState.withoutMergeable`)
    3. **Merge**: atomically combine the program's new state into
       every mutex's current state via `MergeableState.merge`

    This ensures concurrent callbacks each contribute their own state
    changes without overwriting each other's updates. When multiple mutexes
    are provided (e.g. from combined flows), the delta is propagated to all.

    Called automatically by ProgramFlow subscribe
    callbacks for thread-safe state access across concurrent emissions.

    Returns the Except result without unwrapping it. -/
def withStateSync
    [MergeableState σ]
    (mutexes : Array (Std.Mutex σ))
    (config : ψ)
    (program : Program ψ σ ε α)
    : IO (Except ε α) := do
  if h : 0 < mutexes.size then
    let mut initialStates : Array σ := #[]
    for mutex in mutexes do
      let state ← mutex.atomically do return ← get
      initialStates := initialStates.push state

    -- Logically always true (loop pushes one per mutex), but Lean needs the
    -- proof term h' for the initialStates[0] index below.
    if h' : 0 < initialStates.size then
      let (result, newState) ←
        Program.run program config (MergeableState.withoutMergeable initialStates[0])

      for (idx, mutex) in mutexes.mapIdx (·,·) do
        if h'' : idx < initialStates.size then
          mutex.atomically do
            let existingState ← get
            set <| MergeableState.merge initialStates[idx] newState existingState

      pure result
    else
      throw (.userError "withStateSync: empty mutex array")
  else
    throw (.userError "withStateSync: empty mutex array")

end Flows

end Flow.Core
