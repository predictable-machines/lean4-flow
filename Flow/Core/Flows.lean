import PredictableFlow.Core.Collector
import PredictableCore.Shared.PredictableProgram
import Std

open PredictableCore.Shared

namespace Flow.Core

/-! # Flow Operators and Flows Typeclass

`DerivedFlow` defines a single derivation mechanism for constructing transformed flows.
`Flows` extends `DerivedFlow` and provides `map`, `filter`, `choose` with defaults
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

private def defaultChoose [DerivedFlow F] (flow : F α) (f : α → Option β) : IO (F β) :=
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
    implementations for `map`, `filter`, and `choose`.
-/
class Flows
    (F : Type → Type)
    (M : outParam (Type → Type))
    (V : outParam (Type → Type))
    [Monad M]
    [MonadLiftT IO M]
    [MonadLiftT V Option]
    extends DerivedFlow F where
  subscribe : F α → (V α → M Unit) → IO (IO Unit)
  flush : F α → M Unit
  combine : F α → F β → IO (F (α ⊕ β))
  map : F α → (α → β) → IO (F β) := defaultMap
  filter : F α → (α → Bool) → IO (F α) := defaultFilter
  choose : F α → (α → Option β) → IO (F β) := defaultChoose
  forEach : F α → (V α → M Unit) → M Unit :=
    fun flow f => do
      let unsub ← (subscribe flow f : IO _)
      flush flow
      unsub
  toList : F α → M (List α) :=
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

/-- Bridge: run a PredictableProgram action atomically using a shared mutex.

    Three-phase protocol:
    1. **Snapshot**: atomically read current state from mutex
    2. **Execute**: run the program with the snapshot (logs cleared)
    3. **Merge**: atomically compute token deltas (new − initial),
       add them to the current mutex state, and append logs

    This ensures concurrent callbacks each contribute their own token
    usage without overwriting each other's updates.

    Called automatically by ProgramFlow/MutableProgramFlow subscribe
    callbacks for thread-safe state access across concurrent emissions.

    Returns the Except result without unwrapping it. -/
def withStateSync
    (mutex : Std.Mutex PredictableState)
    (config : PredictableConfig)
    (program : PredictableProgram ε α)
    : IO (Except ε α) := do
  let initialState ← mutex.atomically do return ← get
  let (result, newState) ←
    PredictableProgram.run program config { initialState with logs := [] }

  mutex.atomically do
    let existingState ← get
    let inputDelta := newState.inputTokens - initialState.inputTokens
    let outputDelta := newState.outputTokens - initialState.outputTokens

    set
      { existingState with
        inputTokens := existingState.inputTokens + inputDelta
        outputTokens := existingState.outputTokens + outputDelta
        logs := existingState.logs ++ newState.logs }
    pure result

end Flows


end Flow.Core
