import Flow.Core.Collector
import Flow.Internal.Program
import Std

open Flow.Internal

namespace Flow.Core

/-! # Flow Operators and Flows Typeclass

`DerivedFlow` defines a single derivation mechanism for constructing transformed flows.
`Flows` extends `DerivedFlow` and provides `map`, `filter`, `filterMap` with defaults
derived from `derive`, plus `combine`, `subscribe`, `flush`, and `toList`.
-/

/-- Construct a derived flow from a source, given a handler that processes
    each emission and decides what to emit downstream. -/
class DerivedFlow (f : Type → Type) where
  /-- Create a derived flow where each source emission is processed by a handler
      that receives an `emit` callback and the source value. -/
  derive : f α → ((β → IO Unit) → α → IO Unit) → IO (f β)

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

/-- Subscription handle returned by `subscribe`.
    Provides named fields for unsubscribing and waiting for the subscriber to complete. -/
structure Subscription (σ : Type) where
  unsubscribe : IO Unit
  waitForCompletion : IO σ

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
  /-- Register a callback for each emission. Returns a Subscription handle. -/
  subscribe : f α → (v α → m Unit) → IO (Subscription Unit)
  /-- Drive the flow to completion, blocking until all items are emitted. -/
  flush : f α → m Unit
  /-- Merge two flows into one, tagging emissions with `Sum.inl` / `Sum.inr`. -/
  combine : f α → f β → IO (f (α ⊕ β))
  /-- Transform each emission. Defaults to `derive` with a mapping handler. -/
  map : f α → (α → β) → IO (f β) := defaultMap
  /-- Keep only emissions satisfying a predicate. Defaults to `derive`. -/
  filter : f α → (α → Bool) → IO (f α) := defaultFilter
  /-- Transform and filter in one pass: emit only `some` results. -/
  filterMap : f α → (α → Option β) → IO (f β) := defaultFilterMap
  /-- Collect all emissions into a list (subscribe, flush, unsubscribe). -/
  toList : f α → m (List α) :=
    fun flow => do
      let list ← (IO.mkRef ([] : List α) : IO _)
      let sub ← (subscribe flow fun a => do
        match MonadLiftT.monadLift a with
        | some a' => ((list.modify (a' :: ·) : IO Unit) : IO _)
        | none => pure ())
      flush flow
      sub.unsubscribe
      (← (list.get : IO _)).reverse |> pure

/-- Subscribe interface with support for a value wrapper `v` (e.g. `Id` or `Except ε`).
    Allows subscribers to handle values and errors through `v α`. -/
class IOSubscribable (f : Type → Type) (v : outParam (Type → Type)) where
  subscribe : f α → (v α → IO Unit) → IO (Subscription Unit)

/-- Any `Flows` instance is automatically `IOSubscribable`. -/
instance
    [Monad m]
    [MonadLiftT IO m]
    [MonadLiftT v Option]
    [Flows f m v]
    : IOSubscribable f v where
  subscribe flow callback := do
    let sub ← Flows.subscribe flow fun va => MonadLiftT.monadLift (callback va)
    pure { unsubscribe := sub.unsubscribe, waitForCompletion := sub.waitForCompletion }

/-- Type-erased subscription handle so heterogeneous source types can share a list.
    `v` comes first so `IOSubscription v` is a proper `Type → Type` for typeclass resolution. -/
structure IOSubscription (v : Type → Type) (α : Type) where
  subscribe : (v α → IO Unit) → IO (Subscription Unit)

instance : IOSubscribable (IOSubscription v) v where
  subscribe sub callback := sub.subscribe callback

/-- Identity adapter that constrains `v` to `Except ε` for the macro's bare-source case. -/
def IOSubscription.withExcept (sub : IOSubscription (Except ε) α) : IOSubscription (Except ε) α := sub

/-- Build a type-erased `IOSubscription` by applying a partial transform to a source.
    Emissions where `transform` returns `none` are silently dropped. -/
def IOSubscribable.mapped
    [IOSubscribable f v]
    (source : f β)
    (transform : v β → Option (w α))
    : IOSubscription w α :=
  { subscribe := fun callback => do
      let sub ← IOSubscribable.subscribe source fun vb =>
        match transform vb with
        | some a => callback a
        | none => pure ()
      pure { unsubscribe := sub.unsubscribe, waitForCompletion := sub.waitForCompletion } }

namespace Flows

/-- Types whose values can be combined into one (e.g. config merging). -/
class Combinable (α : Type) where
  combine : α → α → α

/-- Three-way merge for concurrent state updates.
    `merge initial new existing` computes the delta `new - initial` and applies it to `existing`,
    so independent mutations from different callbacks compose correctly. -/
class MergeableState (α : Type) where
  merge (initial new existing : α) : α
  /-- Strip fields that participate in merging, giving a clean baseline for execution. -/
  withoutMergeable (initial : α) : α

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
