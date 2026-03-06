import Flow.Core.SharedFlow
import Flow.Core.Flows
import Flow.Internal.Program
import Std

namespace Flow.Core

open Flow.Internal

/-! # ProgramFlow - Hot, multicast streams in PredictableProgram context

A ProgramFlow wraps a `MutableSharedFlow (Except ε α)` with shared
`Std.Mutex σ` instances so that collection callbacks can run in the
`Program ψ σ ε` monad with thread-safe state access.

Emissions broadcast to all subscribers (hot semantics). Mutexes are shared across
mapped/filtered children so all operations in a pipeline share the same state.
When flows are combined, both mutex sets are preserved so state deltas propagate
to all originating mutexes.

Type parameters:
- `ψ`: config type
- `σ`: state type (requires MergeableState instance)
- `ε`: error type
- `α`: value type
-/

/-- Subscription handle for ProgramFlow that also provides access to the current state. -/
structure ProgramFlowSubscription (σ : Type) extends Subscription where
  currentState : IO σ

/-- Hot, multicast stream backed by a `MutableSharedFlow` with shared state mutexes.

    `stateMutexes` is a non-empty subtype so callers can index element 0 without
    bounds checks, and combined flows carry mutexes from both sources so state
    deltas propagate to every originating mutex. -/
structure ProgramFlow (ψ σ ε α : Type) where
  underlying : MutableSharedFlow (Except ε α)
  stateMutexes : { arr : Array (Std.Mutex σ) // 0 < arr.size }
  config : ψ

instance
  [Flows.MergeableState σ]
  : DerivedFlow (fun α => ProgramFlow ψ σ ε α)
where
  derive flow handler := do
    let derived ← DerivedFlow.derive flow.underlying fun emit exceptVal =>
      match exceptVal with
      | .ok a => handler (fun b => emit (Except.ok b)) a
      | .error e => emit (Except.error e)
    pure { underlying := derived, stateMutexes := flow.stateMutexes, config := flow.config }

namespace ProgramFlow

/-- Create a new ProgramFlow, initialising the state mutex from the current Program state. -/
def create
    (replay : Nat := 0)
    (bufferSize : Nat := 64)
    (onBufferOverflow : BufferOverflow := .dropOldest)
    : Program ψ σ ε (ProgramFlow ψ σ ε α) := do
  let state ← get
  let config ← read
  let underlying ← MutableSharedFlow.create
    (α := Except ε α)
    (replay := replay)
    (bufferSize := bufferSize)
    (onBufferOverflow := onBufferOverflow)
  let stateMutex ← Std.Mutex.new state
  pure { underlying, stateMutexes := ⟨#[stateMutex], by simp⟩, config }

/-- Lift an existing `MutableSharedFlow` into a `ProgramFlow`, wrapping each value in `Except.ok`. -/
def liftShared
    (shared : MutableSharedFlow α)
    : Program ψ σ ε (ProgramFlow ψ σ ε α) := do
  let state ← get
  let config ← read
  let stateMutex ← Std.Mutex.new state
  let underlying ← Flows.map shared (Except.ok ·)
  pure { underlying, stateMutexes := ⟨#[stateMutex], by simp⟩, config }


def emit (flow : ProgramFlow ψ σ ε α) (value : α) : IO Unit :=
  flow.underlying.emit (.ok value)

def emitError (flow : ProgramFlow ψ σ ε α) (error : ε) : IO Unit :=
  flow.underlying.emit (.error error)

def emitExcept (flow : ProgramFlow ψ σ ε α) (value : Except ε α) : IO Unit :=
  flow.underlying.emit value

def emitAll (flow : ProgramFlow ψ σ ε α) (values : List α) : IO Unit := do
  for value in values do
    flow.emit value

def close (flow : ProgramFlow ψ σ ε α) : IO Unit :=
  flow.underlying.close

def isClosed (flow : ProgramFlow ψ σ ε α) : IO Bool :=
  flow.underlying.isClosed

def subscriberCount (flow : ProgramFlow ψ σ ε α) : IO Nat :=
  flow.underlying.subscriberCount

/-- Read the current state from the primary mutex. -/
def currentState (flow : ProgramFlow ψ σ ε α) : IO σ :=
  flow.stateMutexes.val[0]'flow.stateMutexes.property |>.atomically do return ← get

/-- Subscribe to emissions, running each callback as a `Program` action via `withStateSync`.
    Returns a `ProgramFlowSubscription` that includes `unsubscribe`, `waitForCompletion`,
    and `currentState`. -/
def subscribe
    [inst : Flows.MergeableState σ]
    (flow : ProgramFlow ψ σ ε α)
    (action : Except ε α → Program ψ σ ε Unit)
    : IO (ProgramFlowSubscription σ) := do
  let sub ← SharedFlow.subscribe flow.underlying.toSharedFlow fun exceptVal => do
    discard <| Flows.withStateSync flow.stateMutexes.val flow.config (action exceptVal)
    pure ()
  pure { toSubscription := sub, currentState := flow.currentState }

/-- Flush buffered emissions and sync the primary mutex's state back into the `Program` monad. -/
def flush (flow : ProgramFlow ψ σ ε α) : Program ψ σ ε Unit := do
  flow.underlying.flush
  let updatedState ← flow.stateMutexes.val[0]'flow.stateMutexes.property |>.atomically do return ← get
  MonadState.set updatedState

def combine
    [Flows.Combinable ψ]
    (flow1 : ProgramFlow ψ σ ε α)
    (flow2 : ProgramFlow ψ σ ε β)
    : IO (ProgramFlow ψ σ ε (α ⊕ β)) := do
  let combined ← MutableSharedFlow.combine flow1.underlying flow2.underlying
  let result ← Flows.map combined (fun
    | .inl (.ok a) => Except.ok (.inl a)
    | .inl (.error e) => Except.error e
    | .inr (.ok b) => Except.ok (.inr b)
    | .inr (.error e) => Except.error e)
  pure
    { underlying := result
      stateMutexes := ⟨flow1.stateMutexes.val ++ flow2.stateMutexes.val, by grind⟩
      config := Flows.Combinable.combine flow1.config flow2.config }

end ProgramFlow

instance
  [Flows.MergeableState σ]
  [Flows.Combinable ψ]
  : Flows
      (ProgramFlow ψ σ ε ·)
      (Program ψ σ ε)
      (Except ε) where
  combine := ProgramFlow.combine
  subscribe := fun flow action => do
    let sub ← ProgramFlow.subscribe flow action
    pure sub.toSubscription
  flush := ProgramFlow.flush

end Flow.Core
