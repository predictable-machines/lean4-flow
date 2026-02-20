import PredictableFlow.Core.SharedFlow
import PredictableFlow.Core.Flows
import PredictableCore.Shared.PredictableProgram
import Std

namespace Flow.Core

open PredictableCore.Shared

/-! # MutableProgramFlow - Hot, multicast streams in PredictableProgram context

A MutableProgramFlow wraps a `MutableSharedFlow (Except ε α)` with shared
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

structure MutableProgramFlow (ψ σ ε α : Type) where
  underlying : MutableSharedFlow (Except ε α)
  stateMutexes : Array (Std.Mutex σ)
  config : ψ

instance
  [Flows.MergeableState σ]
  : DerivedFlow (fun α => MutableProgramFlow ψ σ ε α)
where
  derive flow handler := do
    let derived ← DerivedFlow.derive flow.underlying fun emit exceptVal =>
      match exceptVal with
      | .ok a => handler (fun b => emit (Except.ok b)) a
      | .error e => emit (Except.error e)
    pure { underlying := derived, stateMutexes := flow.stateMutexes, config := flow.config }

namespace MutableProgramFlow

def create
    (config : ψ)
    (initialState : σ)
    (replay : Nat := 0)
    (bufferSize : Nat := 64)
    (onBufferOverflow : BufferOverflow := .dropOldest)
    : IO (MutableProgramFlow ψ σ ε α) := do
  let underlying ← MutableSharedFlow.create
    (α := Except ε α)
    (replay := replay)
    (bufferSize := bufferSize)
    (onBufferOverflow := onBufferOverflow)
  let stateMutex ← Std.Mutex.new initialState
  pure { underlying, stateMutexes := #[stateMutex], config }

def init
    (replay : Nat := 0)
    (bufferSize : Nat := 64)
    (onBufferOverflow : BufferOverflow := .dropOldest)
    : Program ψ σ ε (MutableProgramFlow ψ σ ε α) := fun config state => do
  let flow ← create config (initialState := state) (replay := replay) (bufferSize := bufferSize)
    (onBufferOverflow := onBufferOverflow)
  pure (.ok flow, state)

def emit (flow : MutableProgramFlow ψ σ ε α) (value : α) : IO Unit :=
  flow.underlying.emit (.ok value)

def emitError (flow : MutableProgramFlow ψ σ ε α) (error : ε) : IO Unit :=
  flow.underlying.emit (.error error)

def emitExcept (flow : MutableProgramFlow ψ σ ε α) (value : Except ε α) : IO Unit :=
  flow.underlying.emit value

def emitAll (flow : MutableProgramFlow ψ σ ε α) (values : List α) : IO Unit := do
  for value in values do
    flow.emit value

def close (flow : MutableProgramFlow ψ σ ε α) : IO Unit :=
  flow.underlying.close

def isClosed (flow : MutableProgramFlow ψ σ ε α) : IO Bool :=
  flow.underlying.isClosed

def subscriberCount (flow : MutableProgramFlow ψ σ ε α) : IO Nat :=
  flow.underlying.subscriberCount

def subscribe
    [inst : Flows.MergeableState σ]
    (flow : MutableProgramFlow ψ σ ε α)
    (action : Except ε α → Program ψ σ ε Unit)
    : IO (IO Unit) := do
  flow.underlying.subscribe fun exceptVal => do
    let _ ← Flows.withStateSync flow.stateMutexes flow.config (action exceptVal)
    pure ()

def flush (flow : MutableProgramFlow ψ σ ε α) : Program ψ σ ε Unit := do
  flow.underlying.flush
  if h : 0 < flow.stateMutexes.size then
    let updatedState ← flow.stateMutexes[0].atomically do return ← get
    MonadState.set updatedState

def combine
    [Flows.Combinable ψ]
    (flow1 : MutableProgramFlow ψ σ ε α)
    (flow2 : MutableProgramFlow ψ σ ε β)
    : IO (MutableProgramFlow ψ σ ε (α ⊕ β)) := do
  let combined ← MutableSharedFlow.combine flow1.underlying flow2.underlying
  let result ← Flows.map combined (fun
    | .inl (.ok a) => Except.ok (.inl a)
    | .inl (.error e) => Except.error e
    | .inr (.ok b) => Except.ok (.inr b)
    | .inr (.error e) => Except.error e)
  pure
    { underlying := result
      stateMutexes := flow1.stateMutexes ++ flow2.stateMutexes
      config := Flows.Combinable.combine flow1.config flow2.config }

end MutableProgramFlow

instance
  [ProgramLogger ψ]
  [Flows.MergeableState σ]
  [Flows.Combinable ψ]
  : Flows
      (MutableProgramFlow ψ σ ε ·)
      (Program ψ σ ε)
      (Except ε) where
  combine := MutableProgramFlow.combine
  subscribe := fun flow action => MutableProgramFlow.subscribe flow action
  flush := MutableProgramFlow.flush

end Flow.Core
