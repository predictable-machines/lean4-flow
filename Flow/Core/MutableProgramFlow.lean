import PredictableFlow.Core.SharedFlow
import PredictableFlow.Core.Flows
import PredictableCore.Shared.PredictableProgram
import Std

namespace Flow.Core

open PredictableCore.Shared

/-! # MutableProgramFlow - Hot, multicast streams in PredictableProgram context

A MutableProgramFlow wraps a `MutableSharedFlow (Except ε α)` with a shared
`Std.Mutex PredictableState` so that collection callbacks can run in the
`PredictableProgram ε` monad with thread-safe state access.

Emissions broadcast to all subscribers (hot semantics). The mutex is shared across
mapped/filtered children so all operations in a pipeline share the same PredictableState.
-/

structure MutableProgramFlow (ε : Type) (α : Type) where
  underlying : MutableSharedFlow (Except ε α)
  stateMutex : Std.Mutex PredictableState
  config : PredictableConfig

instance : DerivedFlow (MutableProgramFlow ε) where
  derive flow handler := do
    let derived ← DerivedFlow.derive flow.underlying fun emit exceptVal =>
      match exceptVal with
      | .ok a => handler (fun b => emit (.ok b)) a
      | .error e => emit (.error e)
    pure { underlying := derived, stateMutex := flow.stateMutex, config := flow.config }

namespace MutableProgramFlow

def create
    (config : PredictableConfig)
    (initialState : PredictableState := default)
    (replay : Nat := 0)
    (bufferSize : Nat := 64)
    (onBufferOverflow : BufferOverflow := .dropOldest)
    : IO (MutableProgramFlow ε α) := do
  let underlying ← MutableSharedFlow.create
    (α := Except ε α)
    (replay := replay)
    (bufferSize := bufferSize)
    (onBufferOverflow := onBufferOverflow)
  let stateMutex ← Std.Mutex.new initialState
  pure { underlying, stateMutex, config }

def init
    (replay : Nat := 0)
    (bufferSize : Nat := 64)
    (onBufferOverflow : BufferOverflow := .dropOldest)
    : PredictableProgram ε (MutableProgramFlow ε α) := do
  let config ← PredictableProgram.ask
  let state ← PredictableProgram.get
  (create config (initialState := state) (replay := replay) (bufferSize := bufferSize)
    (onBufferOverflow := onBufferOverflow) : IO _)

def emit (flow : MutableProgramFlow ε α) (value : α) : IO Unit :=
  flow.underlying.emit (.ok value)

def emitError (flow : MutableProgramFlow ε α) (error : ε) : IO Unit :=
  flow.underlying.emit (.error error)

def emitExcept (flow : MutableProgramFlow ε α) (value : Except ε α) : IO Unit :=
  flow.underlying.emit value

def emitAll (flow : MutableProgramFlow ε α) (values : List α) : IO Unit := do
  for value in values do
    flow.emit value

def close (flow : MutableProgramFlow ε α) : IO Unit :=
  flow.underlying.close

def isClosed (flow : MutableProgramFlow ε α) : IO Bool :=
  flow.underlying.isClosed

def subscriberCount (flow : MutableProgramFlow ε α) : IO Nat :=
  flow.underlying.subscriberCount

def subscribe
    (flow : MutableProgramFlow ε α)
    (action : Except ε α → PredictableProgram ε Unit)
    : IO (IO Unit) := do
  flow.underlying.subscribe fun exceptVal => do
    let _ ← Flows.withStateSync flow.stateMutex flow.config (action exceptVal)
    pure ()

def flush (flow : MutableProgramFlow ε α) : PredictableProgram ε Unit := do
  flow.underlying.flush
  let updatedState ← (flow.stateMutex.atomically do get : IO PredictableState)
  PredictableProgram.put updatedState

def combine
    (flow1 : MutableProgramFlow ε α)
    (flow2 : MutableProgramFlow ε β)
    : IO (MutableProgramFlow ε (α ⊕ β)) := do
  let combined ← MutableSharedFlow.combine flow1.underlying flow2.underlying
  let result ← Flows.map combined (fun
    | .inl (.ok a) => Except.ok (.inl a)
    | .inl (.error e) => Except.error e
    | .inr (.ok b) => Except.ok (.inr b)
    | .inr (.error e) => Except.error e)
  pure { underlying := result, stateMutex := flow1.stateMutex, config := flow1.config }

end MutableProgramFlow

instance : Flows (MutableProgramFlow ε) (PredictableProgram ε) (Except ε) where
  combine := MutableProgramFlow.combine
  subscribe := MutableProgramFlow.subscribe
  flush := MutableProgramFlow.flush

end Flow.Core
