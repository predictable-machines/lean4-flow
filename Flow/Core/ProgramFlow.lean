import PredictableFlow.Core.Flow
import PredictableFlow.Core.Flows
import PredictableCore.Shared.PredictableProgram
import Std

namespace Flow.Core

open PredictableCore.Shared

/-! # ProgramFlow - Cold, lazy streams in PredictableProgram context

A ProgramFlow wraps a cold `Flow (Except ε α)` with a shared `Std.Mutex PredictableState`
so that collection callbacks can run in the `PredictableProgram ε` monad with thread-safe
state access.

Each collection triggers fresh execution (cold semantics). The mutex is shared across
mapped/filtered children so all operations in a pipeline share the same PredictableState.
-/

structure ProgramFlow (ε : Type) (α : Type) where
  underlying : Flow (Except ε α)
  stateMutex : Std.Mutex PredictableState
  config : PredictableConfig

instance : DerivedFlow (ProgramFlow ε) where
  derive flow handler := do
    let derived ← DerivedFlow.derive flow.underlying fun emit exceptVal =>
      match exceptVal with
      | .ok a => handler (fun b => emit (.ok b)) a
      | .error e => emit (.error e)
    pure { underlying := derived, stateMutex := flow.stateMutex, config := flow.config }

namespace ProgramFlow

def fromList
    (stateMutex : Std.Mutex PredictableState)
    (config : PredictableConfig)
    (items : List α)
    : ProgramFlow ε α :=
  { underlying := Flow.mk fun collector => do
      for item in items do
        collector.emit (.ok item)
    stateMutex, config }

def fromExceptList
    (stateMutex : Std.Mutex PredictableState)
    (config : PredictableConfig)
    (items : List (Except ε α))
    : ProgramFlow ε α :=
  { underlying := Flow.mk fun collector => do
      for item in items do
        collector.emit item
    stateMutex, config }

def subscribe
    (flow : ProgramFlow ε α)
    (action : Except ε α → PredictableProgram ε Unit)
    : IO (IO Unit) := do
  flow.underlying.forEach fun exceptVal => do
    let _ ← Flows.withStateSync flow.stateMutex flow.config (action exceptVal)
    pure ()
  pure (pure ())

def forEach
    (flow : ProgramFlow ε α)
    (action : Except ε α → PredictableProgram ε Unit)
    : PredictableProgram ε Unit := do
  let currentState ← PredictableProgram.get
  let mutex := flow.stateMutex
  (mutex.atomically do set currentState : IO Unit)
  let firstError ← (IO.mkRef (none : Option ε) : IO _)
  (flow.underlying.forEach fun exceptVal => do
    if (← firstError.get).isSome then return
    let result ← Flows.withStateSync mutex flow.config (action exceptVal)
    match result with
    | .ok () => pure ()
    | .error e => firstError.set (some e)
    : IO Unit)
  let updatedState ← (mutex.atomically do get : IO PredictableState)
  PredictableProgram.put updatedState
  match ← (firstError.get : IO _) with
  | some e => PredictableProgram.throw e
  | none => pure ()

def flush (flow : ProgramFlow ε α) : PredictableProgram ε Unit := do
  let updatedState ← (flow.stateMutex.atomically do get : IO PredictableState)
  PredictableProgram.put updatedState

def combine
    (flow1 : ProgramFlow ε α)
    (flow2 : ProgramFlow ε β)
    : IO (ProgramFlow ε (α ⊕ β)) := do
  let combined ← Flow.combine flow1.underlying flow2.underlying
  let transformed ← Flows.map combined (fun
    | .inl (.ok a) => Except.ok (.inl a)
    | .inl (.error e) => Except.error e
    | .inr (.ok b) => Except.ok (.inr b)
    | .inr (.error e) => Except.error e)
  pure { underlying := transformed, stateMutex := flow1.stateMutex, config := flow1.config }

end ProgramFlow

instance : Flows (ProgramFlow ε) (PredictableProgram ε) (Except ε) where
  combine := ProgramFlow.combine
  subscribe := ProgramFlow.subscribe
  flush := ProgramFlow.flush
  forEach := ProgramFlow.forEach

end Flow.Core
