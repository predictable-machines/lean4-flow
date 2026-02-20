import PredictableCore.Shared.Index
import PredictableFlow.Index

open PredictableCore.Shared
open Flow.Core

namespace ReactiveProgram

def init
    [Flows.MergeableState σ]
    [ProgramLogger ψ]
    (definition : ReactiveProgramDefinition ψ σ ε α)
    : Program ψ σ ε (IO.Promise (Option (Except ε σ))) := do
  let state ← MonadState.get
  let config ← MonadReader.read
  let flow : MutableProgramFlow ψ σ ε α ←
    MutableProgramFlow.create config (initialState := state) (replay := 10)
  let finishingPromise ← IO.Promise.new (α := Option (Except ε σ))

  let accessor : FlowAccessor α :=
    { emit := fun m => flow.emit m
      close := flow.close }

  let processMsg (event : α) : Program ψ σ ε Unit := do
    let currentState ← MonadState.get
    let newState := definition.update currentState event
    MonadState.set newState
    let _ ← definition.onUpdated newState
    let stateAfterUpdate ← MonadState.get
    definition.sideEffect stateAfterUpdate event accessor

  let readFinalState : IO (Option σ) := do
    if h : 0 < flow.stateMutexes.size then
      some <$> flow.stateMutexes[0].atomically do return ← get
    else
      pure none

  let _ ← flow.underlying.subscribe fun (exceptVal : Except ε α) => do
    match exceptVal with
    | .ok event =>
      let result ← Flows.withStateSync flow.stateMutexes flow.config (processMsg event)
      match result with
      | .error e =>
        let finalState ← readFinalState
        finishingPromise.resolve (finalState.map fun _ => Except.error e)
        flow.close
      | .ok () =>
        if ← flow.isClosed then
          let finalState ← readFinalState
          finishingPromise.resolve (finalState.map Except.ok)
    | .error err =>
      match definition.onError err with
      | some event => flow.emit event
      | none => pure ()

  match definition.firstMessage with
  | some msg => flow.emit msg
  | none => definition.onUpdated definition.initialState

  pure finishingPromise

def launchReactiveProgram
    [Flows.MergeableState σ']
    [ProgramLogger ψ]
    (definition : ReactiveProgramDefinition ψ σ' ε α)
    : Program ψ σ ε (Option σ') := do
  let promise ←
    Program.discardState
      (ReactiveProgram.init definition)
      definition.initialState
  let result ← IO.wait promise.result?
  match result.bind (·) with
  | some (.ok state) => pure (some state)
  | some (.error e) => PredictableProgram.throw e
  | none => pure none

end ReactiveProgram
