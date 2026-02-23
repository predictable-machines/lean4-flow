import PredictableCore.Shared.Index
import PredictableFlow.Index

open PredictableCore.Shared
open Flow.Core

structure FlowAccessor (α : Type) where
  emit : α → IO Unit
  close : IO Unit

structure ReactiveProgramDefinition (ψ σ ε α : Type) where
  initialState : σ
  update : σ → α → σ
  sideEffect : σ → α → FlowAccessor α → Program ψ σ ε Unit
  onUpdated : σ → Program ψ σ ε Unit
  onError : ε → Option α := fun _ => none
  firstMessage : Option α := none
  sourceFlows : List (ProgramFlow ψ σ ε α)

namespace ReactiveProgram

private def combineSourceFlows
    [Flows.MergeableState σ]
    [Flows.Combinable ψ]
    (sources : List (ProgramFlow ψ σ ε α))
    : Program ψ σ ε (ProgramFlow ψ σ ε α) := do
  match sources with
    | [] => ProgramFlow.create (replay := 10)
    | head :: tail =>
      tail.foldlM
        (fun acc source => do
          let combined ← ProgramFlow.combine acc source
          DerivedFlow.derive combined fun emit val =>
            match val with
            | .inl a => emit a
            | .inr a => emit a)
        head

def init
    [Flows.MergeableState σ]
    [ProgramLogger ψ]
    [Flows.Combinable ψ]
    (definition : ReactiveProgramDefinition ψ σ ε α)
    : Program ψ σ ε (IO.Promise (Option (Except ε σ))) := do
  let flow ← combineSourceFlows definition.sourceFlows

  let accessor : FlowAccessor α :=
    { emit := flow.emit
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

  let finishingPromise ← IO.Promise.new (α := Option (Except ε σ))

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
    [Flows.Combinable ψ]
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
