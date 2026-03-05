import Flow.Index
import Flow.Internal.Index

open Flow.Internal
open Flow.Core

structure FlowAccessor (ε α : Type) where
  emit : α → IO Unit
  emitError : ε → IO Unit
  close : IO Unit


structure ReactiveProgramDefinition (f : Type → Type) (ψ σ ε α : Type) where
  initialState : σ
  update : σ → α → σ
  sideEffect : α → FlowAccessor ε α → Program ψ σ ε Unit
  onUpdated : σ → ReaderT ψ (ExceptT ε IO) Unit := fun _ => pure ()
  onError : ε → Option α := fun _ => none
  firstMessage : Option α := none
  sources : Array (f α)
  onClose : Array (IO Unit)

namespace ReactiveProgram

def init
    [IOSubscribable f (Except ε)]
    [Flows.MergeableState σ]
    [BEq σ]
    (definition : ReactiveProgramDefinition f ψ σ ε α)
    : Program ψ σ ε (IO.Promise (Option (Except ε σ))) := do
  let (flow : ProgramFlow ψ σ ε α) ← ProgramFlow.create (replay := 10)

  for source in definition.sources do
    let sub ← IOSubscribable.subscribe source fun exceptVal =>
      match exceptVal with
      | Except.ok a => flow.emit a
      | Except.error e => flow.emitError e
    flow.underlying.state.atomically do
      let state ← get
      set { state with closeActions := state.closeActions.push sub.unsubscribe }

  flow.underlying.state.atomically do
    let state ← get
    set { state with closeActions := state.closeActions ++ definition.onClose }

  let accessor : FlowAccessor ε α :=
    { emit := flow.emit
      emitError := flow.emitError
      close := flow.close }

  let processMsg (event : α) : Program ψ σ ε Unit := do
    let currentState ← MonadState.get
    let newState := definition.update currentState event
    MonadState.set newState
    if newState != currentState then
      definition.onUpdated newState
    definition.sideEffect event accessor

  let readFinalState : IO (Option σ) := do
    if h : 0 < flow.stateMutexes.size then
      flow.stateMutexes[0].atomically do return ← get
    else
      pure none

  let finishingPromise ← IO.Promise.new (α := Option (Except ε σ))

  discard <| flow.underlying.toSharedFlow.subscribe fun (exceptVal : Except ε α) => do
    match exceptVal with
    | .ok event =>
      let result ← Flows.withStateSync flow.stateMutexes flow.config (processMsg event)
      match result with
      | .error e =>
        finishingPromise.resolve <| some <| Except.error e
        flow.close
      | .ok () =>
        if ← flow.isClosed then
          let finalState ← readFinalState
          finishingPromise.resolve <| finalState.map Except.ok
    | .error err =>
      match definition.onError err with
      | some event => flow.emit event
      | none => pure ()

  match definition.firstMessage with
  | some msg => flow.emit msg
  | none => definition.onUpdated definition.initialState

  pure finishingPromise

def launchReactiveProgram
    [IOSubscribable f (Except ε)]
    [Flows.MergeableState σ']
    [BEq σ']
    (definition : ReactiveProgramDefinition f ψ σ' ε α)
    : Program ψ σ ε (Option σ') := do
  let promise ←
    Program.discardState
      (ReactiveProgram.init definition)
      definition.initialState
  let result ← IO.wait promise.result?
  match result.bind (·) with
    | some <| .ok state => pure <| some state
    | some <| .error e => MonadExcept.throw e
    | none => pure none

end ReactiveProgram
