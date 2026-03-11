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
  sideEffect : σ → α → FlowAccessor ε α → ReaderT ψ (ExceptT ε IO) Unit
  onUpdated : σ → ReaderT ψ (ExceptT ε IO) Unit := fun _ => pure ()
  onError : ε → Option α := fun _ => none
  firstMessage : Option α := none
  sources : Array (f α)
  onClose : Array (IO Unit)

namespace ReactiveProgram

/-- Initialise a reactive program loop.

    Subscribes to all source flows, routing emissions into an internal `ProgramFlow`.
    Each event is processed by `update` (pure state transition), then `sideEffect` (reader/IO action
    with no state access), and finally `onUpdated` (rendering hook — should be fast and failsafe).
    Returns a promise that resolves to `some (.ok finalState)` when the flow closes normally,
    `some (.error e)` on an unhandled error, or `none` if the promise was never resolved. -/
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
    definition.sideEffect newState event accessor

  let finishingPromise ← IO.Promise.new (α := Option (Except ε σ))

  discard <| flow.underlying.toSharedFlow.subscribe fun (exceptVal : Except ε α) => do
    match exceptVal with
    | .ok event =>
      let result ← Flows.withStateSync flow.stateMutexes.val flow.config (processMsg event)
      match result with
      | .error e =>
        finishingPromise.resolve <| some <| Except.error e
        flow.close
      | .ok () =>
        if ← flow.isClosed then
          let finalState ← flow.currentState
          finishingPromise.resolve <| some <| Except.ok finalState
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
