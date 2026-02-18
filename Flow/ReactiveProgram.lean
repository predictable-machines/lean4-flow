import PredictableCore.Shared.Index
import PredictableFlow.Index

open PredictableCore.Shared
open Flow.Core

namespace PredictableProgram

structure ReactiveProgramHandler (α ε : Type) where
  writeFlow : MutableProgramFlow (ReactiveError ε) (ReactiveMessage α ε)
  finishingPromise : IO.Promise Unit

def isExit : ReactiveMessage α ε → Bool
  | .user UserMessage.exit => true
  | _ => false

def init
    {σ α ε : Type}
    (definition : ReactiveProgramDefinition σ α ε)
    : BaseProgram (ReactiveProgramHandler α ε) := do
  let config ← PredictableProgram.ask
  let programState ← PredictableProgram.get
  let (stateFlow : MutableStateFlow σ) ← MutableStateFlow.create definition.initialState
  let writeFlow ← MutableProgramFlow.create
    config
    (initialState := programState)
    (replay := 10)

  let finishingPromise ← IO.Promise.new (α := Unit)

  let processMessage
      (exceptMsg : Except (ReactiveError ε)
      (ReactiveMessage α ε))
      : PredictableProgram (ReactiveError ε) Unit := do
    match exceptMsg with
    | .error _ => pure ()
    | .ok msg =>
      if isExit msg then
        finishingPromise.resolve ()
        stateFlow.close
        writeFlow.close
        return ()
      match ← (stateFlow.value : IO (Option σ)) with
      | none => pure ()
      | some state =>
      let newState := definition.update state msg
      (stateFlow.emit newState : IO Unit)
      let _ ←
        IO.asTask do
          match ← Flows.withStateSync
            writeFlow.stateMutex
            writeFlow.config
            (definition.sideEffect newState msg writeFlow.emit)
          with
          | .ok () => pure ()
          | .error err => writeFlow.emit (.error err)

      pure ()

  let processState (state : σ) : IO Unit := do
    match ← Flows.withStateSync
      writeFlow.stateMutex
      writeFlow.config
      (definition.onUpdated state)
    with
    | .ok () => pure ()
    | .error err => writeFlow.emit (.error err)

  let _ ← stateFlow.subscribe processState
  let _ ← writeFlow.subscribe processMessage

  pure { writeFlow, finishingPromise }

end PredictableProgram
