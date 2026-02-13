import PredictableCore.Shared.Index
import PredictableFlow.Index

open PredictableCore.Shared
open Flow.Core

namespace PredictableProgram

structure ReactiveProgramHandler (α ε : Type) where
  writeFlow : MutableSharedFlow (ReactiveMessage α ε)
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
  let _ ← stateFlow.collect fun s => do
    let _ ← PredictableProgram.run (definition.onUpdated s) config programState
    return ()
  let (writeFlow : MutableSharedFlow (ReactiveMessage α ε)) ← MutableSharedFlow.create (replay := 10)

  let finishingPromise ← IO.Promise.new (α := Unit)

  let processMessage (msg : ReactiveMessage α ε) : IO Unit := do
    if isExit msg then
      finishingPromise.resolve ()
      stateFlow.close
      writeFlow.close
      return ()
    let state ← stateFlow.value

    let newState := definition.update state msg
    stateFlow.emit newState
    let _ ←
      IO.asTask do
        match ← PredictableProgram.run
          (definition.sideEffect newState msg writeFlow.emit) config programState with
        | (.error err, _) => writeFlow.emit (.error err)
        | _ => pure ()

  let processUpdate (state : σ) : IO Unit := do
      let (result, _)  ← PredictableProgram.run (definition.onUpdated state) config programState
      match result with
      | .error err => writeFlow.emit (.error err)
      | _ => pure ()

  let _ ← stateFlow.collect processUpdate
  let _ ← writeFlow.collect processMessage

  pure { writeFlow, finishingPromise }

end PredictableProgram
