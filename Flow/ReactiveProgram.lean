import PredictableCore.Shared.Index
import PredictableFlow.Index

open PredictableCore.Shared
open Flow.Core

namespace PredictableProgram

def init
    {σ α ε : Type}
    (definition : ReactiveProgramDefinition σ α ε)
    : BaseProgram (MutableSharedFlow (σ × ReactiveMessage α ε)) := do
  let config ← PredictableProgram.ask
  let programState ← PredictableProgram.get
  let flow ← MutableSharedFlow.create (replay := 1)
  let processMessage : σ × ReactiveMessage α ε → IO Unit :=
    fun (state, msg) => do
      let newState := definition.update state msg
      let result ← PredictableProgram.run (definition.sideEffect newState msg) config programState
      match result with
      | .ok (some a, _) => flow.emit (newState, .result a)
      | _ => pure ()
  let _ ← flow.collect processMessage
  pure flow

end PredictableProgram
