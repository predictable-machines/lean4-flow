import PredictableFlow.Core.ProgramFlow
import PredictableFlow.Core.MutableProgramFlow

namespace Flow.Builders

open Flow.Core
open PredictableCore.Shared

def emptyProgramFlow
    (stateMutex : Std.Mutex PredictableState)
    (config : PredictableConfig)
    : ProgramFlow ε α :=
  ProgramFlow.fromExceptList stateMutex config []

def programFlowOf
    (stateMutex : Std.Mutex PredictableState)
    (config : PredictableConfig)
    (items : List α)
    : ProgramFlow ε α :=
  ProgramFlow.fromList stateMutex config items

def programFlowOfExcept
    (stateMutex : Std.Mutex PredictableState)
    (config : PredictableConfig)
    (items : List (Except ε α))
    : ProgramFlow ε α :=
  ProgramFlow.fromExceptList stateMutex config items

def mutableProgramFlow
    (replay : Nat := 0)
    (bufferSize : Nat := 64)
    : PredictableProgram ε (MutableProgramFlow ε α) :=
  MutableProgramFlow.init (replay := replay) (bufferSize := bufferSize)

end Flow.Builders
