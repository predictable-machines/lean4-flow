import PredictableFlow.Core.MutableProgramFlow

namespace Flow.Builders

open Flow.Core
open PredictableCore.Shared

def mutableProgramFlow
    [Inhabited σ]
    (replay : Nat := 0)
    (bufferSize : Nat := 64)
    : Program ψ σ ε (MutableProgramFlow ψ σ ε α) :=
  MutableProgramFlow.init (replay := replay) (bufferSize := bufferSize)

end Flow.Builders
