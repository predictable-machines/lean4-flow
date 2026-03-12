import Flow
import FlowTest.Assertions

open Flow.Core Flow.Builders Flow.Internal

instance : Flows.MergeableState Nat where
  merge _ new _ := new
  withEmptyAppendable s := s

def runProgram (action : Program Unit Nat String α) : IO α := do
  let (result, _) ← Program.run action () 0
  match result with
  | .ok a => pure a
  | .error e => throw <| IO.userError s!"Program failed: {e}"

namespace ProgramFlowTests

def testClosePreventsNewEmissions : IO Unit := do
  let flow ← runProgram <| ProgramFlow.create (ψ := Unit) (σ := Nat) (ε := String) (α := Nat)

  let values ← IO.mkRef ([] : List Nat)
  discard <| flow.subscribe fun v => do
    match v with
    | .ok n => values.modify (n :: ·)
    | .error _ => pure ()

  flow.emit 1
  flow.underlying.flush
  flow.close

  let closed ← flow.isClosed
  closed |> shouldEqual true

  flow.emit 2

  let vals ← values.get
  vals |> shouldEqual [1]

-- StateFlow.update calls value then emit. After close, emit silently drops,
-- so update has no effect even though value still returns the last emitted value.
def testUpdateAfterCloseIsNoOp : IO Unit := do
  let flow ← MutableStateFlow.create (some 10)

  let values ← IO.mkRef ([] : List Nat)
  discard <| flow.subscribe fun v => do
    values.modify (v :: ·)

  flow.update (· + 1)
  flow.flush
  let v1 ← flow.value
  v1 |> shouldBeSome 11

  flow.close
  flow.update (· + 100)

  let vals ← values.get
  vals |> shouldEqual [11, 10]

  -- value still returns the last emitted value (replay cache is not cleared by close)
  let v2 ← flow.value
  v2 |> shouldBeSome 11

def allTests : List (String × IO Unit) := [
    ("ProgramFlow: close prevents new emissions", testClosePreventsNewEmissions),
    ("StateFlow: update after close is no-op", testUpdateAfterCloseIsNoOp)
  ]

end ProgramFlowTests
