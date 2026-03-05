import Flow
import FlowTest.FlowTestDataBuilder
import FlowTest.Assertions

open Flow.Core Flow.Builders

namespace ForEachTests

-- Generic test: forEach counts each element (for IO-based flows)
def testForEachCallsCallbackForEachElement
    [MonadLiftT V Option]
    [Flows F IO V]
    (flow : F Nat)
    (expectedCount : Nat)
    : IO Unit := do
  let count ← IO.mkRef 0
  Flows.forEach flow fun _ =>
    count.modify (· + 1)
  (← count.get) |> shouldEqual expectedCount

def testForEachCallsCallbackForEachElement_Flow : IO Unit :=
  testForEachCallsCallbackForEachElement (flowOf [1, 2, 3]) 3

def testForEachCallsCallbackForEachElement_SharedFlow : IO Unit := do
  let flow ← sharedFlowOf [1, 2, 3]
  testForEachCallsCallbackForEachElement flow 3

def testForEachCallsCallbackForEachElement_StateFlow : IO Unit := do
  let flow ← MutableStateFlow.create (some 42)
  testForEachCallsCallbackForEachElement flow 1

def testCombineWithEmptyFlows : IO Unit := do
  let flow1 : Flow Nat := emptyFlow
  let flow2 := flowOf [10, 20]
  let combined ← Flow.combine flow1 flow2
  let count ← IO.mkRef 0
  Flows.forEach combined fun _ =>
    count.modify (· + 1)
  (← count.get) |> shouldEqual 2

def allTests : List (String × IO Unit) :=
  [ ("forEach: calls callback for each element (Flow)", testForEachCallsCallbackForEachElement_Flow),
    ("forEach: calls callback for each element (SharedFlow)", testForEachCallsCallbackForEachElement_SharedFlow),
    ("forEach: calls callback for each element (StateFlow)", testForEachCallsCallbackForEachElement_StateFlow),
    ("forEach: combine with empty first flow", testCombineWithEmptyFlows) ]

end ForEachTests
