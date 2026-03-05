import Flow
import FlowTest.FlowTestDataBuilder
import FlowTest.Assertions

open Flow.Core Flow.Builders

namespace MapTests

-- Generic test that works with any flow type that has map
def testMapTransformsValues
    [Monad M]
    [MonadLiftT IO M]
    [MonadLiftT V Option]
    [Flows F M V]
    [Repr (V Nat)]
    [BEq (V Nat)]
    (mappedFlow : F Nat)
    (expected : List (Nat))
    : M Unit := do
  let result ← Flows.toList mappedFlow
  result |> shouldEqual expected

def testMapTransformsValues_Flow : IO Unit := do
  let flow := flowOf [1, 2, 3]
  let mapped ← Flows.map flow (· * 2)
  testMapTransformsValues mapped [2, 4, 6]

def testMapTransformsValues_SharedFlow : IO Unit := do
  let flow ← sharedFlowOf [1, 2, 3]
  let mapped ← Flows.map flow (· * 2)
  testMapTransformsValues mapped [2, 4, 6]

def testMapTransformsValues_StateFlow : IO Unit := do
  let flow ← MutableStateFlow.create (some 0)
  let mapped ← Flows.map flow (· * 2)
  testMapTransformsValues mapped [0]

def allTests : List (String × IO Unit) :=
  [ ("map: transforms values (Flow)", testMapTransformsValues_Flow),
    ("map: transforms values (SharedFlow)", testMapTransformsValues_SharedFlow),
    ("map: transforms values (StateFlow)", testMapTransformsValues_StateFlow) ]

end MapTests
