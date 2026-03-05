import Flow
import FlowTest.FlowTestDataBuilder
import FlowTest.Assertions

open Flow.Core Flow.Builders

namespace FilterTests

-- Generic test that works with any flow type that has filter
def testFilterSelectsMatchingValues
    [Monad M]
    [MonadLiftT IO M]
    [MonadLiftT V Option]
    [Flows F M V]
    [Repr (V Nat)]
    [BEq (V Nat)]
    (filteredFlow : F Nat)
    (expected : List Nat)
    : M Unit := do
  let result ← Flows.toList filteredFlow
  result |> shouldEqual expected

def testFilterSelectsMatchingValues_Flow : IO Unit := do
  let flow ← Flows.filter (flowOf [1, 2, 3, 4, 5]) (· % 2 == 0)
  testFilterSelectsMatchingValues flow [2, 4]

def testFilterSelectsMatchingValues_SharedFlow : IO Unit := do
  let flow ← sharedFlowOf [1, 2, 3, 4, 5]
  let filtered ← Flows.filter flow (· % 2 == 0)
  testFilterSelectsMatchingValues filtered [2, 4]

def testFilterSelectsMatchingValues_StateFlow : IO Unit := do
  let flow ← MutableStateFlow.create (some 4)
  let filtered ← MutableStateFlow.filter flow (· % 2 == 0)
  testFilterSelectsMatchingValues filtered [4]

def allTests : List (String × IO Unit) :=
  [ ("filter: selects matching values (Flow)", testFilterSelectsMatchingValues_Flow),
    ("filter: selects matching values (SharedFlow)", testFilterSelectsMatchingValues_SharedFlow),
    ("filter: selects matching values (StateFlow)", testFilterSelectsMatchingValues_StateFlow) ]

end FilterTests
