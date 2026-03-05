import Flow
import FlowTest.FlowTestDataBuilder
import FlowTest.Assertions

open Flow.Core Flow.Builders

namespace ToListTests

-- Generic test that works with any flow type that has a Flows instance
def testToListCollectsAllValuesInOrder
    [Monad M]
    [MonadLiftT IO M]
    [MonadLiftT V Option]
    [Flows F M V]
    [Repr α]
    [BEq α]
    (flow : F α)
    (expected : List α)
    : M Unit := do
  let result ← Flows.toList flow
  result |> shouldEqual expected

def testToListCollectsAllValuesInOrder_Flow : IO Unit :=
  testToListCollectsAllValuesInOrder
    (flowOf [1, 2, 3, 4, 5])
    [1, 2, 3, 4, 5]

def testToListCollectsAllValuesInOrder_SharedFlow : IO Unit := do
  let flow ← sharedFlowOf [1, 2, 3, 4, 5]
  testToListCollectsAllValuesInOrder flow [1, 2, 3, 4, 5]

def testToListCollectsAllValuesInOrder_StateFlow : IO Unit := do
  let flow ← MutableStateFlow.create (some 42)
  testToListCollectsAllValuesInOrder flow [42]

def testToListWithEmptyFlow : IO Unit := do
  let flow : Flow Nat := emptyFlow
  let result ← Flows.toList flow
  result |> shouldEqual []

def allTests : List (String × IO Unit) :=
  [ ("toList: collects all values in order (Flow)", testToListCollectsAllValuesInOrder_Flow),
    ("toList: collects all values in order (SharedFlow)", testToListCollectsAllValuesInOrder_SharedFlow),
    ("toList: collects all values in order (StateFlow)", testToListCollectsAllValuesInOrder_StateFlow),
    ("toList: empty flow", testToListWithEmptyFlow) ]

end ToListTests
