import Flow
import FlowTest.FlowTestDataBuilder
import FlowTest.Assertions

open Flow.Core Flow.Builders

namespace BuildersTests

def testRangeCreatesCorrectSequence : IO Unit := do
  let flow := range 1 5
  let result ← Flows.toList flow
  result |> shouldEqual [1, 2, 3, 4, 5]

def testJustCreatesSingleValueFlow : IO Unit := do
  let flow := just 42
  let result ← Flows.toList flow
  result |> shouldEqual [42]

def testReplicateCreatesRepeatedValues : IO Unit := do
  let flow := replicate "x" 3
  let result ← Flows.toList flow
  result |> shouldEqual ["x", "x", "x"]

def allTests : List (String × IO Unit) :=
  [ ("builders: range creates correct sequence", testRangeCreatesCorrectSequence),
    ("builders: just creates single-value flow", testJustCreatesSingleValueFlow),
    ("builders: replicate creates repeated values", testReplicateCreatesRepeatedValues) ]

end BuildersTests
