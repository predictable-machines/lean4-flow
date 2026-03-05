import Flow
import FlowTest.FlowTestDataBuilder
import FlowTest.Assertions

open Flow.Core Flow.Builders

namespace SubscribeTests

-- Generic test: subscribe + flush counts each element (for IO-based flows)
def testSubscribeCallsCallbackForEachElement
    [MonadLiftT V Option]
    [Flows F IO V]
    (flow : F Nat)
    (expectedCount : Nat)
    : IO Unit := do
  let count ← IO.mkRef 0
  let sub ← Flows.subscribe flow fun _ =>
    count.modify (· + 1)
  Flows.flush flow
  sub.unsubscribe
  (← count.get) |> shouldEqual expectedCount

def testSubscribeCallsCallbackForEachElement_Flow : IO Unit :=
  testSubscribeCallsCallbackForEachElement (flowOf [1, 2, 3]) 3

def testSubscribeCallsCallbackForEachElement_SharedFlow : IO Unit := do
  let flow ← sharedFlowOf [1, 2, 3]
  testSubscribeCallsCallbackForEachElement flow 3

def testSubscribeCallsCallbackForEachElement_StateFlow : IO Unit := do
  let flow ← MutableStateFlow.create (some 42)
  testSubscribeCallsCallbackForEachElement flow 1

def testCombineWithEmptyFlows : IO Unit := do
  let flow1 : Flow Nat := emptyFlow
  let flow2 := flowOf [10, 20]
  let combined ← Flow.combine flow1 flow2
  let count ← IO.mkRef 0
  let sub ← Flows.subscribe combined fun _ =>
    count.modify (· + 1)
  Flows.flush combined
  sub.unsubscribe
  (← count.get) |> shouldEqual 2

def testSubscribeReturnsSubscriptionWithUnsubscribe : IO Unit := do
  let flow ← MutableSharedFlow.create (α := Nat)
  let count ← IO.mkRef 0
  let sub ← SharedFlow.subscribe flow.toSharedFlow fun _ =>
    count.modify (· + 1)
  flow.emit 1
  flow.emit 2
  SharedFlow.flush flow.toSharedFlow
  sub.unsubscribe
  flow.emit 3
  SharedFlow.flush flow.toSharedFlow
  (← count.get) |> shouldEqual 2

def testWaitForCompletionBlocksUntilSubscriberTaskEnds : IO Unit := do
  let flow ← MutableSharedFlow.create (α := Nat)
  let values ← IO.mkRef ([] : List Nat)
  let sub ← SharedFlow.subscribe flow.toSharedFlow fun v =>
    values.modify (v :: ·)
  -- Spawn a task that waits 5ms then closes the flow
  let _ ← IO.asTask do
    IO.sleep 1
    flow.emit 1
    flow.emit 2
    IO.sleep 1
    flow.close
  -- waitForCompletion blocks until the subscriber's processing loop ends,
  -- which happens after close tears down the channel
  sub.waitForCompletion
  (← values.get) |> shouldEqual [2, 1]

def testWaitForCompletionOnColdFlow : IO Unit := do
  let flow := flowOf [1, 2, 3]
  let count ← IO.mkRef 0
  let sub ← Flow.subscribe flow fun _ =>
    count.modify (· + 1)
  -- Cold flow completes synchronously, waitForCompletion is a no-op
  sub.waitForCompletion
  (← count.get) |> shouldEqual 3

def allTests : List (String × IO Unit) :=
  [ ("subscribe: calls callback for each element (Flow)", testSubscribeCallsCallbackForEachElement_Flow),
    ("subscribe: calls callback for each element (SharedFlow)", testSubscribeCallsCallbackForEachElement_SharedFlow),
    ("subscribe: calls callback for each element (StateFlow)", testSubscribeCallsCallbackForEachElement_StateFlow),
    ("subscribe: combine with empty first flow", testCombineWithEmptyFlows),
    ("subscribe: returns Subscription with unsubscribe", testSubscribeReturnsSubscriptionWithUnsubscribe),
    ("subscribe: waitForCompletion blocks until subscriber task ends", testWaitForCompletionBlocksUntilSubscriberTaskEnds),
    ("subscribe: waitForCompletion on cold flow is no-op", testWaitForCompletionOnColdFlow) ]

end SubscribeTests
