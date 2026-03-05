import Flow
import FlowTest.Assertions

open Flow.Core Flow.Builders

namespace SharedFlowTests

def testMultipleConsumersAndCancellation : IO Unit := do
  let flow ← MutableSharedFlow.create (α := Nat)

  let consumer1Values ← IO.mkRef ([] : List Nat)
  let consumer2Values ← IO.mkRef ([] : List Nat)
  let consumer3Values ← IO.mkRef ([] : List Nat)

  discard <|flow.subscribe fun v => do
    consumer1Values.modify (v :: ·)
  let subscription2 ← flow.subscribe fun v => do
    consumer2Values.modify (v :: ·)
  let subscription3 ← flow.subscribe fun v => do
    consumer3Values.modify (v :: ·)

  flow.emit 10
  flow.flush
  subscription2.unsubscribe
  flow.emit 20
  flow.flush
  subscription3.unsubscribe
  flow.emit 30
  flow.flush

  let vals1 ← consumer1Values.get
  let vals2 ← consumer2Values.get
  let vals3 ← consumer3Values.get

  vals1 |> shouldEqual [30, 20, 10]
  vals2 |> shouldEqual [10]
  vals3 |> shouldEqual [20, 10]
  flow.close

def testReplayBufferForNewSubscribers : IO Unit := do
  let flow ← MutableSharedFlow.create (α := Nat) (replay := 2)

  flow.emit 1
  flow.emit 2
  flow.emit 3

  let lateSubscriberValues ← IO.mkRef ([] : List Nat)
  discard <|flow.subscribe fun v => do
    lateSubscriberValues.modify (v :: ·)
  flow.flush

  let vals ← lateSubscriberValues.get
  vals |> shouldEqual [3, 2]
  flow.close

def testSubscriberCountTracking : IO Unit := do
  let flow ← MutableSharedFlow.create (α := String)

  let count0 ← flow.subscriberCount
  count0 |> shouldEqual 0

  let subscription ← flow.subscribe fun _ => pure ()
  let count1 ← flow.subscriberCount
  count1 |> shouldEqual 1

  subscription.unsubscribe
  let count2 ← flow.subscriberCount
  count2 |> shouldEqual 0
  flow.close

def testEmitAllSendsMultipleValues : IO Unit := do
  let flow ← MutableSharedFlow.create (α := Nat)

  let consumer1 ← IO.mkRef ([] : List Nat)
  let consumer2 ← IO.mkRef ([] : List Nat)

  discard <|flow.subscribe fun v => do
    consumer1.modify (v :: ·)
  discard <|flow.subscribe fun v => do
    consumer2.modify (v :: ·)

  flow.emitAll [100, 200, 300, 400, 500]
  flow.flush

  let vals1 ← consumer1.get
  let vals2 ← consumer2.get

  vals1.length |> shouldEqual 5
  vals1 |> shouldEqual vals2
  flow.close

def testClosePreventsNewEmissions : IO Unit := do
  let flow ← MutableSharedFlow.create (α := Nat)

  let values ← IO.mkRef ([] : List Nat)
  discard <|flow.subscribe fun v => do
    values.modify (v :: ·)

  flow.emit 1
  flow.flush
  flow.close

  let closed ← flow.isClosed
  closed |> shouldEqual true

  let errorThrown ← do
    try
      flow.emit 2
      pure false
    catch _ =>
      pure true

  errorThrown |> shouldEqual true

  let vals ← values.get
  vals |> shouldEqual [1]

def testCloseCascadesToMapChild : IO Unit := do
  let parent ← MutableSharedFlow.create (α := Nat)
  let child ← Flows.map parent (· * 2)

  let childValues ← IO.mkRef ([] : List Nat)
  discard <|child.subscribe fun v => do
    childValues.modify (v :: ·)

  parent.emit 5
  child.flush

  (← childValues.get) |> shouldEqual [10]

  parent.close

  let parentClosed ← parent.isClosed
  let childClosed ← child.isClosed

  parentClosed |> shouldEqual true
  childClosed |> shouldEqual true

def testCloseCascadesToFilterChild : IO Unit := do
  let parent ← MutableSharedFlow.create (α := Nat)
  let child ← Flows.filter parent (· > 5)

  let childValues ← IO.mkRef ([] : List Nat)
  discard <|child.subscribe fun v => do
    childValues.modify (v :: ·)

  parent.emit 3
  parent.emit 10
  child.flush

  (← childValues.get) |> shouldEqual [10]

  parent.close

  let parentClosed ← parent.isClosed
  let childClosed ← child.isClosed

  parentClosed |> shouldEqual true
  childClosed |> shouldEqual true

def testCloseCascadesToFilterMapChild : IO Unit := do
  let parent ← MutableSharedFlow.create (α := Nat)
  let child ← MutableSharedFlow.filterMap parent.toSharedFlow fun n =>
    if n % 2 == 0 then some (n * 10) else none

  let childValues ← IO.mkRef ([] : List Nat)
  discard <|child.subscribe fun v => do
    childValues.modify (v :: ·)

  parent.emit 1
  parent.emit 2
  parent.emit 3
  parent.emit 4
  child.flush

  (← childValues.get) |> shouldEqual [40, 20]

  parent.close

  let parentClosed ← parent.isClosed
  let childClosed ← child.isClosed

  parentClosed |> shouldEqual true
  childClosed |> shouldEqual true

def testRecursiveCascadingCancellation : IO Unit := do
  let parent ← MutableSharedFlow.create (α := Nat)
  let child ← Flows.map parent (· * 2)
  let grandchild ← Flows.filter child (· > 10)

  let grandchildValues ← IO.mkRef ([] : List Nat)
  discard <|grandchild.subscribe fun v => do
    grandchildValues.modify (v :: ·)

  parent.emit 3
  parent.emit 10
  grandchild.flush

  (← grandchildValues.get) |> shouldEqual [20]

  parent.close

  let parentClosed ← parent.isClosed
  let childClosed ← child.isClosed
  let grandchildClosed ← grandchild.isClosed

  parentClosed |> shouldEqual true
  childClosed |> shouldEqual true
  grandchildClosed |> shouldEqual true

def testFlushMapChildCascadesToParent : IO Unit := do
  let parent ← MutableSharedFlow.create (α := Nat)
  let child ← Flows.map parent (· * 2)

  let parentValues ← IO.mkRef ([] : List Nat)
  let childValues ← IO.mkRef ([] : List Nat)

  discard <|parent.subscribe fun v => do
    parentValues.modify (v :: ·)
  discard <|child.subscribe fun v => do
    childValues.modify (v :: ·)

  parent.emit 5
  parent.emit 10

  child.flush

  (← parentValues.get) |> shouldEqual [10, 5]
  (← childValues.get) |> shouldEqual [20, 10]
  parent.close

def testFlushFilterChildCascadesToParent : IO Unit := do
  let parent ← MutableSharedFlow.create (α := Nat)
  let child ← Flows.filter parent (· > 5)

  let parentValues ← IO.mkRef ([] : List Nat)
  let childValues ← IO.mkRef ([] : List Nat)

  discard <|parent.subscribe fun v => do
    parentValues.modify (v :: ·)
  discard <|child.subscribe fun v => do
    childValues.modify (v :: ·)

  parent.emit 3
  parent.emit 10

  child.flush

  (← parentValues.get) |> shouldEqual [10, 3]
  (← childValues.get) |> shouldEqual [10]
  parent.close

def testFlushFilterMapChildCascadesToParent : IO Unit := do
  let parent ← MutableSharedFlow.create (α := Nat)
  let child ← MutableSharedFlow.filterMap parent.toSharedFlow fun n =>
    if n % 2 == 0 then some (n * 10) else none

  let parentValues ← IO.mkRef ([] : List Nat)
  let childValues ← IO.mkRef ([] : List Nat)

  discard <|parent.subscribe fun v => do
    parentValues.modify (v :: ·)
  discard <|child.subscribe fun v => do
    childValues.modify (v :: ·)

  parent.emit 1
  parent.emit 2
  parent.emit 3
  parent.emit 4

  child.flush

  (← parentValues.get) |> shouldEqual [4, 3, 2, 1]
  (← childValues.get) |> shouldEqual [40, 20]
  parent.close

def testRecursiveCascadingFlush : IO Unit := do
  let parent ← MutableSharedFlow.create (α := Nat)
  let child ← Flows.map parent (· * 2)
  let grandchild ← Flows.filter child (· > 10)

  let parentValues ← IO.mkRef ([] : List Nat)
  let childValues ← IO.mkRef ([] : List Nat)
  let grandchildValues ← IO.mkRef ([] : List Nat)

  discard <|parent.subscribe fun v => do
    parentValues.modify (v :: ·)
  discard <|child.subscribe fun v => do
    childValues.modify (v :: ·)
  discard <|grandchild.subscribe fun v => do
    grandchildValues.modify (v :: ·)

  parent.emit 3
  parent.emit 10
  parent.emit 15

  grandchild.flush

  (← parentValues.get) |> shouldEqual [15, 10, 3]
  (← childValues.get) |> shouldEqual [30, 20, 6]
  (← grandchildValues.get) |> shouldEqual [30, 20]
  parent.close

def testCombineReceivesFromBothParents : IO Unit := do
  let flow1 ← MutableSharedFlow.create (α := Nat)
  let flow2 ← MutableSharedFlow.create (α := Nat)
  let combined ← MutableSharedFlow.combine flow1 flow2
  let leftValues ← IO.mkRef ([] : List Nat)
  let rightValues ← IO.mkRef ([] : List Nat)
  discard <|combined.subscribe fun v => match v with
    | Sum.inl a => leftValues.modify (· ++ [a])
    | Sum.inr b => rightValues.modify (· ++ [b])
  flow1.emit 1
  flow2.emit 10
  flow1.emit 2
  flow2.emit 20
  combined.flush
  (← leftValues.get) |> shouldEqual [1, 2]
  (← rightValues.get) |> shouldEqual [10, 20]
  flow1.close
  flow2.close

def testCombineCloseCascadesFromParent : IO Unit := do
  let flow1 ← MutableSharedFlow.create (α := Nat)
  let flow2 ← MutableSharedFlow.create (α := Nat)
  let combined ← MutableSharedFlow.combine flow1 flow2
  flow1.close
  let combinedClosed ← combined.isClosed
  combinedClosed |> shouldEqual true
  flow2.close

def testCombineFlushCascadesToParents : IO Unit := do
  let flow1 ← MutableSharedFlow.create (α := Nat)
  let flow2 ← MutableSharedFlow.create (α := Nat)
  let combined ← MutableSharedFlow.combine flow1 flow2
  let parentValues ← IO.mkRef ([] : List Nat)
  let leftValues ← IO.mkRef ([] : List Nat)
  let rightValues ← IO.mkRef ([] : List Nat)
  discard <|flow1.subscribe fun v => do
    parentValues.modify (v :: ·)
  discard <|combined.subscribe fun v => match v with
    | Sum.inl a => leftValues.modify (· ++ [a])
    | Sum.inr b => rightValues.modify (· ++ [b])
  flow1.emit 1
  flow2.emit 10
  combined.flush
  (← parentValues.get) |> shouldEqual [1]
  (← leftValues.get) |> shouldEqual [1]
  (← rightValues.get) |> shouldEqual [10]
  flow1.close
  flow2.close

def allTests : List (String × IO Unit) := [
    ("SharedFlow: multiple consumers and cancellation", testMultipleConsumersAndCancellation),
    ("SharedFlow: replay buffer for new subscribers", testReplayBufferForNewSubscribers),
    ("SharedFlow: subscriber count tracking", testSubscriberCountTracking),
    ("SharedFlow: emitAll sends multiple values", testEmitAllSendsMultipleValues),
    ("SharedFlow: close prevents new emissions", testClosePreventsNewEmissions),
    ("SharedFlow: close cascades to map child", testCloseCascadesToMapChild),
    ("SharedFlow: close cascades to filter child", testCloseCascadesToFilterChild),
    ("SharedFlow: close cascades to filterMap child", testCloseCascadesToFilterMapChild),
    ("SharedFlow: recursive cascading cancellation", testRecursiveCascadingCancellation),
    ("SharedFlow: flush map child cascades to parent", testFlushMapChildCascadesToParent),
    ("SharedFlow: flush filter child cascades to parent", testFlushFilterChildCascadesToParent),
    ("SharedFlow: flush filterMap child cascades to parent", testFlushFilterMapChildCascadesToParent),
    ("SharedFlow: recursive cascading flush", testRecursiveCascadingFlush),
    ("SharedFlow: combine receives from both parents", testCombineReceivesFromBothParents),
    ("SharedFlow: combine close cascades from parent", testCombineCloseCascadesFromParent),
    ("SharedFlow: combine flush cascades to parents", testCombineFlushCascadesToParents)
  ]

end SharedFlowTests
