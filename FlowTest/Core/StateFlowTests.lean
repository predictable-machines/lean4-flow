import Flow
import FlowTest.Assertions

open Flow.Core Flow.Builders

namespace StateFlowTests

def testHoldsInitialValue : IO Unit := do
  let flow ← MutableStateFlow.create (some 42)
  let v ← flow.value
  v |> shouldBeSome 42

def testNewSubscribersReceiveCurrentValue : IO Unit := do
  let flow ← MutableStateFlow.create "hello"
  let receivedValues ← IO.mkRef ([] : List String)
  discard <| flow.subscribe fun v => do
    receivedValues.modify (v :: ·)
  flow.flush
  let vals ← receivedValues.get
  vals |> shouldEqual ["hello"]
  flow.close

def testMultipleSubscribersReceiveSameUpdates : IO Unit := do
  let flow ← MutableStateFlow.create (some 0)
  let consumer1Values ← IO.mkRef ([] : List Nat)
  let consumer2Values ← IO.mkRef ([] : List Nat)
  discard <| flow.subscribe fun v => do
    consumer1Values.modify (v :: ·)
  discard <| flow.subscribe fun v => do
    consumer2Values.modify (v :: ·)
  flow.flush
  flow.emit 10
  flow.flush
  flow.emit 20
  flow.flush
  let vals1 ← consumer1Values.get
  let vals2 ← consumer2Values.get
  vals1 |> shouldEqual [20, 10, 0]
  vals2 |> shouldEqual [20, 10, 0]
  flow.close

def testCancellationStopsReceivingValues : IO Unit := do
  let flow ← MutableStateFlow.create (some 0)
  let consumer1Values ← IO.mkRef ([] : List Nat)
  let consumer2Values ← IO.mkRef ([] : List Nat)
  let consumer3Values ← IO.mkRef ([] : List Nat)
  discard <| flow.subscribe fun v => do
    consumer1Values.modify (v :: ·)
  let subscription2 ← flow.subscribe fun v => do
    consumer2Values.modify (v :: ·)
  let subscription3 ← flow.subscribe fun v => do
    consumer3Values.modify (v :: ·)
  flow.flush
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
  vals1 |> shouldEqual [30, 20, 10, 0]
  vals2 |> shouldEqual [10, 0]
  vals3 |> shouldEqual [20, 10, 0]
  flow.close

def testUpdateModifiesValue : IO Unit := do
  let flow ← MutableStateFlow.create (some 5)
  flow.update (· * 2)
  let v1 ← flow.value
  v1 |> shouldBeSome 10
  flow.update (· + 3)
  let v2 ← flow.value
  v2 |> shouldBeSome 13

def testSubscriberCountTracking : IO Unit := do
  let flow ← MutableStateFlow.create (some 0)
  let count0 ← flow.subscriberCount
  count0 |> shouldEqual 0
  let subscription ← flow.subscribe fun _ => pure ()
  let count1 ← flow.subscriberCount
  count1 |> shouldEqual 1
  subscription.unsubscribe
  let count2 ← flow.subscriberCount
  count2 |> shouldEqual 0
  flow.close

def testCompareAndSet : IO Unit := do
  let flow ← MutableStateFlow.create (some 0)
  let success1 ← flow.compareAndSet 0 42
  success1 |> shouldEqual true
  let v1 ← flow.value
  v1 |> shouldBeSome 42
  let success2 ← flow.compareAndSet 0 100
  success2 |> shouldEqual false
  let v2 ← flow.value
  v2 |> shouldBeSome 42

def testClosePreventsNewEmissions : IO Unit := do
  let flow ← MutableStateFlow.create (some 0)
  let values ← IO.mkRef ([] : List Nat)
  discard <| flow.subscribe fun v => do
    values.modify (v :: ·)
  flow.emit 1
  flow.flush
  flow.close
  let closed ← flow.isClosed
  closed |> shouldEqual true
  flow.emit 2
  (← values.get) |> shouldEqual [1, 0]

def testToSharedFlowReceivesReplayAndNewEmissions : IO Unit := do
  let mutableFlow ← MutableSharedFlow.create (α := Nat) (replay := 2)
  mutableFlow.emit 1
  mutableFlow.emit 2
  mutableFlow.emit 3
  let sharedFlow := mutableFlow.toSharedFlow
  let receivedValues ← IO.mkRef ([] : List Nat)
  discard <|sharedFlow.subscribe fun v => do
    receivedValues.modify (v :: ·)
  (← mutableFlow.subscriberCount) |> shouldEqual 1
  (← sharedFlow.subscriberCount) |> shouldEqual 1
  mutableFlow.flush
  (← receivedValues.get) |> shouldEqual [3, 2]
  mutableFlow.emit 4
  mutableFlow.emit 5
  sharedFlow.flush
  (← receivedValues.get) |> shouldEqual [5, 4, 3, 2]
  mutableFlow.close

def testCloseCascadesToMapChild : IO Unit := do
  let parent ← MutableStateFlow.create (some 5)
  let child ← Flows.map parent (· * 2)
  child.flush
  let childValues ← IO.mkRef ([] : List Nat)
  discard <|child.subscribe fun v => do
    childValues.modify (v :: ·)
  child.flush
  (← childValues.get) |> shouldEqual [10]
  parent.emit 10
  child.flush
  (← childValues.get) |> shouldEqual [20, 10]
  parent.close
  let parentClosed ← parent.isClosed
  let childClosed ← child.isClosed
  parentClosed |> shouldEqual true
  childClosed |> shouldEqual true

def testCloseCascadesToFilterChild : IO Unit := do
  let parent ← MutableStateFlow.create (some 10)
  let child ← MutableStateFlow.filter parent (· > 5)
  child.flush
  let childValues ← IO.mkRef ([] : List Nat)
  discard <|child.subscribe fun v => do
    childValues.modify (v :: ·)
  child.flush
  (← childValues.get) |> shouldEqual [10]
  parent.emit 3
  parent.emit 20
  child.flush
  (← childValues.get) |> shouldEqual [20, 10]
  parent.close
  let parentClosed ← parent.isClosed
  let childClosed ← child.isClosed
  parentClosed |> shouldEqual true
  childClosed |> shouldEqual true

def testRecursiveCascadingCancellation : IO Unit := do
  let parent ← MutableStateFlow.create (some 10)
  let child ← Flows.map parent (· * 2)
  let grandchild ← MutableStateFlow.filter child (· > 15)
  grandchild.flush
  let grandchildValues ← IO.mkRef ([] : List Nat)
  discard <|grandchild.subscribe fun v => do
    grandchildValues.modify (v :: ·)
  grandchild.flush
  (← grandchildValues.get) |> shouldEqual [20]
  parent.emit 5
  grandchild.flush
  (← grandchildValues.get) |> shouldEqual [20]
  parent.emit 20
  grandchild.flush
  (← grandchildValues.get) |> shouldEqual [40, 20]
  parent.close
  let parentClosed ← parent.isClosed
  let childClosed ← child.isClosed
  let grandchildClosed ← grandchild.isClosed
  parentClosed |> shouldEqual true
  childClosed |> shouldEqual true
  grandchildClosed |> shouldEqual true

def testFlushMapChildCascadesToParent : IO Unit := do
  let parent ← MutableStateFlow.create (some 5)
  let child ← Flows.map parent (· * 2)
  let parentValues ← IO.mkRef ([] : List Nat)
  let childValues ← IO.mkRef ([] : List Nat)
  discard <|parent.subscribe fun v => do
    parentValues.modify (v :: ·)
  discard <|child.subscribe fun v => do
    childValues.modify (v :: ·)
  parent.emit 10
  child.flush
  parent.emit 15
  child.flush
  (← parentValues.get) |> shouldEqual [15, 10, 5]
  (← childValues.get) |> shouldEqual [30, 20, 10]
  parent.close

def testFlushFilterChildCascadesToParent : IO Unit := do
  let parent ← MutableStateFlow.create (some 10)
  let child ← MutableStateFlow.filter parent (· > 5)
  let parentValues ← IO.mkRef ([] : List Nat)
  let childValues ← IO.mkRef ([] : List Nat)
  discard <|parent.subscribe fun v => do
    parentValues.modify (v :: ·)
  discard <|child.subscribe fun v => do
    childValues.modify (v :: ·)
  parent.emit 3
  child.flush
  parent.emit 20
  child.flush
  (← parentValues.get) |> shouldEqual [20, 3, 10]
  (← childValues.get) |> shouldEqual [20, 10]
  parent.close

def testRecursiveCascadingFlush : IO Unit := do
  let parent ← MutableStateFlow.create (some 10)
  let child ← Flows.map parent (· * 2)
  let grandchild ← MutableStateFlow.filter child (· > 15)
  let parentValues ← IO.mkRef ([] : List Nat)
  let childValues ← IO.mkRef ([] : List Nat)
  let grandchildValues ← IO.mkRef ([] : List Nat)
  discard <|parent.subscribe fun v => do
    parentValues.modify (v :: ·)
  discard <|child.subscribe fun v => do
    childValues.modify (v :: ·)
  discard <|grandchild.subscribe fun v => do
    grandchildValues.modify (v :: ·)
  parent.emit 5
  grandchild.flush
  parent.emit 20
  grandchild.flush
  (← parentValues.get) |> shouldEqual [20, 5, 10]
  (← childValues.get) |> shouldEqual [40, 10, 20]
  (← grandchildValues.get) |> shouldEqual [40, 20]
  parent.close

def testCombineReceivesFromBothParents : IO Unit := do
  let flow1 ← MutableStateFlow.create (some 10)
  let flow2 ← MutableStateFlow.create (some 20)
  let combined ← MutableStateFlow.combine flow1 flow2
  let leftValues ← IO.mkRef ([] : List Nat)
  let rightValues ← IO.mkRef ([] : List Nat)
  discard <|combined.subscribe fun v => match v with
    | Sum.inl a => leftValues.modify (· ++ [a])
    | Sum.inr b => rightValues.modify (· ++ [b])
  combined.flush
  flow1.emit 100
  flow2.emit 200
  combined.flush
  (← leftValues.get) |> shouldEqual [10, 100]
  (← rightValues.get) |> shouldEqual [200]
  flow1.close
  flow2.close

def testCombineCloseCascadesFromParent : IO Unit := do
  let flow1 ← MutableStateFlow.create (some 10)
  let flow2 ← MutableStateFlow.create (some 20)
  let combined ← MutableStateFlow.combine flow1 flow2
  flow1.close
  let combinedClosed ← combined.isClosed
  combinedClosed |> shouldEqual true
  flow2.close

def testCombineFlushCascadesToParents : IO Unit := do
  let flow1 ← MutableStateFlow.create (some 10)
  let flow2 ← MutableStateFlow.create (some 20)
  let combined ← MutableStateFlow.combine flow1 flow2
  let parentValues ← IO.mkRef ([] : List Nat)
  let leftValues ← IO.mkRef ([] : List Nat)
  let rightValues ← IO.mkRef ([] : List Nat)
  discard <| flow1.subscribe fun v => do
    parentValues.modify (v :: ·)
  discard <|combined.subscribe fun v => match v with
    | Sum.inl a => leftValues.modify (a :: ·)
    | Sum.inr b => rightValues.modify (b :: ·)
  flow1.emit 100
  flow2.emit 200
  combined.flush
  (← parentValues.get) |> shouldEqual [100, 10]
  (← leftValues.get) |> shouldEqual [100, 10]
  (← rightValues.get) |> shouldEqual [200]
  flow1.close
  flow2.close

def testDeriveWithFilteredInitialValueStartsNone : IO Unit := do
  let flow ← MutableStateFlow.create (some 0)
  let filtered ← Flows.filterMap flow (fun n => if n > 0 then some n else none)
  let initial ← filtered.value
  initial |> shouldEqual none
  flow.emit 5
  filtered.flush
  let updated ← filtered.value
  updated |> shouldBeSome 5
  flow.close

def testFilterWithFilteredInitialValueStartsNone : IO Unit := do
  let flow ← MutableStateFlow.create (some 0)
  let filtered ← MutableStateFlow.filter flow (· > 0)
  let initial ← filtered.value
  initial |> shouldEqual none
  flow.emit 10
  filtered.flush
  let updated ← filtered.value
  updated |> shouldBeSome 10
  flow.close

def allTests : List (String × IO Unit) := [
    ("StateFlow: holds initial value", testHoldsInitialValue),
    ("StateFlow: new subscribers receive current value", testNewSubscribersReceiveCurrentValue),
    ("StateFlow: multiple subscribers receive same updates", testMultipleSubscribersReceiveSameUpdates),
    ("StateFlow: cancellation stops receiving values", testCancellationStopsReceivingValues),
    ("StateFlow: update modifies value", testUpdateModifiesValue),
    ("StateFlow: subscriber count tracking", testSubscriberCountTracking),
    ("StateFlow: compareAndSet", testCompareAndSet),
    ("StateFlow: close prevents new emissions", testClosePreventsNewEmissions),
    ("StateFlow: toSharedFlow receives replay and new emissions", testToSharedFlowReceivesReplayAndNewEmissions),
    ("StateFlow: close cascades to map child", testCloseCascadesToMapChild),
    ("StateFlow: close cascades to filter child", testCloseCascadesToFilterChild),
    ("StateFlow: recursive cascading cancellation", testRecursiveCascadingCancellation),
    ("StateFlow: flush map child cascades to parent", testFlushMapChildCascadesToParent),
    ("StateFlow: flush filter child cascades to parent", testFlushFilterChildCascadesToParent),
    ("StateFlow: recursive cascading flush", testRecursiveCascadingFlush),
    ("StateFlow: combine receives from both parents", testCombineReceivesFromBothParents),
    ("StateFlow: combine close cascades from parent", testCombineCloseCascadesFromParent),
    ("StateFlow: combine flush cascades to parents", testCombineFlushCascadesToParents),
    ("StateFlow: derive with filtered initial value starts none", testDeriveWithFilteredInitialValueStartsNone),
    ("StateFlow: filter with filtered initial value starts none", testFilterWithFilteredInitialValueStartsNone)
  ]

end StateFlowTests
