import PredictableFlow.Core.Index

namespace Flow.Builders

open Flow.Core

/-! # Flow Builders - Constructing cold streams

Provides convenient constructors for creating Flow instances from
various sources (lists, arrays, single values, or custom emission logic).

## Examples

```lean
-- From a list
let flow1 := flowOf [1, 2, 3]

-- Empty flow
let flow2 := emptyFlow

-- Single value
let flow3 := flowOf [42]

-- Custom emission logic
let flow4 := flow fun collector => do
    collector.emit "Hello"
    collector.emit "World"
```
-/

/-- Create an empty flow that completes immediately -/
def emptyFlow : Flow α :=
  Flow.mk fun _ => pure ()

/-- Create a flow from a single value -/
def just (value : α) : Flow α :=
  Flow.mk fun collector => collector.emit value

/-- Create a flow from a list of values -/
def flowOf (items : List α) : Flow α :=
  Flow.mk fun collector => do
    for item in items do
      collector.emit item

/-- Create a flow from an array of values -/
def flowOfArray (items : Array α) : Flow α :=
  flowOf items.toList

/-- Flow builder DSL - create a flow with custom emission logic.

    Example:
    ```lean
    let myFlow := flow fun collector => do
        collector.emit 1
        IO.println "Between emissions"
        collector.emit 2
    ```
-/
def flow (block : Collector α → IO Unit) : Flow α :=
  Flow.mk block

/-- Create a flow that emits values from start to end (inclusive) -/
def range (start : Nat) (endInclusive : Nat) : Flow Nat :=
  Flow.mk fun collector => do
    for i in [start:endInclusive+1] do
      collector.emit i

/-- Create a flow that repeats a value n times -/
def replicate (value : α) (count : Nat) : Flow α :=
  Flow.mk fun collector => do
    for _ in [:count] do
      collector.emit value

end Flow.Builders
