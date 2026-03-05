import Flow

open Flow.Core Flow.Builders

/-- Create a SharedFlow pre-populated with values from a list -/
def sharedFlowOfGen (α : Type) (values : List α) : IO (SharedFlow α) := do
  let flow ← MutableSharedFlow.create (α := α) (replay := 64)
  for v in values do
    flow.emit v
  pure flow.toSharedFlow

/-- Create a SharedFlow of Nat values from a list -/
def sharedFlowOf (values : List Nat) : IO (SharedFlow Nat) :=
  sharedFlowOfGen Nat values
