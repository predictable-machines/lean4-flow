import Flow.Core.StateFlow

namespace Flow.Builders

open Flow.Core

/-! # StateFlow Builders - Creating hot stateful streams

Convenience functions for creating StateFlow instances.
The main creation function is `MutableStateFlow.create`.

## Examples

```lean
-- Integer state
let counter ← stateFlow 0

-- String state
let message ← stateFlow "Hello"

-- Complex state
structure AppState where
  count : Nat
  message : String

let appState ← stateFlow { count := 0, message := "Ready" }
```
-/

/-- Create a StateFlow with an initial value (alias for MutableStateFlow.create).

    Example:
    ```lean
    let flow ← stateFlow 42
    let v ← flow.value  -- 42
    ```
-/
unsafe def stateFlow (initialValue : α) : IO (MutableStateFlow α) :=
  MutableStateFlow.create initialValue

end Flow.Builders
