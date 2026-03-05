import Flow.Core.SharedFlow

namespace Flow.Builders

open Flow.Core

/-! # SharedFlow Builders - Creating hot multicast streams

Convenience functions for creating SharedFlow instances.
The main creation function is `MutableSharedFlow.create`.

## Examples

```lean
-- Basic SharedFlow
let flow1 ← MutableSharedFlow.create (α := Int)

-- With replay buffer
let flow2 ← MutableSharedFlow.create (replay := 2)

-- With custom buffer size and overflow strategy
let flow3 ← MutableSharedFlow.create
  (replay := 1)
  (bufferSize := 32)
  (onBufferOverflow := .dropLatest)
```
-/

/-- Create a SharedFlow with default configuration.

    Example:
    ```lean
    let flow ← sharedFlow (α := String)
    ```
-/
def sharedFlow (α : Type) : IO (MutableSharedFlow α) :=
  MutableSharedFlow.create

/-- Create a SharedFlow with replay buffer.

    Example:
    ```lean
    let flow ← sharedFlowWithReplay 2 (α := Int)
    let subscription ← flow.subscribe IO.println
    flow.emit 1
    flow.emit 2
    flow.emit 3
    -- New subscriber will receive [2, 3] (last 2 values)
    let subscription2 ← flow.subscribe (fun x => IO.println s!"New: {x}")
    ```
-/
def sharedFlowWithReplay (replay : Nat) (α : Type) : IO (MutableSharedFlow α) :=
  MutableSharedFlow.create (replay := replay)

end Flow.Builders
