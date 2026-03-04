# lean4-flow

Reactive streams library for Lean 4 with Kotlin Coroutines Flow semantics.

## Features

- **Flow** — Cold, lazy, unicast streams
- **SharedFlow** — Hot, multicast streams with replay buffer
- **StateFlow** — Hot, stateful streams with current value
- **ProgramFlow** — Hot streams in a `Program` monad context with thread-safe state
- **ReactiveProgram** — Declarative reactive program loop driven by source flows
- **Flows typeclass** — Uniform `map`, `filter`, `filterMap`, `toList`, `subscribe`, `forEach` across all stream types

## Installation

Add to your `lakefile.lean`:

```lean
require flow from git
  "https://github.com/predictable-machines/lean4-flow" @ "v0.1.0"
```

## Quick Start

```lean
import Flow

open Flow.Core Flow.Builders

-- Cold Flow: lazy, unicast
let flow := flowOf [1, 2, 3, 4, 5]
let mapped ← Flows.map flow (· * 2)
let result ← Flows.toList mapped
-- result: [2, 4, 6, 8, 10]

-- Hot SharedFlow: multicast to multiple subscribers
let shared ← MutableSharedFlow.create (replay := 2)
let cancel ← shared.subscribe IO.println
shared.emit 42  -- subscriber receives 42
cancel          -- stop subscribing

-- Hot StateFlow: current value with change notifications
let state ← MutableStateFlow.create (some 0)
let cancel ← state.subscribe IO.println
state.emit 100  -- subscriber receives 100
```

## License

MIT
