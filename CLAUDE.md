# lean4-flow

Reactive streams library for Lean 4 with Kotlin Coroutines Flow semantics.

## Origin

Extracted from [predictable-code](https://github.com/predictable-machines/predictable-code) monorepo (commit predictable-machines/predictable-code@d4c59fd, PR #262). Originally the `PredictableFlow` package, decoupled from `PredictableCore` and published as a standalone library.

## Build

Uses Lake build system, targeting Lean v4.28.0 (pinned in `lean-toolchain`).

```bash
make              # Build library
make test         # Build and run tests
make rebuild      # Clean and rebuild
```

## Architecture

```
Flow/
├── Core/          # Core types: Flow, SharedFlow, StateFlow, ProgramFlow, Collector, Flows typeclass
├── Builders/      # Builder functions: FlowBuilders, SharedFlowBuilders, StateFlowBuilders
├── Internal/      # Internal utilities: UUID generation, Program monad
├── ReactiveProgram.lean      # Reactive program definitions and execution
└── ReactiveProgramSyntax.lean # Macro syntax for reactive programs
```

### Stream Types

- **Flow** — Cold, lazy, unicast streams
- **SharedFlow** — Hot, multicast streams with replay buffer
- **StateFlow** — Hot, stateful streams with current value
- **ProgramFlow** — Hot streams in a `Program` monad context with thread-safe state
- **ReactiveProgram** — Declarative reactive program loop driven by source flows

### Flows Typeclass

Uniform `map`, `filter`, `filterMap`, `toList`, `subscribe`, `forEach` across all stream types.

## Module Organization

- Each folder has an `Index.lean` that re-exports all modules
- Use hierarchical imports: `import Flow.Core.Index`
- When adding new files: create file, add import to parent Index.lean

## Code Style

- Multi-line function signatures: each parameter on its own line with 4-space indent
- Braces on same lines as content
- No default parameter values
- Use plain `_` for discarded parameters
- Comments explain "why", not "what"

## Utility Scripts

See [UTILITY_SCRIPTS.md](UTILITY_SCRIPTS.md) for pre-approved read-only scripts in `.claude/scripts/` used by `/pr.review` and `/pr.create` commands.

## Pull Request Conventions

Include `Fixes #<issue-number>` on first line of PR body to auto-close issues.
