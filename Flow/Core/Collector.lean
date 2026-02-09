namespace Flow.Core

/-! # Collector - Flow emission infrastructure

A Collector receives values emitted by a Flow. It provides the `emit` function
that Flow builders use to send values downstream to collectors.

This is analogous to Kotlin's FlowCollector interface.
-/

/-- Collector receives values emitted by a Flow -/
structure Collector (α : Type) where
  /-- Emit a value downstream -/
  emit : α → IO Unit

namespace Collector

/-- Create a simple collector from an action -/
def fromAction (action : α → IO Unit) : Collector α :=
  { emit := action }

/-- Create a collector that does nothing (useful for testing) -/
def noop : Collector α :=
  { emit := fun _ => pure () }

/-- Create a collector that prints values -/
def print [ToString α] : Collector α :=
  { emit := fun x => IO.println (toString x) }

end Collector

end Flow.Core
