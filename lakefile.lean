import Lake
open Lake DSL

package flow where
  version := v!"0.2.1"

@[default_target]
lean_lib Flow where
  roots := #[`Flow]

lean_lib FlowTest where
  roots := #[`FlowTest]

lean_exe tests where
  root := `FlowTest.RunFlowTests
