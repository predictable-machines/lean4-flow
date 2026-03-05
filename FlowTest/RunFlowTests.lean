import FlowTest.TestRunner
import FlowTest.Core.Index

def main : IO Unit := do
  IO.println "Running Flow tests..."
  TestRunner.run
    CoreTests.allTests
