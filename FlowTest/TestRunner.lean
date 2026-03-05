namespace TestRunner

inductive TestResult where
  | success : Nat → TestResult
  | failure : String → Nat → TestResult
  | timeout : Nat → TestResult

def TestResult.ms : TestResult → Nat
  | .success ms | .failure _ ms | .timeout ms => ms

-- Runs all tests, printing results and timing for each.
-- timeoutMs = 0 means no timeout.
def run (tests : List (String × IO Unit)) (timeoutMs : Nat := 50) : IO Unit := do
  let startTime ← IO.monoMsNow
  let mut results : List (String × TestResult) := []
  for (name, action) in tests do
    let t0 ← IO.monoMsNow
    IO.print s!"Running : {name}"
    (← IO.getStdout).flush
    -- .inl none = success, .inl (some msg) = test error, .inr () = timeout
    let promise : IO.Promise (Option String ⊕ Unit) ← IO.Promise.new
    let testTask ← IO.asTask (prio := .dedicated) do
      try
        action
        promise.resolve (.inl none)
      catch e =>
        promise.resolve (.inl (some (toString e)))
    if timeoutMs > 0 then
      discard <| IO.asTask (prio := .dedicated) do
        IO.sleep timeoutMs.toUInt32
        promise.resolve (.inr ())
    let result ← IO.wait promise.result!
    let ms ← IO.monoMsNow >>= fun t1 => pure (t1 - t0)
    match result with
    | .inr () => do
      IO.cancel testTask
      IO.print s!"\r\x1b[2KTIMEOUT : {name}  ⏱ ({ms}ms) [exceeded {timeoutMs}ms limit]\n"
      results := results ++ [(name, .timeout ms)]
      IO.Process.exit 1
    | .inl (some msg) =>
      IO.print s!"\r\x1b[2KFAIL    : {name}  ✗ ({ms}ms)\n"
      results := results ++ [(name, .failure msg ms)]
    | .inl none =>
      IO.print s!"\r\x1b[2KSuccess : {name}  ✓ ({ms}ms)\n"
      results := results ++ [(name, .success ms)]
    let result ← IO.wait testTask
    if !result.isOk then
      IO.Process.exit 1
  let elapsedMs ← IO.monoMsNow >>= fun now => pure (now - startTime)
  let failures := results.filter fun (_, r) => match r with | .failure .. | .timeout .. => true | _ => false
  IO.println ""
  if failures.isEmpty then
    IO.println s!"All {tests.length} tests passed in {elapsedMs}ms."
    let sorted := results.toArray.qsort (fun (_, a) (_, b) => a.ms > b.ms)
    let slowest := sorted.toList.take 5
    IO.println "\nSlowest tests:"
    for (name, r) in slowest do
      IO.println s!"  {r.ms}ms  {name}"
    IO.Process.exit 0
  else
    IO.println s!"{failures.length}/{tests.length} tests failed in {elapsedMs}ms."
    for (fname, r) in failures do
      match r with
      | .timeout ms => IO.println s!"  ⏱ {fname}: TIMEOUT (exceeded {timeoutMs}ms limit) ({ms}ms)"
      | .failure reason ms => IO.println s!"  ✗ {fname}: {reason} ({ms}ms)"
      | _ => pure ()
    IO.Process.exit 1

end TestRunner
