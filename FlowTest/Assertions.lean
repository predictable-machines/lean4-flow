def Assert : Bool → String → IO Unit
  | true, _ => pure ()
  | false, msg => throw <| IO.userError msg

def AssertEqual [Repr α] [BEq α] : α → α → IO Unit
  | a, b => Assert (a == b) s!"{reprStr a} should be equal to {reprStr b}"

def shouldEqual
    [Repr α]
    [BEq α]
    (expected : α)
    (actual : α)
    : IO Unit :=
  AssertEqual actual expected

def shouldBeEmpty [Repr α] (list : List α) : IO Unit :=
  Assert list.isEmpty s!"Expected empty list, got {reprStr list}"

def shouldBeSome
    [Repr α]
    [BEq α]
    (expected : α)
    (actual : Option α)
    : IO Unit :=
  match actual with
  | some value => value |> shouldEqual expected
  | none => throw <| IO.userError s!"Expected some {reprStr expected}, got none"

def shouldBeOk
    [Repr ε]
    [Repr α]
    [BEq α]
    (expected : α)
    (result : Except ε α)
    : IO Unit :=
  match result with
  | .ok value => value |> shouldEqual expected
  | .error e => throw <| IO.userError s!"Expected Ok with {reprStr expected}, got error: {reprStr e}"

def AssertDifferent [Repr α] [BEq α] : α → α → IO Unit
  | a, b => Assert (a != b) s!"{reprStr a} should differ from {reprStr b}"

def AssertAll
    [Repr α]
    (items : List α)
    (check : α → Bool)
    (msg : String)
    : IO Unit := do
  let failed := items.filter (!check ·)
  if !failed.isEmpty then
    let failedStr := failed.map (reprStr ·) |>.foldl (· ++ "\n" ++ ·) ""
    throw <| IO.userError s!"{msg} ({failed.length} failed):\n{failedStr}"
