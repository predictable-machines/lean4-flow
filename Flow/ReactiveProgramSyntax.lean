import Flow.ReactiveProgram

open Lean

declare_syntax_cat rctField
syntax ident " := " term ";" : rctField

syntax "`[RctProg|\n"
    withPosition((colGe rctField)*)
  "\n]" : term

private def findTerm
    (name : String)
    (fields : TSyntaxArray `rctField)
    : Option Term :=
  fields.find? (fun f => f.raw[0]? |>.map (·.getId.toString == name) |>.getD false)
  |>.bind (·.raw[2]?)
  |>.map (⟨·⟩)

private def requireTerm
    (name : String)
    (fields : TSyntaxArray `rctField)
    : MacroM Term := do
  let some term :=
    findTerm name fields
    | Macro.throwError s!"Missing required field {name}"
  pure term

private def findAllTerms
    (name : String)
    (fields : TSyntaxArray `rctField)
    : Array Term :=
  fields.filterMap fun f =>
    if f.raw[0]? |>.map (·.getId.toString == name) |>.getD false then
      f.raw[2]? |>.map (⟨·⟩)
    else
      none

private def knownFields : List String :=
  [ "initialState",
    "update",
    "sideEffect",
    "onUpdated",
    "onError",
    "firstMessage",
    "source",
    "onClose" ]

private def validateFieldNames (fields : TSyntaxArray `rctField) : MacroM Unit := do
  for field in fields do
    let some nameNode := field.raw[0]? | continue
    let name := nameNode.getId.toString
    unless knownFields.contains name do
      Macro.throwError s!"Unknown RctProg field '{name}'. Known fields: {knownFields}"

macro_rules
  | `(`[RctProg| $fields*]) => do
    validateFieldNames fields

    let initialState ← fields |> requireTerm "initialState"
    let update ← fields |> requireTerm "update"
    let sideEffect ← fields |> requireTerm "sideEffect"

    let onUpdated := fields |> findTerm "onUpdated" |>.getD (← `(fun _ => pure ()))
    let onError := fields |> findTerm "onError" |>.getD (← `(fun _ => none))
    let firstMessage := fields |> findTerm "firstMessage" |>.getD (← `(none))

    let sourceTerms := fields |> findAllTerms "source"
    let sourceExprs ← sourceTerms.mapM fun sourceTerm => do
      match sourceTerm with
      | `(($flow, $transform)) => `((Flow.Core.IOSubscribable.mapped $flow $transform : Flow.Core.IOSubscription _ _))
      | `($flow) => `((Flow.Core.IOSubscription.withExcept (($flow : Flow.Core.IOSubscription _ _)) : Flow.Core.IOSubscription _ _))

    let onClose := fields |> findAllTerms "onClose"

    `(ReactiveProgram.launchReactiveProgram
      { initialState := $initialState
        update := $update
        sideEffect := $sideEffect
        onUpdated := $onUpdated
        onError := $onError
        firstMessage := $firstMessage
        sources := #[$sourceExprs,*],
        onClose := #[$onClose,*] })
