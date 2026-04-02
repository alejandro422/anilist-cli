type operationType = Query | Mutation | Subscription

type t = {
  operationType : operationType;
  name : string option;
  variableDefinitions : string list;
  directives : GraphQlDirective.t list;
  selectionSet : GraphQlSelection.t list;
}

let make ?name ?(variableDefinitions = []) ?(directives = []) ~operationType
    ~selectionSet () =
  { operationType; name; variableDefinitions; directives; selectionSet }

let renderOperationType = function
  | Query -> "query"
  | Mutation -> "mutation"
  | Subscription -> "subscription"

let renderVariableDefinitions variableDefinitions =
  match variableDefinitions with
  | [] -> ""
  | _ -> variableDefinitions |> String.concat ", " |> Printf.sprintf "(%s)"

let renderDirectives directives =
  match directives with
  | [] -> ""
  | _ ->
      directives
      |> List.map GraphQlDirective.render
      |> String.concat " " |> Printf.sprintf " %s"

let renderHeader operation =
  let renderedName =
    match operation.name with Some name -> " " ^ name | None -> ""
  in
  Printf.sprintf "%s%s%s%s"
    (renderOperationType operation.operationType)
    renderedName
    (renderVariableDefinitions operation.variableDefinitions)
    (renderDirectives operation.directives)
