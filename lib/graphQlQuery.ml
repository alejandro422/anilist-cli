type t = {
  operations : GraphQlOperation.t list;
  fragmentDefinitions : GraphQlFragmentDefinition.t list;
}

let make ~operations ~fragmentDefinitions () =
  { operations; fragmentDefinitions }

let renderOperation operation =
  let renderedSelectionSet =
    operation.GraphQlOperation.selectionSet
    |> List.map (GraphQlSelection.render ~indentationLevel:1)
    |> String.concat "\n"
  in
  Printf.sprintf "%s {\n%s\n}"
    (GraphQlOperation.renderHeader operation)
    renderedSelectionSet

let render query =
  let renderedOperations =
    query.operations |> List.map renderOperation |> String.concat "\n\n"
  in
  match query.fragmentDefinitions with
  | [] -> renderedOperations
  | _ ->
      let renderedFragments =
        query.fragmentDefinitions
        |> List.map GraphQlFragmentDefinition.render
        |> String.concat "\n\n"
      in
      Printf.sprintf "%s\n\n%s" renderedOperations renderedFragments
