type structured = {
  name : string;
  typeCondition : string;
  directives : GraphQlDirective.t list;
  selectionSet : GraphQlSelection.t list;
}

type t = Structured of structured | Raw of string

let make ?(directives = []) ~name ~typeCondition ~selectionSet () =
  Structured { name; typeCondition; directives; selectionSet }

let makeRaw text =
  let trimmedText = String.trim text in
  if trimmedText = "" then
    invalid_arg "Fragment definition text cannot be empty"
  else Raw trimmedText

let renderDirectives directives =
  match directives with
  | [] -> ""
  | _ ->
      directives
      |> List.map GraphQlDirective.render
      |> String.concat " " |> Printf.sprintf " %s"

let renderStructured fragmentDefinition =
  let renderedSelectionSet =
    fragmentDefinition.selectionSet
    |> List.map (GraphQlSelection.render ~indentationLevel:1)
    |> String.concat "\n"
  in
  Printf.sprintf "fragment %s on %s%s {\n%s\n}" fragmentDefinition.name
    fragmentDefinition.typeCondition
    (renderDirectives fragmentDefinition.directives)
    renderedSelectionSet

let render = function
  | Structured fragmentDefinition -> renderStructured fragmentDefinition
  | Raw text -> text
