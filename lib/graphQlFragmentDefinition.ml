type structured = {
  name : string;
  typeCondition : string;
  directives : string list;
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

let renderStructured fragmentDefinition =
  let renderedSelectionSet =
    fragmentDefinition.selectionSet
    |> List.map (GraphQlSelection.render ~indentationLevel:1)
    |> String.concat "\n"
  in
  Printf.sprintf "fragment %s on %s%s {\n%s\n}" fragmentDefinition.name
    fragmentDefinition.typeCondition
    (GraphQlSelection.renderDirectives fragmentDefinition.directives)
    renderedSelectionSet

let render = function
  | Structured fragmentDefinition -> renderStructured fragmentDefinition
  | Raw text -> text
