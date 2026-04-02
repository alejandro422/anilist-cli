type field = {
  fieldAlias : string option;
  fieldName : string;
  fieldArguments : CliArgument.t list;
  fieldDirectives : GraphQlDirective.t list;
  fieldSelectionSet : t list;
}

and fragmentSpread = {
  fragmentSpreadName : string;
  fragmentSpreadDirectives : GraphQlDirective.t list;
}

and inlineFragment = {
  inlineFragmentTypeCondition : string option;
  inlineFragmentDirectives : GraphQlDirective.t list;
  inlineFragmentSelectionSet : t list;
}

and t =
  | Field of field
  | FragmentSpread of fragmentSpread
  | InlineFragment of inlineFragment

let makeField ?alias ?(directives = []) ~name ~arguments ~selectionSet () =
  {
    fieldAlias = alias;
    fieldName = name;
    fieldArguments = arguments;
    fieldDirectives = directives;
    fieldSelectionSet = selectionSet;
  }

let makeFragmentSpread ?(directives = []) ~name () =
  { fragmentSpreadName = name; fragmentSpreadDirectives = directives }

let makeInlineFragment ?typeCondition ?(directives = []) ~selectionSet () =
  {
    inlineFragmentTypeCondition = typeCondition;
    inlineFragmentDirectives = directives;
    inlineFragmentSelectionSet = selectionSet;
  }

let field value = Field value
let fragmentSpread value = FragmentSpread value
let inlineFragment value = InlineFragment value
let indentation indentationLevel = String.make (indentationLevel * 2) ' '

let renderDirectives directives =
  match directives with
  | [] -> ""
  | _ ->
      directives
      |> List.map GraphQlDirective.render
      |> String.concat " " |> Printf.sprintf " %s"

let renderArguments arguments =
  match arguments with
  | [] -> ""
  | _ ->
      arguments
      |> List.map (fun argument ->
          Printf.sprintf "%s: %s" argument.CliArgument.name
            (CliArgument.graphQlLiteralOfValue argument.CliArgument.value))
      |> String.concat ", " |> Printf.sprintf "(%s)"

let renderAlias = function Some alias -> alias ^ ": " | None -> ""

let rec renderField ~indentationLevel field =
  let fieldHeader =
    Printf.sprintf "%s%s%s%s%s"
      (indentation indentationLevel)
      (renderAlias field.fieldAlias)
      field.fieldName
      (renderArguments field.fieldArguments)
      (renderDirectives field.fieldDirectives)
  in
  match field.fieldSelectionSet with
  | [] -> fieldHeader
  | _ ->
      let renderedSelectionSet =
        field.fieldSelectionSet
        |> List.map (render ~indentationLevel:(indentationLevel + 1))
        |> String.concat "\n"
      in
      Printf.sprintf "%s {\n%s\n%s}" fieldHeader renderedSelectionSet
        (indentation indentationLevel)

and renderFragmentSpread ~indentationLevel fragmentSpread =
  Printf.sprintf "%s...%s%s"
    (indentation indentationLevel)
    fragmentSpread.fragmentSpreadName
    (renderDirectives fragmentSpread.fragmentSpreadDirectives)

and renderInlineFragment ~indentationLevel inlineFragment =
  let typeCondition =
    match inlineFragment.inlineFragmentTypeCondition with
    | Some typeCondition -> " on " ^ typeCondition
    | None -> ""
  in
  let header =
    Printf.sprintf "%s...%s%s"
      (indentation indentationLevel)
      typeCondition
      (renderDirectives inlineFragment.inlineFragmentDirectives)
  in
  match inlineFragment.inlineFragmentSelectionSet with
  | [] -> header
  | _ ->
      let renderedSelectionSet =
        inlineFragment.inlineFragmentSelectionSet
        |> List.map (render ~indentationLevel:(indentationLevel + 1))
        |> String.concat "\n"
      in
      Printf.sprintf "%s {\n%s\n%s}" header renderedSelectionSet
        (indentation indentationLevel)

and render ~indentationLevel = function
  | Field field -> renderField ~indentationLevel field
  | FragmentSpread fragmentSpread ->
      renderFragmentSpread ~indentationLevel fragmentSpread
  | InlineFragment inlineFragment ->
      renderInlineFragment ~indentationLevel inlineFragment
