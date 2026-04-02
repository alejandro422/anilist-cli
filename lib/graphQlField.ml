type t = GraphQlSelection.field

let make ?alias ?(directives = []) ~name ~arguments ~selectionSet () =
  GraphQlSelection.makeField ?alias ~directives ~name ~arguments ~selectionSet
    ()

let render ~indentationLevel field =
  GraphQlSelection.render ~indentationLevel (GraphQlSelection.field field)
