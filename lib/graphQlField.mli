type t = GraphQlSelection.field

val make :
  ?alias:string ->
  ?directives:GraphQlDirective.t list ->
  name:string ->
  arguments:CliArgument.t list ->
  selectionSet:GraphQlSelection.t list ->
  unit ->
  t

val render : indentationLevel:int -> t -> string
