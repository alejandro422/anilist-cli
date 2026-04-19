type structured = {
  name : string;
  typeCondition : string;
  directives : string list;
  selectionSet : GraphQlSelection.t list;
}

type t = Structured of structured | Raw of string

val make :
  ?directives:string list ->
  name:string ->
  typeCondition:string ->
  selectionSet:GraphQlSelection.t list ->
  unit ->
  t

val makeRaw : string -> t
val render : t -> string
