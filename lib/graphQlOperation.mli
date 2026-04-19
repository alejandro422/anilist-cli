type operationType = Query | Mutation | Subscription

type t = {
  operationType : operationType;
  name : string option;
  variableDefinitions : string list;
  directives : string list;
  selectionSet : GraphQlSelection.t list;
}

val make :
  ?name:string ->
  ?variableDefinitions:string list ->
  ?directives:string list ->
  operationType:operationType ->
  selectionSet:GraphQlSelection.t list ->
  unit ->
  t

val renderHeader : t -> string
