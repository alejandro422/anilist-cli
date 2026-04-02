type t = {
  operations : GraphQlOperation.t list;
  fragmentDefinitions : GraphQlFragmentDefinition.t list;
}

val make :
  operations:GraphQlOperation.t list ->
  fragmentDefinitions:GraphQlFragmentDefinition.t list ->
  unit ->
  t

val render : t -> string
