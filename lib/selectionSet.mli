type t = GraphQlSelection.t list
type scopedFieldSelection = { relativeSelectionSet : t; rootSelectionSet : t }

val empty : t
val merge : t -> t list -> t

val ofCliArguments :
  ?capitalizeRelativeRootFieldNames:bool ->
  ?capitalizeRootFieldNames:bool ->
  string list ->
  scopedFieldSelection
