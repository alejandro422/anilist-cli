type field = {
  fieldAlias : string option;
  fieldName : string;
  fieldArguments : CliArgument.t list;
  fieldDirectives : string list;
  fieldSelectionSet : t list;
}

and fragmentSpread = {
  fragmentSpreadName : string;
  fragmentSpreadDirectives : string list;
}

and inlineFragment = {
  inlineFragmentTypeCondition : string option;
  inlineFragmentDirectives : string list;
  inlineFragmentSelectionSet : t list;
}

and t =
  | Field of field
  | FragmentSpread of fragmentSpread
  | InlineFragment of inlineFragment

val makeField :
  ?alias:string ->
  ?directives:string list ->
  name:string ->
  arguments:CliArgument.t list ->
  selectionSet:t list ->
  unit ->
  field

val makeFragmentSpread :
  ?directives:string list -> name:string -> unit -> fragmentSpread

val makeInlineFragment :
  ?typeCondition:string ->
  ?directives:string list ->
  selectionSet:t list ->
  unit ->
  inlineFragment

val field : field -> t
val fragmentSpread : fragmentSpread -> t
val inlineFragment : inlineFragment -> t
val render : indentationLevel:int -> t -> string
val renderDirectiveText : string -> string
val renderDirectives : string list -> string
