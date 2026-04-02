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

val makeField :
  ?alias:string ->
  ?directives:GraphQlDirective.t list ->
  name:string ->
  arguments:CliArgument.t list ->
  selectionSet:t list ->
  unit ->
  field

val makeFragmentSpread :
  ?directives:GraphQlDirective.t list -> name:string -> unit -> fragmentSpread

val makeInlineFragment :
  ?typeCondition:string ->
  ?directives:GraphQlDirective.t list ->
  selectionSet:t list ->
  unit ->
  inlineFragment

val field : field -> t
val fragmentSpread : fragmentSpread -> t
val inlineFragment : inlineFragment -> t
val render : indentationLevel:int -> t -> string
