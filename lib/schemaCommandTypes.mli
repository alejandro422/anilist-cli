type t = {
  operationName : string option;
  queryText : string;
  variables : Yojson.Safe.t option;
  requestedDirectiveName : string option;
}

val fullSchemaCommand : t
val typeCommand : string -> t
val directiveCommand : string -> t
