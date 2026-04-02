type t = {
  operationName : string option;
  queryText : string;
  variables : Yojson.Safe.t option;
  requestedDirectiveName : string option;
}

val usageText : string
val helpRequestedOfArguments : string list -> bool
val invocationOfArguments : string list -> (t option, string) result

val execute :
  headers:(string * string) list -> endpoint:string -> t -> (int * string) Lwt.t
