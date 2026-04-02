val defaultEndpoint : string
val endpointOfEnvironment : unit -> string

val executeQuery :
  operationName:string option ->
  variables:Yojson.Safe.t option ->
  headers:(string * string) list ->
  endpoint:string ->
  query:string ->
  (int * string) Lwt.t

val prettyPrintedJsonOrOriginal : string -> string
