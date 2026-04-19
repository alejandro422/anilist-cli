val execute :
  headers:(string * string) list ->
  endpoint:string ->
  SchemaCommandTypes.t ->
  (int * string) Lwt.t
