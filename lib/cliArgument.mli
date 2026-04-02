type value =
  | Null
  | Boolean of bool
  | Integer of int
  | Float of float
  | Enum of string
  | String of string
  | Variable of string
  | Json of Yojson.Safe.t

type t = { name : string; value : value }

val make : name:string -> rawValue:string -> t
val graphQlLiteralOfValue : value -> string
val jsonLiteralOfValue : value -> Yojson.Safe.t
