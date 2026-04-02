type t = {
  headerPairs : (string * string) list;
  remainingArguments : string list;
}

val extractionOfArguments : string list -> (t, string) result
