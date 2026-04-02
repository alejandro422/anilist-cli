val usageText : string
val helpRequestedOfArguments : string list -> bool

val invocationOfArguments :
  string list -> (SchemaCommandTypes.t option, string) result
