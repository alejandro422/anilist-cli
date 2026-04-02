open CommandLineInvocationTypes

val usageText : string
val helpRequestedOfArguments : string list -> bool
val selectionSetOptionName : string
val compatibilitySelectionSetOptionName : string
val fieldOptionName : string
val inlineFragmentOptionName : string
val fragmentSpreadOptionName : string
val fragmentSpreadCompatibilityOptionName : string
val fragmentOptionName : string
val operationOptionName : string
val operationNameOptionName : string
val selectedOperationNameOptionName : string
val variableDefinitionOptionName : string
val variableOptionName : string
val directiveOptionName : string
val aliasOptionName : string
val fragmentDefinitionOptionName : string
val currentBranchPathPrefix : string
val absoluteBranchPathPrefix : string
val isLongOption : string -> bool
val optionNameAndValueOfEqualsSyntax : string -> (string * string) option
val valueHasPrefix : prefix:string -> string -> bool
val valueWithoutPrefix : prefix:string -> string -> string
val normalizedAlias : string -> string
val operationTypeOfToken : string -> operationType option
val optionPairOfToken : string -> string list -> (string * string) * string list
val valueOfOptionToken : string -> string list -> string * string list
val variableAssignmentOfText : string -> string * string
val structuredFragmentNameAndTypeCondition : string -> string * string
val operationHeaderOfText : string -> operationType * string option
