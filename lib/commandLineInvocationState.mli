open CommandLineInvocationTypes

val initialParserState : parserState
val finalizedCurrentSelectionBranch : parserState -> parserState
val finalizedCurrentStructuredFragmentDefinition : parserState -> parserState
val finalizedCurrentOperationDefinition : parserState -> parserState

val withUpdatedCurrentSelectionBranch :
  parserState -> selectionBranchBuilder -> parserState

val withAddedRootSelectionExpression : parserState -> string -> parserState
val withAddedCurrentTargetDirective : parserState -> string -> parserState

val startedOperationDefinition :
  parserState -> operationType -> string option -> parserState

val startedShorthandQueryOperation : parserState -> string -> parserState

val startedStructuredFragmentDefinition :
  parserState -> string -> string -> parserState

val withUpdatedCurrentOperationDefinition :
  parserState -> operationDefinitionBuilder -> parserState

val currentInvocationOfState : parserState -> t
