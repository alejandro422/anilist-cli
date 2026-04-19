type operationType = CommandLineInvocationTypes.operationType =
  | Query
  | Mutation
  | Subscription

type fieldSegment = CommandLineInvocationTypes.fieldSegment = {
  fieldName : string;
  fieldAlias : string option;
  fieldArgumentPairs : (string * string) list;
  fieldDirectiveTexts : string list;
}

type inlineFragmentSegment = CommandLineInvocationTypes.inlineFragmentSegment = {
  inlineFragmentTypeCondition : string;
  inlineFragmentDirectiveTexts : string list;
}

type fragmentSpreadSegment = CommandLineInvocationTypes.fragmentSpreadSegment = {
  fragmentSpreadName : string;
  fragmentSpreadDirectiveTexts : string list;
}

type selectionPathSegment = CommandLineInvocationTypes.selectionPathSegment =
  | FieldSegment of fieldSegment
  | InlineFragmentSegment of inlineFragmentSegment
  | FragmentSpreadSegment of fragmentSpreadSegment

type selectionBranch = CommandLineInvocationTypes.selectionBranch = {
  selectionPathSegments : selectionPathSegment list;
  selectionExpressions : string list;
}

type operationDefinition = CommandLineInvocationTypes.operationDefinition = {
  operationType : operationType;
  operationName : string option;
  variableDefinitions : string list;
  variableAssignments : (string * string) list;
  operationDirectiveTexts : string list;
  rootSelectionExpressions : string list;
  selectionBranches : selectionBranch list;
}

type structuredFragmentDefinition =
  CommandLineInvocationTypes.structuredFragmentDefinition = {
  fragmentName : string;
  fragmentTypeCondition : string;
  fragmentDirectiveTexts : string list;
  fragmentRootSelectionExpressions : string list;
  fragmentSelectionBranches : selectionBranch list;
}

type request = {
  operations : GraphQlOperation.t list;
  selectedOperationName : string option;
  loweredVariableAssignments : (string * CliArgument.value) list;
  fragmentDefinitions : GraphQlFragmentDefinition.t list;
}

val lower :
  operationDefinitions:operationDefinition list ->
  selectedOperationName:string option ->
  structuredFragmentDefinitions:structuredFragmentDefinition list ->
  rawFragmentDefinitionTexts:string list ->
  request

val graphQlQueryOfRequest : request -> GraphQlQuery.t
