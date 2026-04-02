type operationType = Query | Mutation | Subscription

type fieldSegment = {
  fieldName : string;
  alias : string option;
  optionPairs : (string * string) list;
  fieldDirectives : string list;
}

type inlineFragmentSegment = {
  typeCondition : string;
  inlineFragmentDirectives : string list;
}

type fragmentSpreadSegment = {
  name : string;
  fragmentSpreadDirectives : string list;
}

type selectionPathSegment =
  | FieldSegment of fieldSegment
  | InlineFragmentSegment of inlineFragmentSegment
  | FragmentSpreadSegment of fragmentSpreadSegment

type selectionBranch = {
  selectionPathSegments : selectionPathSegment list;
  selectionExpressions : string list;
}

type operationDefinition = {
  operationType : operationType;
  operationName : string option;
  variableDefinitions : string list;
  operationVariableAssignments : (string * string) list;
  operationDirectives : string list;
  rootSelectionExpressions : string list;
  selectionBranches : selectionBranch list;
}

type structuredFragmentDefinition = {
  fragmentName : string;
  fragmentTypeCondition : string;
  fragmentDirectives : string list;
  fragmentRootSelectionExpressions : string list;
  fragmentSelectionBranches : selectionBranch list;
}

type request = {
  operations : GraphQlOperation.t list;
  selectedOperationName : string option;
  variableAssignments : (string * CliArgument.value) list;
  fragmentDefinitions : GraphQlFragmentDefinition.t list;
}

val lower :
  operationDefinitions:operationDefinition list ->
  selectedOperationName:string option ->
  structuredFragmentDefinitions:structuredFragmentDefinition list ->
  rawFragmentDefinitionTexts:string list ->
  request

val graphQlQueryOfRequest : request -> GraphQlQuery.t
