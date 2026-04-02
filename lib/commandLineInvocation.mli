type operationType = Query | Mutation | Subscription

type fieldSegment = {
  fieldName : string;
  fieldAlias : string option;
  fieldArgumentPairs : (string * string) list;
  fieldDirectiveTexts : string list;
}

type inlineFragmentSegment = {
  inlineFragmentTypeCondition : string;
  inlineFragmentDirectiveTexts : string list;
}

type fragmentSpreadSegment = {
  fragmentSpreadName : string;
  fragmentSpreadDirectiveTexts : string list;
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
  variableAssignments : (string * string) list;
  operationDirectiveTexts : string list;
  rootSelectionExpressions : string list;
  selectionBranches : selectionBranch list;
}

type structuredFragmentDefinition = {
  fragmentName : string;
  fragmentTypeCondition : string;
  fragmentDirectiveTexts : string list;
  fragmentRootSelectionExpressions : string list;
  fragmentSelectionBranches : selectionBranch list;
}

type t = {
  operationDefinitions : operationDefinition list;
  selectedOperationName : string option;
  structuredFragmentDefinitions : structuredFragmentDefinition list;
  rawFragmentDefinitionTexts : string list;
}

val usageText : string
val helpRequestedOfArguments : string list -> bool
val invocationOfArguments : string list -> (t, string) result
