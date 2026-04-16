open CommandLineInvocationTypes

val makeSelectionBranchBuilderFromFieldPath : string -> selectionBranchBuilder

val makeSelectionBranchBuilderFromSegments :
  selectionPathSegment list -> selectionBranchBuilder

val orderedSelectionPathSegmentsOfBranchBuilder :
  selectionBranchBuilder -> selectionPathSegment list

val withAddedBranchSelectionExpression :
  selectionBranchBuilder -> string -> selectionBranchBuilder

val withPushedFieldSelectionPathSegment :
  selectionBranchBuilder -> string -> selectionBranchBuilder

val withAddedBranchFieldArgumentPair :
  selectionBranchBuilder -> string * string -> selectionBranchBuilder

val withUpdatedBranchFieldAlias :
  selectionBranchBuilder -> string -> selectionBranchBuilder

val withAddedBranchDirective :
  selectionBranchBuilder -> string -> selectionBranchBuilder

val currentSelectionBranchOfState : parserState -> selectionBranchBuilder option
val currentDefaultSelectionPathPrefix : parserState -> selectionPathSegment list

val resolvedSelectionPathSegmentsOfFieldPath :
  parserState -> string -> selectionPathSegment list

val resolvedSelectionPathSegmentsOfInlineFragment :
  parserState -> string -> selectionPathSegment list

val resolvedSelectionPathSegmentsOfFragmentSpread :
  parserState -> string -> selectionPathSegment list
