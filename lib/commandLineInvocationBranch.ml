open CommandLineInvocationTypes

let makeFieldSegment ?fieldAlias fieldName =
  { fieldName; fieldAlias; fieldArgumentPairs = []; fieldDirectiveTexts = [] }

let makeInlineFragmentSegment inlineFragmentTypeCondition =
  { inlineFragmentTypeCondition; inlineFragmentDirectiveTexts = [] }

let makeFragmentSpreadSegment fragmentSpreadName =
  { fragmentSpreadName; fragmentSpreadDirectiveTexts = [] }

let splitAliasAndFieldName pathSegment =
  match String.split_on_char ':' pathSegment with
  | [ fieldName ] -> (None, fieldName)
  | [ alias; fieldName ] -> (Some alias, fieldName)
  | _ ->
      raise
        (Invalid_argument
           (Printf.sprintf "Invalid field path segment %s\n\n%s" pathSegment
              CommandLineInvocationShared.usageText))

let fieldPathSegmentOfText pathSegmentText =
  let aliasValue, fieldName =
    splitAliasAndFieldName (String.trim pathSegmentText)
  in
  let trimmedFieldName = String.trim fieldName in
  if trimmedFieldName = "" then
    raise
      (Invalid_argument
         (Printf.sprintf "Field path cannot be empty.\n\n%s"
            CommandLineInvocationShared.usageText))
  else
    let fieldAlias =
      match aliasValue with
      | Some alias when String.trim alias <> "" ->
          Some (CommandLineInvocationShared.normalizedAlias (String.trim alias))
      | _ -> None
    in
    makeFieldSegment ?fieldAlias trimmedFieldName

let fieldSegmentsOfPath fieldPath =
  fieldPath |> String.split_on_char '.' |> List.map String.trim
  |> List.filter (fun segmentText -> segmentText <> "")
  |> List.map (fun segmentText ->
      FieldSegment (fieldPathSegmentOfText segmentText))

let orderedSelectionPathSegmentsOfBranchBuilder selectionBranchBuilder =
  List.rev
    (selectionBranchBuilder.builderCurrentSelectionPathSegment
   :: selectionBranchBuilder.builderPreviousSelectionPathSegments)

let branchBuilderEndsInFragmentSpread selectionBranchBuilder =
  match selectionBranchBuilder.builderCurrentSelectionPathSegment with
  | FragmentSpreadSegment _ -> true
  | FieldSegment _ | InlineFragmentSegment _ -> false

let makeSelectionBranchBuilderFromSegments selectionPathSegments =
  match List.rev selectionPathSegments with
  | [] ->
      raise
        (Invalid_argument
           (Printf.sprintf "Selection path cannot be empty.\n\n%s"
              CommandLineInvocationShared.usageText))
  | builderCurrentSelectionPathSegment :: builderPreviousSelectionPathSegments
    ->
      {
        builderCurrentSelectionPathSegment;
        builderPreviousSelectionPathSegments;
        builderSelectionExpressions = [];
      }

let makeSelectionBranchBuilderFromFieldPath fieldPath =
  fieldPath |> fieldSegmentsOfPath |> makeSelectionBranchBuilderFromSegments

let withAddedFieldArgumentPair fieldSegment fieldArgumentPair =
  {
    fieldSegment with
    fieldArgumentPairs = fieldSegment.fieldArgumentPairs @ [ fieldArgumentPair ];
  }

let withUpdatedFieldAlias fieldSegment fieldAlias =
  {
    fieldSegment with
    fieldAlias = Some (CommandLineInvocationShared.normalizedAlias fieldAlias);
  }

let withAddedFieldDirective fieldSegment fieldDirectiveText =
  {
    fieldSegment with
    fieldDirectiveTexts =
      fieldSegment.fieldDirectiveTexts @ [ fieldDirectiveText ];
  }

let withAddedInlineFragmentDirective inlineFragmentSegment fieldDirectiveText =
  {
    inlineFragmentSegment with
    inlineFragmentDirectiveTexts =
      inlineFragmentSegment.inlineFragmentDirectiveTexts
      @ [ fieldDirectiveText ];
  }

let withAddedFragmentSpreadDirective fragmentSpreadSegment fieldDirectiveText =
  {
    fragmentSpreadSegment with
    fragmentSpreadDirectiveTexts =
      fragmentSpreadSegment.fragmentSpreadDirectiveTexts
      @ [ fieldDirectiveText ];
  }

let withAddedBranchSelectionExpression selectionBranchBuilder
    selectionExpression =
  if branchBuilderEndsInFragmentSpread selectionBranchBuilder then
    raise
      (Invalid_argument
         (Printf.sprintf "Fragment spreads cannot define a selection set.\n\n%s"
            CommandLineInvocationShared.usageText))
  else
    {
      selectionBranchBuilder with
      builderSelectionExpressions =
        selectionBranchBuilder.builderSelectionExpressions
        @ [ selectionExpression ];
    }

let withPushedFieldSelectionPathSegment selectionBranchBuilder fieldName =
  if branchBuilderEndsInFragmentSpread selectionBranchBuilder then
    raise
      (Invalid_argument
         (Printf.sprintf
            "Fragment spreads cannot contain child selections.\n\n%s"
            CommandLineInvocationShared.usageText))
  else
    match List.rev (fieldSegmentsOfPath fieldName) with
    | [] ->
        raise
          (Invalid_argument
             (Printf.sprintf "Selection path cannot be empty.\n\n%s"
                CommandLineInvocationShared.usageText))
    | builderCurrentSelectionPathSegment :: builderPreviousSelectionPathSegments
      ->
        {
          builderCurrentSelectionPathSegment;
          builderPreviousSelectionPathSegments =
            builderPreviousSelectionPathSegments
            @ [ selectionBranchBuilder.builderCurrentSelectionPathSegment ]
            @ selectionBranchBuilder.builderPreviousSelectionPathSegments;
          builderSelectionExpressions =
            selectionBranchBuilder.builderSelectionExpressions;
        }

let withAddedBranchFieldArgumentPair selectionBranchBuilder fieldArgumentPair =
  match selectionBranchBuilder.builderCurrentSelectionPathSegment with
  | FieldSegment fieldSegment ->
      {
        selectionBranchBuilder with
        builderCurrentSelectionPathSegment =
          FieldSegment
            (withAddedFieldArgumentPair fieldSegment fieldArgumentPair);
      }
  | InlineFragmentSegment _ | FragmentSpreadSegment _ ->
      raise
        (Invalid_argument
           (Printf.sprintf
              "Field arguments require the current selection branch to end in \
               a field.\n\n\
               %s"
              CommandLineInvocationShared.usageText))

let withUpdatedBranchFieldAlias selectionBranchBuilder fieldAlias =
  match selectionBranchBuilder.builderCurrentSelectionPathSegment with
  | FieldSegment fieldSegment ->
      {
        selectionBranchBuilder with
        builderCurrentSelectionPathSegment =
          FieldSegment (withUpdatedFieldAlias fieldSegment fieldAlias);
      }
  | InlineFragmentSegment _ | FragmentSpreadSegment _ ->
      raise
        (Invalid_argument
           (Printf.sprintf
              "--alias requires the current selection branch to end in a \
               field.\n\n\
               %s"
              CommandLineInvocationShared.usageText))

let withAddedBranchDirective selectionBranchBuilder fieldDirectiveText =
  match selectionBranchBuilder.builderCurrentSelectionPathSegment with
  | FieldSegment fieldSegment ->
      {
        selectionBranchBuilder with
        builderCurrentSelectionPathSegment =
          FieldSegment (withAddedFieldDirective fieldSegment fieldDirectiveText);
      }
  | InlineFragmentSegment inlineFragmentSegment ->
      {
        selectionBranchBuilder with
        builderCurrentSelectionPathSegment =
          InlineFragmentSegment
            (withAddedInlineFragmentDirective inlineFragmentSegment
               fieldDirectiveText);
      }
  | FragmentSpreadSegment fragmentSpreadSegment ->
      {
        selectionBranchBuilder with
        builderCurrentSelectionPathSegment =
          FragmentSpreadSegment
            (withAddedFragmentSpreadDirective fragmentSpreadSegment
               fieldDirectiveText);
      }

let currentSelectionBranchOfState parserState =
  match parserState.currentStructuredFragmentDefinition with
  | Some fragmentDefinitionBuilder ->
      fragmentDefinitionBuilder.builderFragmentSelectionTarget
        .builderCurrentSelectionBranch
  | None -> (
      match parserState.currentOperationDefinition with
      | Some operationDefinitionBuilder ->
          operationDefinitionBuilder.builderOperationSelectionTarget
            .builderCurrentSelectionBranch
      | None -> None)

let currentDefaultSelectionPathPrefix parserState =
  match parserState.currentStructuredFragmentDefinition with
  | Some _ -> []
  | None -> (
      match parserState.currentOperationDefinition with
      | Some operationDefinitionBuilder ->
          operationDefinitionBuilder.builderDefaultSelectionPathPrefix
      | None -> [])

let ensureSelectionPathCanAcceptChildren selectionPathSegments =
  match List.rev selectionPathSegments with
  | FragmentSpreadSegment _ :: _ ->
      raise
        (Invalid_argument
           (Printf.sprintf
              "Fragment spreads cannot contain child selections.\n\n%s"
              CommandLineInvocationShared.usageText))
  | FieldSegment _ :: _ | InlineFragmentSegment _ :: _ | [] ->
      selectionPathSegments

let resolvedSelectionPathSegmentsOfFieldPath parserState fieldPath =
  let currentSelectionBranch = currentSelectionBranchOfState parserState in
  let defaultSelectionPathPrefix =
    currentDefaultSelectionPathPrefix parserState
  in
  if
    CommandLineInvocationShared.valueHasPrefix
      ~prefix:CommandLineInvocationShared.currentBranchPathPrefix fieldPath
  then
    let relativeFieldPath =
      CommandLineInvocationShared.valueWithoutPrefix
        ~prefix:CommandLineInvocationShared.currentBranchPathPrefix fieldPath
    in
    let relativeSelectionPathSegments = fieldSegmentsOfPath relativeFieldPath in
    if relativeSelectionPathSegments = [] then
      raise
        (Invalid_argument
           (Printf.sprintf "Selection path cannot be empty.\n\n%s"
              CommandLineInvocationShared.usageText))
    else
      match currentSelectionBranch with
      | Some selectionBranchBuilder ->
          orderedSelectionPathSegmentsOfBranchBuilder selectionBranchBuilder
          |> ensureSelectionPathCanAcceptChildren
          |> fun parentSelectionPathSegments ->
          parentSelectionPathSegments @ relativeSelectionPathSegments
      | None ->
          raise
            (Invalid_argument
               (Printf.sprintf
                  "Relative --field paths require an existing selection \
                   branch.\n\n\
                   %s"
                  CommandLineInvocationShared.usageText))
  else if
    CommandLineInvocationShared.valueHasPrefix
      ~prefix:CommandLineInvocationShared.absoluteBranchPathPrefix fieldPath
  then
    let absoluteFieldPath =
      CommandLineInvocationShared.valueWithoutPrefix
        ~prefix:CommandLineInvocationShared.absoluteBranchPathPrefix fieldPath
    in
    let absoluteSelectionPathSegments = fieldSegmentsOfPath absoluteFieldPath in
    if absoluteSelectionPathSegments = [] then
      raise
        (Invalid_argument
           (Printf.sprintf "Selection path cannot be empty.\n\n%s"
              CommandLineInvocationShared.usageText))
    else absoluteSelectionPathSegments
  else
    let relativeSelectionPathSegments = fieldSegmentsOfPath fieldPath in
    if relativeSelectionPathSegments = [] then
      raise
        (Invalid_argument
           (Printf.sprintf "Selection path cannot be empty.\n\n%s"
              CommandLineInvocationShared.usageText))
    else defaultSelectionPathPrefix @ relativeSelectionPathSegments

let resolvedSelectionPathSegmentsOfInlineFragment parserState
    inlineFragmentTypeConditionText =
  let currentSelectionBranch = currentSelectionBranchOfState parserState in
  let parentSelectionPathSegments =
    if
      CommandLineInvocationShared.valueHasPrefix
        ~prefix:CommandLineInvocationShared.currentBranchPathPrefix
        inlineFragmentTypeConditionText
    then
      match currentSelectionBranch with
      | Some selectionBranchBuilder ->
          orderedSelectionPathSegmentsOfBranchBuilder selectionBranchBuilder
          |> ensureSelectionPathCanAcceptChildren
      | None ->
          raise
            (Invalid_argument
               (Printf.sprintf
                  "Relative --inline-fragment paths require an existing \
                   selection branch.\n\n\
                   %s"
                  CommandLineInvocationShared.usageText))
    else if
      CommandLineInvocationShared.valueHasPrefix
        ~prefix:CommandLineInvocationShared.absoluteBranchPathPrefix
        inlineFragmentTypeConditionText
    then []
    else
      match currentSelectionBranch with
      | Some selectionBranchBuilder ->
          orderedSelectionPathSegmentsOfBranchBuilder selectionBranchBuilder
          |> ensureSelectionPathCanAcceptChildren
      | None -> currentDefaultSelectionPathPrefix parserState
  in
  let typeConditionText =
    if
      CommandLineInvocationShared.valueHasPrefix
        ~prefix:CommandLineInvocationShared.currentBranchPathPrefix
        inlineFragmentTypeConditionText
    then
      CommandLineInvocationShared.valueWithoutPrefix
        ~prefix:CommandLineInvocationShared.currentBranchPathPrefix
        inlineFragmentTypeConditionText
    else if
      CommandLineInvocationShared.valueHasPrefix
        ~prefix:CommandLineInvocationShared.absoluteBranchPathPrefix
        inlineFragmentTypeConditionText
    then
      CommandLineInvocationShared.valueWithoutPrefix
        ~prefix:CommandLineInvocationShared.absoluteBranchPathPrefix
        inlineFragmentTypeConditionText
    else inlineFragmentTypeConditionText
  in
  let normalizedTypeCondition = String.trim typeConditionText in
  if normalizedTypeCondition = "" then
    raise
      (Invalid_argument
         (Printf.sprintf "Inline fragments require a type condition.\n\n%s"
            CommandLineInvocationShared.usageText))
  else
    parentSelectionPathSegments
    @ [
        InlineFragmentSegment
          (makeInlineFragmentSegment normalizedTypeCondition);
      ]

let resolvedSelectionPathSegmentsOfFragmentSpread parserState fragmentSpreadText
    =
  let currentSelectionBranch = currentSelectionBranchOfState parserState in
  let parentSelectionPathSegments, fragmentSpreadName =
    if
      CommandLineInvocationShared.valueHasPrefix
        ~prefix:CommandLineInvocationShared.currentBranchPathPrefix
        fragmentSpreadText
    then
      match currentSelectionBranch with
      | Some selectionBranchBuilder ->
          ( orderedSelectionPathSegmentsOfBranchBuilder selectionBranchBuilder
            |> ensureSelectionPathCanAcceptChildren,
            CommandLineInvocationShared.valueWithoutPrefix
              ~prefix:CommandLineInvocationShared.currentBranchPathPrefix
              fragmentSpreadText )
      | None ->
          raise
            (Invalid_argument
               (Printf.sprintf
                  "Relative --fragment-spread paths require an existing \
                   selection branch.\n\n\
                   %s"
                  CommandLineInvocationShared.usageText))
    else if
      CommandLineInvocationShared.valueHasPrefix
        ~prefix:CommandLineInvocationShared.absoluteBranchPathPrefix
        fragmentSpreadText
    then
      ( [],
        CommandLineInvocationShared.valueWithoutPrefix
          ~prefix:CommandLineInvocationShared.absoluteBranchPathPrefix
          fragmentSpreadText )
    else
      match currentSelectionBranch with
      | Some selectionBranchBuilder ->
          ( orderedSelectionPathSegmentsOfBranchBuilder selectionBranchBuilder
            |> ensureSelectionPathCanAcceptChildren,
            fragmentSpreadText )
      | None ->
          (currentDefaultSelectionPathPrefix parserState, fragmentSpreadText)
  in
  let normalizedFragmentSpreadName = String.trim fragmentSpreadName in
  if normalizedFragmentSpreadName = "" then
    raise
      (Invalid_argument
         (Printf.sprintf "Fragment spreads require a fragment name.\n\n%s"
            CommandLineInvocationShared.usageText))
  else
    parentSelectionPathSegments
    @ [
        FragmentSpreadSegment
          (makeFragmentSpreadSegment normalizedFragmentSpreadName);
      ]
