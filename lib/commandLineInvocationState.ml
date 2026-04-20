open CommandLineInvocationTypes

let makeSelectionTargetBuilder () =
  {
    builderRootSelectionExpressions = [];
    builderCurrentSelectionBranch = None;
    builderSelectionBranches = [];
  }

let finalizedSelectionBranchOfBuilder selectionBranchBuilder =
  {
    selectionPathSegments =
      CommandLineInvocationBranch.orderedSelectionPathSegmentsOfBranchBuilder
        selectionBranchBuilder;
    selectionExpressions = selectionBranchBuilder.builderSelectionExpressions;
  }

let selectionTargetBuilderWithFinalizedBranch selectionTargetBuilder =
  match selectionTargetBuilder.builderCurrentSelectionBranch with
  | Some selectionBranchBuilder ->
      {
        selectionTargetBuilder with
        builderCurrentSelectionBranch = None;
        builderSelectionBranches =
          selectionTargetBuilder.builderSelectionBranches
          @ [ finalizedSelectionBranchOfBuilder selectionBranchBuilder ];
      }
  | None -> selectionTargetBuilder

let finalizedOperationDefinitionOfBuilder operationDefinitionBuilder =
  let finalizedSelectionTarget =
    selectionTargetBuilderWithFinalizedBranch
      operationDefinitionBuilder.builderOperationSelectionTarget
  in
  {
    operationType = operationDefinitionBuilder.builderOperationType;
    operationName = operationDefinitionBuilder.builderOperationName;
    variableDefinitions = operationDefinitionBuilder.builderVariableDefinitions;
    variableAssignments = operationDefinitionBuilder.builderVariableAssignments;
    operationDirectiveTexts =
      operationDefinitionBuilder.builderOperationDirectiveTexts;
    rootSelectionExpressions =
      finalizedSelectionTarget.builderRootSelectionExpressions;
    selectionBranches = finalizedSelectionTarget.builderSelectionBranches;
  }

let finalizedStructuredFragmentDefinitionOfBuilder fragmentDefinitionBuilder =
  let finalizedSelectionTarget =
    selectionTargetBuilderWithFinalizedBranch
      fragmentDefinitionBuilder.builderFragmentSelectionTarget
  in
  {
    fragmentName = fragmentDefinitionBuilder.builderFragmentName;
    fragmentTypeCondition =
      fragmentDefinitionBuilder.builderFragmentTypeCondition;
    fragmentDirectiveTexts =
      fragmentDefinitionBuilder.builderFragmentDirectiveTexts;
    fragmentRootSelectionExpressions =
      finalizedSelectionTarget.builderRootSelectionExpressions;
    fragmentSelectionBranches =
      finalizedSelectionTarget.builderSelectionBranches;
  }

let keepParserState parserState = parserState

let raiseMissingContext message _parserState =
  raise
    (Invalid_argument
       (Printf.sprintf "%s\n\n%s" message CommandLineInvocationShared.usageText))

let mapCurrentFragmentOrOperation ~onMissing ~mapFragment ~mapOperation
    parserState =
  match parserState.currentStructuredFragmentDefinition with
  | Some fragmentDefinitionBuilder ->
      {
        parserState with
        currentStructuredFragmentDefinition =
          Some (mapFragment fragmentDefinitionBuilder);
      }
  | None -> (
      match parserState.currentOperationDefinition with
      | Some operationDefinitionBuilder ->
          {
            parserState with
            currentOperationDefinition =
              Some (mapOperation operationDefinitionBuilder);
          }
      | None -> onMissing parserState)

let mapCurrentSelectionTarget ~onMissing parserState mapTarget =
  mapCurrentFragmentOrOperation ~onMissing parserState
    ~mapFragment:(fun fragmentDefinitionBuilder ->
      {
        fragmentDefinitionBuilder with
        builderFragmentSelectionTarget =
          mapTarget fragmentDefinitionBuilder.builderFragmentSelectionTarget;
      })
    ~mapOperation:(fun operationDefinitionBuilder ->
      {
        operationDefinitionBuilder with
        builderOperationSelectionTarget =
          mapTarget operationDefinitionBuilder.builderOperationSelectionTarget;
      })

let finalizedCurrentSelectionBranch parserState =
  mapCurrentSelectionTarget ~onMissing:keepParserState parserState
    selectionTargetBuilderWithFinalizedBranch

let finalizedCurrentStructuredFragmentDefinition parserState =
  match parserState.currentStructuredFragmentDefinition with
  | Some fragmentDefinitionBuilder ->
      let finalizedParserState = finalizedCurrentSelectionBranch parserState in
      {
        finalizedParserState with
        currentStructuredFragmentDefinition = None;
        finalizedStructuredFragmentDefinitions =
          finalizedParserState.finalizedStructuredFragmentDefinitions
          @ [
              finalizedStructuredFragmentDefinitionOfBuilder
                fragmentDefinitionBuilder;
            ];
      }
  | None -> parserState

let finalizedCurrentOperationDefinition parserState =
  match parserState.currentOperationDefinition with
  | Some operationDefinitionBuilder ->
      let finalizedParserState = finalizedCurrentSelectionBranch parserState in
      {
        finalizedParserState with
        currentOperationDefinition = None;
        finalizedOperationDefinitions =
          finalizedParserState.finalizedOperationDefinitions
          @ [ finalizedOperationDefinitionOfBuilder operationDefinitionBuilder ];
      }
  | None -> parserState

let missingSelectionContext =
  raiseMissingContext "Selections require an operation or fragment context."

let missingDirectiveContext =
  raiseMissingContext "Directives require an operation or fragment context."

let withUpdatedCurrentSelectionBranch parserState selectionBranchBuilder =
  mapCurrentSelectionTarget ~onMissing:missingSelectionContext parserState
    (fun selectionTarget ->
      {
        selectionTarget with
        builderCurrentSelectionBranch = Some selectionBranchBuilder;
      })

let withAddedRootSelectionExpression parserState selectionExpression =
  mapCurrentSelectionTarget ~onMissing:missingSelectionContext parserState
    (fun selectionTarget ->
      {
        selectionTarget with
        builderRootSelectionExpressions =
          selectionTarget.builderRootSelectionExpressions
          @ [ selectionExpression ];
      })

let withAddedCurrentTargetDirective parserState directiveText =
  mapCurrentFragmentOrOperation ~onMissing:missingDirectiveContext parserState
    ~mapFragment:(fun fragmentDefinitionBuilder ->
      {
        fragmentDefinitionBuilder with
        builderFragmentDirectiveTexts =
          fragmentDefinitionBuilder.builderFragmentDirectiveTexts
          @ [ directiveText ];
      })
    ~mapOperation:(fun operationDefinitionBuilder ->
      {
        operationDefinitionBuilder with
        builderOperationDirectiveTexts =
          operationDefinitionBuilder.builderOperationDirectiveTexts
          @ [ directiveText ];
      })

let finalizedAllPending parserState =
  parserState |> finalizedCurrentSelectionBranch
  |> finalizedCurrentStructuredFragmentDefinition
  |> finalizedCurrentOperationDefinition

let startedOperationDefinition parserState operationType operationName =
  let finalizedParserState = finalizedAllPending parserState in
  {
    finalizedParserState with
    currentOperationDefinition =
      Some
        {
          builderOperationType = operationType;
          builderOperationName = operationName;
          builderVariableDefinitions = [];
          builderVariableAssignments = [];
          builderOperationDirectiveTexts = [];
          builderOperationSelectionTarget = makeSelectionTargetBuilder ();
          builderDefaultSelectionPathPrefix = [];
        };
  }

let startedShorthandQueryOperation parserState firstFieldToken =
  let currentSelectionBranch =
    CommandLineInvocationBranch.makeSelectionBranchBuilderFromFieldPath
      firstFieldToken
  in
  let finalizedParserState = finalizedAllPending parserState in
  {
    finalizedParserState with
    currentOperationDefinition =
      Some
        {
          builderOperationType = Query;
          builderOperationName = None;
          builderVariableDefinitions = [];
          builderVariableAssignments = [];
          builderOperationDirectiveTexts = [];
          builderOperationSelectionTarget =
            {
              builderRootSelectionExpressions = [];
              builderCurrentSelectionBranch = Some currentSelectionBranch;
              builderSelectionBranches = [];
            };
          builderDefaultSelectionPathPrefix =
            CommandLineInvocationBranch
            .orderedSelectionPathSegmentsOfBranchBuilder currentSelectionBranch;
        };
  }

let startedStructuredFragmentDefinition parserState fragmentName
    fragmentTypeCondition =
  let finalizedParserState = finalizedAllPending parserState in
  {
    finalizedParserState with
    currentStructuredFragmentDefinition =
      Some
        {
          builderFragmentName = fragmentName;
          builderFragmentTypeCondition = fragmentTypeCondition;
          builderFragmentDirectiveTexts = [];
          builderFragmentSelectionTarget = makeSelectionTargetBuilder ();
        };
  }

let withUpdatedCurrentOperationDefinition parserState operationDefinitionBuilder
    =
  match parserState.currentOperationDefinition with
  | Some _ ->
      {
        parserState with
        currentOperationDefinition = Some operationDefinitionBuilder;
      }
  | None ->
      raise
        (Invalid_argument
           (Printf.sprintf
              "Operation options require a current operation.\n\n%s"
              CommandLineInvocationShared.usageText))

let initialParserState =
  {
    finalizedOperationDefinitions = [];
    currentOperationDefinition = None;
    requestedOperationName = None;
    finalizedStructuredFragmentDefinitions = [];
    currentStructuredFragmentDefinition = None;
    pendingRawFragmentDefinitionTexts = [];
  }

let currentInvocationOfState parserState =
  let finalizedParserState = finalizedAllPending parserState in
  {
    operationDefinitions = finalizedParserState.finalizedOperationDefinitions;
    selectedOperationName = finalizedParserState.requestedOperationName;
    structuredFragmentDefinitions =
      finalizedParserState.finalizedStructuredFragmentDefinitions;
    rawFragmentDefinitionTexts =
      finalizedParserState.pendingRawFragmentDefinitionTexts;
  }
