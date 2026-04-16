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

let finalizedCurrentSelectionBranch parserState =
  match parserState.currentStructuredFragmentDefinition with
  | Some fragmentDefinitionBuilder ->
      {
        parserState with
        currentStructuredFragmentDefinition =
          Some
            {
              fragmentDefinitionBuilder with
              builderFragmentSelectionTarget =
                selectionTargetBuilderWithFinalizedBranch
                  fragmentDefinitionBuilder.builderFragmentSelectionTarget;
            };
      }
  | None -> (
      match parserState.currentOperationDefinition with
      | Some operationDefinitionBuilder ->
          {
            parserState with
            currentOperationDefinition =
              Some
                {
                  operationDefinitionBuilder with
                  builderOperationSelectionTarget =
                    selectionTargetBuilderWithFinalizedBranch
                      operationDefinitionBuilder.builderOperationSelectionTarget;
                };
          }
      | None -> parserState)

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

let withUpdatedCurrentSelectionBranch parserState selectionBranchBuilder =
  match parserState.currentStructuredFragmentDefinition with
  | Some fragmentDefinitionBuilder ->
      {
        parserState with
        currentStructuredFragmentDefinition =
          Some
            {
              fragmentDefinitionBuilder with
              builderFragmentSelectionTarget =
                {
                  fragmentDefinitionBuilder.builderFragmentSelectionTarget with
                  builderCurrentSelectionBranch = Some selectionBranchBuilder;
                };
            };
      }
  | None -> (
      match parserState.currentOperationDefinition with
      | Some operationDefinitionBuilder ->
          {
            parserState with
            currentOperationDefinition =
              Some
                {
                  operationDefinitionBuilder with
                  builderOperationSelectionTarget =
                    {
                      operationDefinitionBuilder.builderOperationSelectionTarget with
                      builderCurrentSelectionBranch =
                        Some selectionBranchBuilder;
                    };
                };
          }
      | None ->
          raise
            (Invalid_argument
               (Printf.sprintf
                  "Selections require an operation or fragment context.\n\n%s"
                  CommandLineInvocationShared.usageText)))

let withAddedRootSelectionExpression parserState selectionExpression =
  match parserState.currentStructuredFragmentDefinition with
  | Some fragmentDefinitionBuilder ->
      {
        parserState with
        currentStructuredFragmentDefinition =
          Some
            {
              fragmentDefinitionBuilder with
              builderFragmentSelectionTarget =
                {
                  fragmentDefinitionBuilder.builderFragmentSelectionTarget with
                  builderRootSelectionExpressions =
                    fragmentDefinitionBuilder.builderFragmentSelectionTarget
                      .builderRootSelectionExpressions @ [ selectionExpression ];
                };
            };
      }
  | None -> (
      match parserState.currentOperationDefinition with
      | Some operationDefinitionBuilder ->
          {
            parserState with
            currentOperationDefinition =
              Some
                {
                  operationDefinitionBuilder with
                  builderOperationSelectionTarget =
                    {
                      operationDefinitionBuilder.builderOperationSelectionTarget with
                      builderRootSelectionExpressions =
                        operationDefinitionBuilder
                          .builderOperationSelectionTarget
                          .builderRootSelectionExpressions
                        @ [ selectionExpression ];
                    };
                };
          }
      | None ->
          raise
            (Invalid_argument
               (Printf.sprintf
                  "Selections require an operation or fragment context.\n\n%s"
                  CommandLineInvocationShared.usageText)))

let withAddedCurrentTargetDirective parserState directiveText =
  match parserState.currentStructuredFragmentDefinition with
  | Some fragmentDefinitionBuilder ->
      {
        parserState with
        currentStructuredFragmentDefinition =
          Some
            {
              fragmentDefinitionBuilder with
              builderFragmentDirectiveTexts =
                fragmentDefinitionBuilder.builderFragmentDirectiveTexts
                @ [ directiveText ];
            };
      }
  | None -> (
      match parserState.currentOperationDefinition with
      | Some operationDefinitionBuilder ->
          {
            parserState with
            currentOperationDefinition =
              Some
                {
                  operationDefinitionBuilder with
                  builderOperationDirectiveTexts =
                    operationDefinitionBuilder.builderOperationDirectiveTexts
                    @ [ directiveText ];
                };
          }
      | None ->
          raise
            (Invalid_argument
               (Printf.sprintf
                  "Directives require an operation or fragment context.\n\n%s"
                  CommandLineInvocationShared.usageText)))

let startedOperationDefinition parserState operationType operationName =
  let finalizedParserState =
    parserState |> finalizedCurrentSelectionBranch
    |> finalizedCurrentStructuredFragmentDefinition
    |> finalizedCurrentOperationDefinition
  in
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
  let finalizedParserState =
    parserState |> finalizedCurrentSelectionBranch
    |> finalizedCurrentStructuredFragmentDefinition
    |> finalizedCurrentOperationDefinition
  in
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
  let finalizedParserState =
    parserState |> finalizedCurrentSelectionBranch
    |> finalizedCurrentStructuredFragmentDefinition
    |> finalizedCurrentOperationDefinition
  in
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
  let finalizedParserState =
    parserState |> finalizedCurrentSelectionBranch
    |> finalizedCurrentStructuredFragmentDefinition
    |> finalizedCurrentOperationDefinition
  in
  {
    operationDefinitions = finalizedParserState.finalizedOperationDefinitions;
    selectedOperationName = finalizedParserState.requestedOperationName;
    structuredFragmentDefinitions =
      finalizedParserState.finalizedStructuredFragmentDefinitions;
    rawFragmentDefinitionTexts =
      finalizedParserState.pendingRawFragmentDefinitionTexts;
  }
