open CommandLineInvocationTypes

let rec parseRemainingArguments parserState = function
  | [] -> CommandLineInvocationState.currentInvocationOfState parserState
  | "--help" :: _ ->
      raise (Invalid_argument CommandLineInvocationShared.usageText)
  | token :: remainingTokens when CommandLineInvocationShared.isLongOption token
    -> (
      let bareOptionName =
        if String.contains token '=' then
          fst
            (Option.get
               (CommandLineInvocationShared.optionNameAndValueOfEqualsSyntax
                  token))
        else String.sub token 2 (String.length token - 2)
      in
      match bareOptionName with
      | optionName
        when optionName = CommandLineInvocationShared.operationOptionName ->
          let operationHeaderText, remainingTokensAfterOperation =
            CommandLineInvocationShared.valueOfOptionToken token remainingTokens
          in
          let operationType, operationName =
            CommandLineInvocationShared.operationHeaderOfText
              operationHeaderText
          in
          parseRemainingArguments
            (CommandLineInvocationState.startedOperationDefinition parserState
               operationType operationName)
            remainingTokensAfterOperation
      | optionName when optionName = CommandLineInvocationShared.fieldOptionName
        ->
          let fieldPath, remainingTokensAfterField =
            CommandLineInvocationShared.valueOfOptionToken token remainingTokens
          in
          let resolvedSelectionPathSegments =
            CommandLineInvocationBranch.resolvedSelectionPathSegmentsOfFieldPath
              parserState fieldPath
          in
          let updatedParserState =
            CommandLineInvocationState.finalizedCurrentSelectionBranch
              parserState
          in
          parseRemainingArguments
            (CommandLineInvocationState.withUpdatedCurrentSelectionBranch
               updatedParserState
               (CommandLineInvocationBranch
                .makeSelectionBranchBuilderFromSegments
                  resolvedSelectionPathSegments))
            remainingTokensAfterField
      | optionName
        when optionName = CommandLineInvocationShared.inlineFragmentOptionName
        ->
          let inlineFragmentTypeCondition, remainingTokensAfterInlineFragment =
            CommandLineInvocationShared.valueOfOptionToken token remainingTokens
          in
          let resolvedSelectionPathSegments =
            CommandLineInvocationBranch
            .resolvedSelectionPathSegmentsOfInlineFragment parserState
              inlineFragmentTypeCondition
          in
          let updatedParserState =
            CommandLineInvocationState.finalizedCurrentSelectionBranch
              parserState
          in
          parseRemainingArguments
            (CommandLineInvocationState.withUpdatedCurrentSelectionBranch
               updatedParserState
               (CommandLineInvocationBranch
                .makeSelectionBranchBuilderFromSegments
                  resolvedSelectionPathSegments))
            remainingTokensAfterInlineFragment
      | optionName
        when optionName = CommandLineInvocationShared.fragmentSpreadOptionName
             || optionName
                = CommandLineInvocationShared
                  .fragmentSpreadCompatibilityOptionName ->
          let fragmentSpreadText, remainingTokensAfterFragmentSpread =
            CommandLineInvocationShared.valueOfOptionToken token remainingTokens
          in
          let resolvedSelectionPathSegments =
            CommandLineInvocationBranch
            .resolvedSelectionPathSegmentsOfFragmentSpread parserState
              fragmentSpreadText
          in
          let updatedParserState =
            CommandLineInvocationState.finalizedCurrentSelectionBranch
              parserState
          in
          parseRemainingArguments
            (CommandLineInvocationState.withUpdatedCurrentSelectionBranch
               updatedParserState
               (CommandLineInvocationBranch
                .makeSelectionBranchBuilderFromSegments
                  resolvedSelectionPathSegments))
            remainingTokensAfterFragmentSpread
      | optionName
        when optionName = CommandLineInvocationShared.fragmentOptionName ->
          let fragmentText, remainingTokensAfterFragment =
            CommandLineInvocationShared.valueOfOptionToken token remainingTokens
          in
          let fragmentName, fragmentTypeCondition =
            CommandLineInvocationShared.structuredFragmentNameAndTypeCondition
              fragmentText
          in
          parseRemainingArguments
            (CommandLineInvocationState.startedStructuredFragmentDefinition
               parserState fragmentName fragmentTypeCondition)
            remainingTokensAfterFragment
      | optionName
        when optionName = CommandLineInvocationShared.selectionSetOptionName
             || optionName
                = CommandLineInvocationShared
                  .compatibilitySelectionSetOptionName ->
          let selectionExpression, remainingTokensAfterSelection =
            CommandLineInvocationShared.valueOfOptionToken token remainingTokens
          in
          let updatedParserState =
            match
              CommandLineInvocationBranch.currentSelectionBranchOfState
                parserState
            with
            | Some selectionBranchBuilder ->
                CommandLineInvocationState.withUpdatedCurrentSelectionBranch
                  parserState
                  (CommandLineInvocationBranch
                   .withAddedBranchSelectionExpression selectionBranchBuilder
                     selectionExpression)
            | None ->
                CommandLineInvocationState.withAddedRootSelectionExpression
                  parserState selectionExpression
          in
          parseRemainingArguments updatedParserState
            remainingTokensAfterSelection
      | optionName
        when optionName = CommandLineInvocationShared.operationNameOptionName
        -> (
          let operationName, remainingTokensAfterName =
            CommandLineInvocationShared.valueOfOptionToken token remainingTokens
          in
          match parserState.currentOperationDefinition with
          | Some operationDefinitionBuilder ->
              parseRemainingArguments
                (CommandLineInvocationState
                 .withUpdatedCurrentOperationDefinition parserState
                   {
                     operationDefinitionBuilder with
                     builderOperationName = Some operationName;
                   })
                remainingTokensAfterName
          | None ->
              raise
                (Invalid_argument
                   (Printf.sprintf
                      "--operation-name requires a current operation.\n\n%s"
                      CommandLineInvocationShared.usageText)))
      | optionName
        when optionName
             = CommandLineInvocationShared.selectedOperationNameOptionName ->
          let selectedOperationName, remainingTokensAfterSelectedOperationName =
            CommandLineInvocationShared.valueOfOptionToken token remainingTokens
          in
          parseRemainingArguments
            {
              parserState with
              requestedOperationName = Some selectedOperationName;
            }
            remainingTokensAfterSelectedOperationName
      | optionName
        when optionName
             = CommandLineInvocationShared.variableDefinitionOptionName -> (
          let variableDefinition, remainingTokensAfterVariableDefinition =
            CommandLineInvocationShared.valueOfOptionToken token remainingTokens
          in
          match parserState.currentOperationDefinition with
          | Some operationDefinitionBuilder ->
              parseRemainingArguments
                (CommandLineInvocationState
                 .withUpdatedCurrentOperationDefinition parserState
                   {
                     operationDefinitionBuilder with
                     builderVariableDefinitions =
                       operationDefinitionBuilder.builderVariableDefinitions
                       @ [ variableDefinition ];
                   })
                remainingTokensAfterVariableDefinition
          | None ->
              raise
                (Invalid_argument
                   (Printf.sprintf
                      "--variable-definition requires a current operation.\n\n\
                       %s"
                      CommandLineInvocationShared.usageText)))
      | optionName
        when optionName = CommandLineInvocationShared.variableOptionName -> (
          let variableAssignmentText, remainingTokensAfterVariable =
            CommandLineInvocationShared.valueOfOptionToken token remainingTokens
          in
          match parserState.currentOperationDefinition with
          | Some operationDefinitionBuilder ->
              parseRemainingArguments
                (CommandLineInvocationState
                 .withUpdatedCurrentOperationDefinition parserState
                   {
                     operationDefinitionBuilder with
                     builderVariableAssignments =
                       operationDefinitionBuilder.builderVariableAssignments
                       @ [
                           CommandLineInvocationShared.variableAssignmentOfText
                             variableAssignmentText;
                         ];
                   })
                remainingTokensAfterVariable
          | None ->
              raise
                (Invalid_argument
                   (Printf.sprintf
                      "--variable requires a current operation.\n\n%s"
                      CommandLineInvocationShared.usageText)))
      | optionName
        when optionName = CommandLineInvocationShared.directiveOptionName ->
          let directiveText, remainingTokensAfterDirective =
            CommandLineInvocationShared.valueOfOptionToken token remainingTokens
          in
          let updatedParserState =
            match
              CommandLineInvocationBranch.currentSelectionBranchOfState
                parserState
            with
            | Some selectionBranchBuilder ->
                CommandLineInvocationState.withUpdatedCurrentSelectionBranch
                  parserState
                  (CommandLineInvocationBranch.withAddedBranchDirective
                     selectionBranchBuilder directiveText)
            | None ->
                CommandLineInvocationState.withAddedCurrentTargetDirective
                  parserState directiveText
          in
          parseRemainingArguments updatedParserState
            remainingTokensAfterDirective
      | optionName when optionName = CommandLineInvocationShared.aliasOptionName
        ->
          let fieldAlias, remainingTokensAfterAlias =
            CommandLineInvocationShared.valueOfOptionToken token remainingTokens
          in
          let updatedParserState =
            match
              CommandLineInvocationBranch.currentSelectionBranchOfState
                parserState
            with
            | Some selectionBranchBuilder ->
                CommandLineInvocationState.withUpdatedCurrentSelectionBranch
                  parserState
                  (CommandLineInvocationBranch.withUpdatedBranchFieldAlias
                     selectionBranchBuilder fieldAlias)
            | None ->
                raise
                  (Invalid_argument
                     (Printf.sprintf
                        "--alias requires an existing selection branch.\n\n%s"
                        CommandLineInvocationShared.usageText))
          in
          parseRemainingArguments updatedParserState remainingTokensAfterAlias
      | optionName
        when optionName
             = CommandLineInvocationShared.fragmentDefinitionOptionName ->
          let fragmentDefinitionText, remainingTokensAfterFragmentDefinition =
            CommandLineInvocationShared.valueOfOptionToken token remainingTokens
          in
          let updatedParserState =
            parserState
            |> CommandLineInvocationState.finalizedCurrentSelectionBranch
            |> CommandLineInvocationState
               .finalizedCurrentStructuredFragmentDefinition
            |> CommandLineInvocationState.finalizedCurrentOperationDefinition
          in
          parseRemainingArguments
            {
              updatedParserState with
              pendingRawFragmentDefinitionTexts =
                updatedParserState.pendingRawFragmentDefinitionTexts
                @ [ fragmentDefinitionText ];
            }
            remainingTokensAfterFragmentDefinition
      | _ ->
          let fieldArgumentPair, remainingTokensAfterOption =
            CommandLineInvocationShared.optionPairOfToken token remainingTokens
          in
          let updatedParserState =
            match
              CommandLineInvocationBranch.currentSelectionBranchOfState
                parserState
            with
            | Some selectionBranchBuilder ->
                CommandLineInvocationState.withUpdatedCurrentSelectionBranch
                  parserState
                  (CommandLineInvocationBranch.withAddedBranchFieldArgumentPair
                     selectionBranchBuilder fieldArgumentPair)
            | None ->
                raise
                  (Invalid_argument
                     (Printf.sprintf
                        "Field argument %s requires an existing selection \
                         branch.\n\n\
                         %s"
                        (fst fieldArgumentPair)
                        CommandLineInvocationShared.usageText))
          in
          parseRemainingArguments updatedParserState remainingTokensAfterOption)
  | token :: remainingTokens -> (
      match
        CommandLineInvocationBranch.currentSelectionBranchOfState parserState
      with
      | Some selectionBranchBuilder
        when CommandLineInvocationBranch.currentDefaultSelectionPathPrefix
               parserState
             = []
             && selectionBranchBuilder.builderSelectionExpressions <> [] ->
          let updatedParserState =
            CommandLineInvocationState.finalizedCurrentSelectionBranch
              parserState
          in
          let resolvedSelectionPathSegments =
            CommandLineInvocationBranch.resolvedSelectionPathSegmentsOfFieldPath
              updatedParserState token
          in
          parseRemainingArguments
            (CommandLineInvocationState.withUpdatedCurrentSelectionBranch
               updatedParserState
               (CommandLineInvocationBranch
                .makeSelectionBranchBuilderFromSegments
                  resolvedSelectionPathSegments))
            remainingTokens
      | Some selectionBranchBuilder ->
          parseRemainingArguments
            (CommandLineInvocationState.withUpdatedCurrentSelectionBranch
               parserState
               (CommandLineInvocationBranch.withPushedFieldSelectionPathSegment
                  selectionBranchBuilder token))
            remainingTokens
      | None -> (
          match parserState.currentOperationDefinition with
          | None -> (
              match CommandLineInvocationShared.operationTypeOfToken token with
              | Some operationType ->
                  parseRemainingArguments
                    (CommandLineInvocationState.startedOperationDefinition
                       parserState operationType None)
                    remainingTokens
              | None ->
                  parseRemainingArguments
                    (CommandLineInvocationState.startedShorthandQueryOperation
                       parserState token)
                    remainingTokens)
          | Some _ ->
              let resolvedSelectionPathSegments =
                CommandLineInvocationBranch
                .resolvedSelectionPathSegmentsOfFieldPath parserState token
              in
              parseRemainingArguments
                (CommandLineInvocationState.withUpdatedCurrentSelectionBranch
                   parserState
                   (CommandLineInvocationBranch
                    .makeSelectionBranchBuilderFromSegments
                      resolvedSelectionPathSegments))
                remainingTokens))

let invocationOfArguments arguments =
  match arguments with
  | [] -> Error CommandLineInvocationShared.usageText
  | firstToken :: _ when firstToken = "help" || firstToken = "--help" ->
      Error CommandLineInvocationShared.usageText
  | _ -> (
      try
        let invocation =
          parseRemainingArguments CommandLineInvocationState.initialParserState
            arguments
        in
        CommandLineInvocationValidation.validatedInvocation invocation
      with Invalid_argument message -> Error message)
