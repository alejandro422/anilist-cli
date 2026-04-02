open Anilist

let assertEqual expected actual =
  if actual <> expected then
    failwith (Printf.sprintf "Expected:\n%s\n\nActual:\n%s" expected actual)

let assertContainsSubstring substring text =
  let rec loop startIndex =
    if startIndex + String.length substring > String.length text then
      failwith (Printf.sprintf "Expected substring %S in:\n%s" substring text)
    else if String.sub text startIndex (String.length substring) = substring
    then ()
    else loop (startIndex + 1)
  in
  loop 0

let fieldSegment ?alias ?(directives = []) fieldName optionPairs =
  let loweredFieldSegment : LoweringEngine.fieldSegment =
    {
      LoweringEngine.fieldName;
      LoweringEngine.alias;
      LoweringEngine.optionPairs;
      LoweringEngine.fieldDirectives = directives;
    }
  in
  LoweringEngine.FieldSegment loweredFieldSegment

let inlineFragmentSegment ?(directives = []) typeCondition =
  LoweringEngine.InlineFragmentSegment
    ({ LoweringEngine.typeCondition; inlineFragmentDirectives = directives }
      : LoweringEngine.inlineFragmentSegment)

let fragmentSpreadSegment ?(directives = []) name =
  LoweringEngine.FragmentSpreadSegment
    ({ LoweringEngine.name; fragmentSpreadDirectives = directives }
      : LoweringEngine.fragmentSpreadSegment)

let selectionBranch selectionPathSegments selectionExpressions =
  { LoweringEngine.selectionPathSegments; selectionExpressions }

let operationDefinition ?operationName ?(variableDefinitions = [])
    ?(variableAssignments = []) ?(directives = []) rootSelectionExpressions
    selectionBranches =
  ({
     LoweringEngine.operationType = LoweringEngine.Query;
     LoweringEngine.operationName;
     LoweringEngine.variableDefinitions;
     LoweringEngine.operationVariableAssignments = variableAssignments;
     LoweringEngine.operationDirectives = directives;
     LoweringEngine.rootSelectionExpressions;
     LoweringEngine.selectionBranches;
   }
    : LoweringEngine.operationDefinition)

let structuredFragmentDefinition ?(directives = []) fragmentName
    fragmentTypeCondition fragmentRootSelectionExpressions
    fragmentSelectionBranches =
  {
    LoweringEngine.fragmentName;
    fragmentTypeCondition;
    fragmentDirectives = directives;
    fragmentRootSelectionExpressions;
    fragmentSelectionBranches;
  }

let queryOfLoweredRequest loweredRequest =
  loweredRequest |> LoweringEngine.graphQlQueryOfRequest |> GraphQlQuery.render

let testSingleFieldSelectionShorthand () =
  let request =
    LoweringEngine.lower
      ~operationDefinitions:
        [
          operationDefinition []
            [
              selectionBranch
                [ fieldSegment "user" [ ("name", "fuwn") ] ]
                [ "id,name" ];
            ];
        ]
      ~selectedOperationName:None ~structuredFragmentDefinitions:[]
      ~rawFragmentDefinitionTexts:[]
  in
  assertEqual "query {\n  User(name: \"fuwn\") {\n    id\n    name\n  }\n}"
    (queryOfLoweredRequest request)

let testExplicitOperationWithMultipleRootFields () =
  let request =
    LoweringEngine.lower
      ~operationDefinitions:
        [
          ({
             LoweringEngine.operationType = LoweringEngine.Query;
             LoweringEngine.operationName = Some "Viewer";
             LoweringEngine.variableDefinitions = [ "$userName: String!" ];
             LoweringEngine.operationVariableAssignments =
               [ ("userName", "string:fuwn") ];
             LoweringEngine.operationDirectives = [ "cacheControl(maxAge: 60)" ];
             LoweringEngine.rootSelectionExpressions = [ "viewer.id" ];
             LoweringEngine.selectionBranches =
               [
                 selectionBranch
                   [ fieldSegment "user" [ ("name", "var:userName") ] ]
                   [ "id,name" ];
                 selectionBranch
                   [ fieldSegment "media" [ ("id", "42") ] ]
                   [ "title.romaji" ];
               ];
           }
            : LoweringEngine.operationDefinition);
        ]
      ~selectedOperationName:(Some "Viewer") ~structuredFragmentDefinitions:[]
      ~rawFragmentDefinitionTexts:[]
  in
  assertEqual
    "query Viewer($userName: String!) @cacheControl(maxAge: 60) {\n\
    \  Viewer {\n\
    \    id\n\
    \  }\n\
    \  User(name: $userName) {\n\
    \    id\n\
    \    name\n\
    \  }\n\
    \  Media(id: 42) {\n\
    \    title {\n\
    \      romaji\n\
    \    }\n\
    \  }\n\
     }"
    (queryOfLoweredRequest request)

let testFieldAliasAndDirectiveLowering () =
  let request =
    LoweringEngine.lower
      ~operationDefinitions:
        [
          operationDefinition []
            [
              selectionBranch
                [
                  fieldSegment ~alias:"account"
                    ~directives:[ "include(if: $withUser)" ]
                    "user"
                    [ ("name", "fuwn") ];
                ]
                [ "id,name" ];
            ];
        ]
      ~selectedOperationName:None ~structuredFragmentDefinitions:[]
      ~rawFragmentDefinitionTexts:[]
  in
  assertEqual
    "query {\n\
    \  account: User(name: \"fuwn\") @include(if: $withUser) {\n\
    \    id\n\
    \    name\n\
    \  }\n\
     }"
    (queryOfLoweredRequest request)

let testNestedFieldBranchLowering () =
  let request =
    LoweringEngine.lower
      ~operationDefinitions:
        [
          operationDefinition []
            [
              selectionBranch
                [
                  fieldSegment "media" [ ("type", "enum:ANIME"); ("id", "42") ];
                ]
                [ "id"; "title.romaji" ];
              selectionBranch
                [
                  fieldSegment "media" [ ("type", "enum:ANIME"); ("id", "42") ];
                  fieldSegment "cover-image" [];
                ]
                [ "extra-large" ];
            ];
        ]
      ~selectedOperationName:None ~structuredFragmentDefinitions:[]
      ~rawFragmentDefinitionTexts:[]
  in
  assertEqual
    "query {\n\
    \  Media(type: ANIME, id: 42) {\n\
    \    id\n\
    \    title {\n\
    \      romaji\n\
    \    }\n\
    \    coverImage {\n\
    \      extraLarge\n\
    \    }\n\
    \  }\n\
     }"
    (queryOfLoweredRequest request)

let testStructuredFragmentDefinitionLowering () =
  let request =
    LoweringEngine.lower
      ~operationDefinitions:
        [
          operationDefinition []
            [
              selectionBranch
                [ fieldSegment "media" [ ("id", "1") ] ]
                [ "id"; "...mediaCore" ];
            ];
        ]
      ~selectedOperationName:None
      ~structuredFragmentDefinitions:
        [
          structuredFragmentDefinition "mediaCore" "Media" [ "popularity" ]
            [ selectionBranch [ fieldSegment "title" [] ] [ "romaji" ] ];
        ]
      ~rawFragmentDefinitionTexts:[]
  in
  assertEqual
    "query {\n\
    \  Media(id: 1) {\n\
    \    id\n\
    \    ...mediaCore\n\
    \  }\n\
     }\n\n\
     fragment mediaCore on Media {\n\
    \  popularity\n\
    \  title {\n\
    \    romaji\n\
    \  }\n\
     }"
    (queryOfLoweredRequest request)

let testInlineFragmentBranchLowering () =
  let request =
    LoweringEngine.lower
      ~operationDefinitions:
        [
          operationDefinition []
            [
              selectionBranch [ fieldSegment "media" [ ("id", "1") ] ] [ "id" ];
              selectionBranch
                [
                  fieldSegment "media" [ ("id", "1") ];
                  inlineFragmentSegment ~directives:[ "skip(if: $skipAnime)" ]
                    "Anime";
                  fieldSegment "title" [];
                ]
                [ "romaji" ];
              selectionBranch
                [
                  fieldSegment "media" [ ("id", "1") ];
                  inlineFragmentSegment "Anime";
                  fieldSegment "relations" [ ("type", "enum:SEQUEL") ];
                ]
                [ "edges.node.id" ];
            ];
        ]
      ~selectedOperationName:None ~structuredFragmentDefinitions:[]
      ~rawFragmentDefinitionTexts:[]
  in
  assertEqual
    "query {\n\
    \  Media(id: 1) {\n\
    \    id\n\
    \    ... on Anime @skip(if: $skipAnime) {\n\
    \      title {\n\
    \        romaji\n\
    \      }\n\
    \    }\n\
    \    ... on Anime {\n\
    \      relations(type: SEQUEL) {\n\
    \        edges {\n\
    \          node {\n\
    \            id\n\
    \          }\n\
    \        }\n\
    \      }\n\
    \    }\n\
    \  }\n\
     }"
    (queryOfLoweredRequest request)

let testFragmentSpreadDirectiveLowering () =
  let request =
    LoweringEngine.lower
      ~operationDefinitions:
        [
          operationDefinition []
            [
              selectionBranch
                [
                  fieldSegment "media" [ ("id", "1") ];
                  fragmentSpreadSegment
                    ~directives:[ "include(if: $withCore)" ]
                    "mediaCore";
                ]
                [];
            ];
        ]
      ~selectedOperationName:None
      ~structuredFragmentDefinitions:
        [ structuredFragmentDefinition "mediaCore" "Media" [ "popularity" ] [] ]
      ~rawFragmentDefinitionTexts:[]
  in
  assertEqual
    "query {\n\
    \  Media(id: 1) {\n\
    \    ...mediaCore @include(if: $withCore)\n\
    \  }\n\
     }\n\n\
     fragment mediaCore on Media {\n\
    \  popularity\n\
     }"
    (queryOfLoweredRequest request)

let testRawAndStructuredFragmentsCoexist () =
  let request =
    LoweringEngine.lower
      ~operationDefinitions:
        [
          operationDefinition []
            [
              selectionBranch
                [ fieldSegment "media" [ ("id", "1") ] ]
                [ "...mediaCore"; "...legacyFragment" ];
            ];
        ]
      ~selectedOperationName:None
      ~structuredFragmentDefinitions:
        [ structuredFragmentDefinition "mediaCore" "Media" [ "popularity" ] [] ]
      ~rawFragmentDefinitionTexts:
        [ "fragment legacyFragment on Media { favourites }" ]
  in
  assertEqual
    "query {\n\
    \  Media(id: 1) {\n\
    \    ...mediaCore\n\
    \    ...legacyFragment\n\
    \  }\n\
     }\n\n\
     fragment mediaCore on Media {\n\
    \  popularity\n\
     }\n\n\
     fragment legacyFragment on Media { favourites }"
    (queryOfLoweredRequest request)

let testMultipleOperationDocumentLowering () =
  let request =
    LoweringEngine.lower
      ~operationDefinitions:
        [
          ({
             LoweringEngine.operationType = LoweringEngine.Query;
             LoweringEngine.operationName = Some "Viewer";
             LoweringEngine.variableDefinitions = [];
             LoweringEngine.operationVariableAssignments = [];
             LoweringEngine.operationDirectives = [ "cacheControl(maxAge: 60)" ];
             LoweringEngine.rootSelectionExpressions = [ "viewer.id" ];
             LoweringEngine.selectionBranches = [];
           }
            : LoweringEngine.operationDefinition);
          operationDefinition ~operationName:"UserLookup" []
            [
              selectionBranch
                [ fieldSegment "user" [ ("name", "fuwn") ] ]
                [ "id,name" ];
            ];
        ]
      ~selectedOperationName:(Some "UserLookup")
      ~structuredFragmentDefinitions:[] ~rawFragmentDefinitionTexts:[]
  in
  assertEqual
    "query Viewer @cacheControl(maxAge: 60) {\n\
    \  Viewer {\n\
    \    id\n\
    \  }\n\
     }\n\n\
     query UserLookup {\n\
    \  User(name: \"fuwn\") {\n\
    \    id\n\
    \    name\n\
    \  }\n\
     }"
    (queryOfLoweredRequest request)

let testSelectionSetCompatibilityAliasParsing () =
  match
    CommandLineInvocation.invocationOfArguments
      [ "user"; "--name"; "fuwn"; "--fields"; "id" ]
  with
  | Ok invocation ->
      let operationDefinition =
        List.hd invocation.CommandLineInvocation.operationDefinitions
      in
      assertEqual "1"
        (string_of_int
           (List.length
              operationDefinition.CommandLineInvocation.selectionBranches))
  | Error message -> failwith message

let testOperationParsing () =
  match
    CommandLineInvocation.invocationOfArguments
      [
        "query";
        "--operation-name";
        "Viewer";
        "--variable-definition";
        "$userName: String!";
        "--variable";
        "userName=string:fuwn";
        "--field";
        "user";
        "--alias";
        "account";
        "--directive";
        "include(if: $withUser)";
        "--name";
        "var:userName";
        "--selection-set";
        "id,name";
        "--fragment";
        "userCore:User";
        "--selection-set";
        "id";
      ]
  with
  | Ok invocation ->
      let operationDefinition =
        List.hd invocation.CommandLineInvocation.operationDefinitions
      in
      assertEqual "Viewer"
        (Option.get operationDefinition.CommandLineInvocation.operationName);
      assertEqual "1"
        (string_of_int
           (List.length
              operationDefinition.CommandLineInvocation.variableDefinitions));
      assertEqual "1"
        (string_of_int
           (List.length
              operationDefinition.CommandLineInvocation.variableAssignments));
      assertEqual "1"
        (string_of_int
           (List.length
              operationDefinition.CommandLineInvocation.selectionBranches));
      assertEqual "1"
        (string_of_int
           (List.length
              invocation.CommandLineInvocation.structuredFragmentDefinitions))
  | Error message -> failwith message

let testVariableAssignmentsPreserveGraphQlVariableNames () =
  match
    CommandLineInvocation.invocationOfArguments
      [
        "query";
        "--variable-definition";
        "$user_name: String!";
        "--variable";
        "user_name=string:fuwn";
        "--field";
        "user";
        "--name";
        "var:user_name";
        "--selection-set";
        "id";
      ]
  with
  | Ok invocation ->
      let operationDefinition =
        List.hd invocation.CommandLineInvocation.operationDefinitions
      in
      assertEqual "user_name"
        (fst
           (List.hd
              operationDefinition.CommandLineInvocation.variableAssignments))
  | Error message -> failwith message

let testVariableAssignmentsRejectVariableReferences () =
  match
    CommandLineInvocation.invocationOfArguments
      [
        "query";
        "--variable";
        "userName=var:otherUser";
        "--field";
        "user";
        "--selection-set";
        "id";
      ]
  with
  | Ok _ -> failwith "Expected variable assignment rejection"
  | Error _ -> ()

let testInlineFragmentsRequireSelections () =
  try
    ignore
      (LoweringEngine.lower
         ~operationDefinitions:
           [
             operationDefinition []
               [
                 selectionBranch
                   [ fieldSegment "media" [ ("id", "1") ] ]
                   [ "...on:Media" ];
               ];
           ]
         ~selectedOperationName:None ~structuredFragmentDefinitions:[]
         ~rawFragmentDefinitionTexts:[]);
    failwith "Expected inline fragment validation failure"
  with Invalid_argument _ -> ()

let testRelativeAndAbsoluteFieldPathParsing () =
  match
    CommandLineInvocation.invocationOfArguments
      [
        "user";
        "--name";
        "fuwn";
        "--selection-set";
        "id";
        "--field";
        "./statistics";
        "--selection-set";
        "anime.count";
        "--field";
        "/media";
        "--id";
        "42";
        "--selection-set";
        "id";
      ]
  with
  | Ok invocation -> (
      let operationDefinition =
        List.hd invocation.CommandLineInvocation.operationDefinitions
      in
      let secondSelectionBranch =
        operationDefinition.CommandLineInvocation.selectionBranches |> List.tl
        |> List.hd
      in
      let thirdSelectionBranch =
        operationDefinition.CommandLineInvocation.selectionBranches |> List.rev
        |> List.hd
      in
      assertEqual "2"
        (string_of_int
           (List.length
              secondSelectionBranch.CommandLineInvocation.selectionPathSegments));
      match
        List.hd thirdSelectionBranch.CommandLineInvocation.selectionPathSegments
      with
      | CommandLineInvocation.FieldSegment fieldSegment ->
          assertEqual "media" fieldSegment.CommandLineInvocation.fieldName
      | CommandLineInvocation.InlineFragmentSegment _
      | CommandLineInvocation.FragmentSpreadSegment _ ->
          failwith "Expected field segment")
  | Error message -> failwith message

let testExplicitOperationBarePositionalSiblingFields () =
  match
    CommandLineInvocation.invocationOfArguments
      [
        "query";
        "user";
        "--selection-set";
        "id";
        "media";
        "--selection-set";
        "id";
      ]
  with
  | Ok invocation ->
      let operationDefinition =
        List.hd invocation.CommandLineInvocation.operationDefinitions
      in
      assertEqual "2"
        (string_of_int
           (List.length
              operationDefinition.CommandLineInvocation.selectionBranches))
  | Error message -> failwith message

let testInlineFragmentParsing () =
  match
    CommandLineInvocation.invocationOfArguments
      [
        "query";
        "--field";
        "media";
        "--id";
        "1";
        "--inline-fragment";
        "Anime";
        "--directive";
        "skip(if: $skipAnime)";
        "--selection-set";
        "episodes";
      ]
  with
  | Ok invocation ->
      let operationDefinition =
        List.hd invocation.CommandLineInvocation.operationDefinitions
      in
      let inlineFragmentBranch =
        operationDefinition.CommandLineInvocation.selectionBranches |> List.rev
        |> List.hd
      in
      assertEqual "2"
        (string_of_int
           (List.length
              inlineFragmentBranch.CommandLineInvocation.selectionPathSegments))
  | Error message -> failwith message

let testFragmentSpreadParsing () =
  match
    CommandLineInvocation.invocationOfArguments
      [
        "query";
        "--field";
        "media";
        "--id";
        "1";
        "--fragment-spread";
        "./mediaCore";
        "--directive";
        "include(if: $withCore)";
      ]
  with
  | Ok invocation -> (
      let operationDefinition =
        List.hd invocation.CommandLineInvocation.operationDefinitions
      in
      let fragmentSpreadBranch =
        operationDefinition.CommandLineInvocation.selectionBranches |> List.rev
        |> List.hd
      in
      match
        List.rev
          fragmentSpreadBranch.CommandLineInvocation.selectionPathSegments
        |> List.hd
      with
      | CommandLineInvocation.FragmentSpreadSegment fragmentSpreadSegment ->
          assertEqual "mediaCore"
            fragmentSpreadSegment.CommandLineInvocation.fragmentSpreadName;
          assertEqual "1"
            (string_of_int
               (List.length
                  fragmentSpreadSegment
                    .CommandLineInvocation.fragmentSpreadDirectiveTexts))
      | CommandLineInvocation.FieldSegment _
      | CommandLineInvocation.InlineFragmentSegment _ ->
          failwith "Expected fragment spread segment")
  | Error message -> failwith message

let testMultipleOperationsRequireSelectedOperationName () =
  match
    CommandLineInvocation.invocationOfArguments
      [
        "--operation";
        "query:Viewer";
        "viewer";
        "--selection-set";
        "id";
        "--operation";
        "query:UserLookup";
        "user";
        "--name";
        "fuwn";
        "--selection-set";
        "id";
      ]
  with
  | Ok _ -> failwith "Expected selected operation validation failure"
  | Error _ -> ()

let testMultipleOperationParsing () =
  match
    CommandLineInvocation.invocationOfArguments
      [
        "--operation";
        "query:Viewer";
        "viewer";
        "--selection-set";
        "id";
        "--operation";
        "query:UserLookup";
        "user";
        "--name";
        "fuwn";
        "--selection-set";
        "id";
        "--execute-operation-name";
        "UserLookup";
      ]
  with
  | Ok invocation ->
      assertEqual "2"
        (string_of_int
           (List.length invocation.CommandLineInvocation.operationDefinitions));
      assertEqual "UserLookup"
        (Option.get invocation.CommandLineInvocation.selectedOperationName)
  | Error message -> failwith message

let testSchemaCommandParsing () =
  match SchemaCommand.invocationOfArguments [ "schema" ] with
  | Ok (Some schemaCommand) ->
      assertEqual "IntrospectionQuery"
        (Option.get schemaCommand.SchemaCommand.operationName);
      assertEqual "0"
        (string_of_int
           (match schemaCommand.SchemaCommand.variables with
           | Some _ -> 1
           | None -> 0));
      assertEqual "0"
        (string_of_int
           (match schemaCommand.SchemaCommand.requestedDirectiveName with
           | Some _ -> 1
           | None -> 0));
      assertContainsSubstring "__schema" schemaCommand.SchemaCommand.queryText;
      assertContainsSubstring "fragment FullType"
        schemaCommand.SchemaCommand.queryText
  | Ok None -> failwith "Expected schema command"
  | Error message -> failwith message

let testSchemaTypeCommandParsing () =
  match SchemaCommand.invocationOfArguments [ "schema"; "--type"; "Media" ] with
  | Ok (Some schemaCommand) ->
      assertEqual "IntrospectionTypeQuery"
        (Option.get schemaCommand.SchemaCommand.operationName);
      assertContainsSubstring "__type(name: $name)"
        schemaCommand.SchemaCommand.queryText;
      assertEqual "0"
        (string_of_int
           (match schemaCommand.SchemaCommand.requestedDirectiveName with
           | Some _ -> 1
           | None -> 0));
      assertEqual "{\"name\":\"Media\"}"
        (schemaCommand.SchemaCommand.variables |> Option.get
       |> Yojson.Safe.to_string)
  | Ok None -> failwith "Expected schema type command"
  | Error message -> failwith message

let testSchemaDirectiveCommandParsing () =
  match
    SchemaCommand.invocationOfArguments [ "schema"; "--directive"; "include" ]
  with
  | Ok (Some schemaCommand) ->
      assertEqual "IntrospectionDirectiveQuery"
        (Option.get schemaCommand.SchemaCommand.operationName);
      assertContainsSubstring "directives" schemaCommand.SchemaCommand.queryText;
      assertEqual "include"
        (Option.get schemaCommand.SchemaCommand.requestedDirectiveName);
      assertEqual "0"
        (string_of_int
           (match schemaCommand.SchemaCommand.variables with
           | Some _ -> 1
           | None -> 0))
  | Ok None -> failwith "Expected schema directive command"
  | Error message -> failwith message

let testSchemaEqualsOptionParsing () =
  (match SchemaCommand.invocationOfArguments [ "schema"; "--type=Media" ] with
  | Ok (Some schemaCommand) ->
      assertEqual "IntrospectionTypeQuery"
        (Option.get schemaCommand.SchemaCommand.operationName)
  | Ok None -> failwith "Expected schema type command"
  | Error message -> failwith message);
  match
    SchemaCommand.invocationOfArguments [ "schema"; "--directive=include" ]
  with
  | Ok (Some schemaCommand) ->
      assertEqual "IntrospectionDirectiveQuery"
        (Option.get schemaCommand.SchemaCommand.operationName)
  | Ok None -> failwith "Expected schema directive command"
  | Error message -> failwith message

let testSchemaCommandRejectsConflictingOptions () =
  match
    SchemaCommand.invocationOfArguments
      [ "schema"; "--type"; "Media"; "--directive"; "include" ]
  with
  | Ok _ -> failwith "Expected schema command rejection"
  | Error _ -> ()

let testSchemaCommandRejectsMissingOptionValues () =
  match
    SchemaCommand.invocationOfArguments
      [ "schema"; "--type"; "--directive"; "include" ]
  with
  | Ok _ -> failwith "Expected missing type value rejection"
  | Error message ->
      assertContainsSubstring "Schema --type requires a value." message

let testRequestHeaderExtraction () =
  match
    RequestOptions.extractionOfArguments
      [
        "--header";
        "Authorization: Bearer token";
        "user";
        "--name";
        "fuwn";
        "--header=X-Test: 1";
      ]
  with
  | Ok requestOptions ->
      assertEqual "2"
        (string_of_int (List.length requestOptions.RequestOptions.headerPairs));
      assertEqual "3"
        (string_of_int
           (List.length requestOptions.RequestOptions.remainingArguments));
      assertEqual "Authorization"
        (requestOptions.RequestOptions.headerPairs |> List.hd |> fst)
  | Error message -> failwith message

let testRequestHeaderExtractionRejectsMalformedHeader () =
  match
    RequestOptions.extractionOfArguments
      [ "--header"; "Authorization Bearer token" ]
  with
  | Ok _ -> failwith "Expected malformed header rejection"
  | Error message ->
      assertContainsSubstring "Header value must use 'Name: Value' syntax."
        message

let testRequestHeaderExtractionRejectsMissingHeaderValue () =
  match RequestOptions.extractionOfArguments [ "--header" ] with
  | Ok _ -> failwith "Expected missing header rejection"
  | Error message ->
      assertContainsSubstring "Missing value for option --header." message

let testCommandLineHelpDetection () =
  if not (CommandLineInvocation.helpRequestedOfArguments [ "--help" ]) then
    failwith "Expected top-level help detection";
  if not (CommandLineInvocation.helpRequestedOfArguments [ "query"; "--help" ])
  then failwith "Expected trailing flag help detection";
  if CommandLineInvocation.helpRequestedOfArguments [ "--bogus"; "help" ] then
    failwith "Unexpected bare help token detection";
  if
    CommandLineInvocation.helpRequestedOfArguments
      [ "query"; "--help"; "extra" ]
  then failwith "Unexpected non-trailing help detection"

let testSchemaHelpDetection () =
  if not (SchemaCommand.helpRequestedOfArguments [ "schema"; "--help" ]) then
    failwith "Expected schema help detection";
  if not (SchemaCommand.helpRequestedOfArguments [ "schema"; "help" ]) then
    failwith "Expected schema bare help detection";
  if SchemaCommand.helpRequestedOfArguments [ "schema"; "nope"; "help" ] then
    failwith "Unexpected schema malformed help detection"

let () =
  testSingleFieldSelectionShorthand ();
  testExplicitOperationWithMultipleRootFields ();
  testFieldAliasAndDirectiveLowering ();
  testNestedFieldBranchLowering ();
  testStructuredFragmentDefinitionLowering ();
  testInlineFragmentBranchLowering ();
  testFragmentSpreadDirectiveLowering ();
  testRawAndStructuredFragmentsCoexist ();
  testMultipleOperationDocumentLowering ();
  testSelectionSetCompatibilityAliasParsing ();
  testOperationParsing ();
  testVariableAssignmentsPreserveGraphQlVariableNames ();
  testVariableAssignmentsRejectVariableReferences ();
  testInlineFragmentsRequireSelections ();
  testRelativeAndAbsoluteFieldPathParsing ();
  testExplicitOperationBarePositionalSiblingFields ();
  testInlineFragmentParsing ();
  testFragmentSpreadParsing ();
  testMultipleOperationsRequireSelectedOperationName ();
  testMultipleOperationParsing ();
  testSchemaCommandParsing ();
  testSchemaTypeCommandParsing ();
  testSchemaDirectiveCommandParsing ();
  testSchemaEqualsOptionParsing ();
  testSchemaCommandRejectsConflictingOptions ();
  testSchemaCommandRejectsMissingOptionValues ();
  testRequestHeaderExtraction ();
  testRequestHeaderExtractionRejectsMalformedHeader ();
  testRequestHeaderExtractionRejectsMissingHeaderValue ();
  testCommandLineHelpDetection ();
  testSchemaHelpDetection ()
