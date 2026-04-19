open Lwt.Infix

let printAndExit channel exitCode message =
  output_string channel message;
  output_char channel '\n';
  flush channel;
  exit exitCode

let printResponse statusCode responseBody =
  let printableResponse =
    Anilist.GraphQlTransport.prettyPrintedJsonOrOriginal responseBody
  in
  if statusCode >= 200 && statusCode < 300 then (
    print_endline printableResponse;
    Lwt.return 0)
  else (
    prerr_endline printableResponse;
    Lwt.return 1)

let executeQuery ~headers ~operationName ~variables ~query =
  let endpoint = Anilist.GraphQlTransport.endpointOfEnvironment () in
  Anilist.GraphQlTransport.executeQuery ~operationName ~variables ~headers
    ~endpoint ~query
  >>= fun (statusCode, responseBody) -> printResponse statusCode responseBody

let runProtected task =
  Lwt.catch task (fun exceptionValue ->
      prerr_endline
        (Printf.sprintf "Request failed: %s"
           (Printexc.to_string exceptionValue));
      Lwt.return 1)

let run ~headers invocation =
  runProtected (fun () ->
      let loweredRequest =
        Anilist.LoweringEngine.lower
          ~operationDefinitions:
            invocation.Anilist.CommandLineInvocation.operationDefinitions
          ~selectedOperationName:
            invocation.Anilist.CommandLineInvocation.selectedOperationName
          ~structuredFragmentDefinitions:
            invocation
              .Anilist.CommandLineInvocation.structuredFragmentDefinitions
          ~rawFragmentDefinitionTexts:
            invocation.Anilist.CommandLineInvocation.rawFragmentDefinitionTexts
      in
      if loweredRequest.Anilist.LoweringEngine.operations = [] then
        printAndExit stderr 1
          (Printf.sprintf "Selection set cannot be empty.\n\n%s"
             Anilist.CommandLineInvocation.usageText);
      let query =
        loweredRequest |> Anilist.LoweringEngine.graphQlQueryOfRequest
        |> Anilist.GraphQlQuery.render
      in
      let variables =
        match
          loweredRequest.Anilist.LoweringEngine.loweredVariableAssignments
        with
        | [] -> None
        | variableAssignments ->
            Some
              (`Assoc
                 (variableAssignments
                 |> List.map (fun (variableName, variableValue) ->
                     ( variableName,
                       Anilist.CliArgument.jsonLiteralOfValue variableValue ))))
      in
      executeQuery ~headers
        ~operationName:
          loweredRequest.Anilist.LoweringEngine.selectedOperationName ~variables
        ~query)

let () =
  let arguments = List.tl (Array.to_list Sys.argv) in
  match Anilist.RequestOptions.extractionOfArguments arguments with
  | Error message -> printAndExit stderr 1 message
  | Ok requestOptions -> (
      match
        Anilist.SchemaCommand.invocationOfArguments
          requestOptions.Anilist.RequestOptions.remainingArguments
      with
      | Ok (Some schemaCommand) ->
          exit
            (Lwt_main.run
               (runProtected (fun () ->
                    let endpoint =
                      Anilist.GraphQlTransport.endpointOfEnvironment ()
                    in
                    Anilist.SchemaCommand.execute
                      ~headers:requestOptions.Anilist.RequestOptions.headerPairs
                      ~endpoint schemaCommand
                    >>= fun (statusCode, responseBody) ->
                    printResponse statusCode responseBody)))
      | Error message ->
          if
            Anilist.SchemaCommand.helpRequestedOfArguments
              requestOptions.Anilist.RequestOptions.remainingArguments
          then printAndExit stdout 0 message
          else printAndExit stderr 1 message
      | Ok None -> (
          match
            Anilist.CommandLineInvocation.invocationOfArguments
              requestOptions.Anilist.RequestOptions.remainingArguments
          with
          | Ok invocation ->
              exit
                (Lwt_main.run
                   (run
                      ~headers:requestOptions.Anilist.RequestOptions.headerPairs
                      invocation))
          | Error message ->
              if
                Anilist.CommandLineInvocation.helpRequestedOfArguments
                  requestOptions.Anilist.RequestOptions.remainingArguments
              then printAndExit stdout 0 message
              else printAndExit stderr 1 message))
