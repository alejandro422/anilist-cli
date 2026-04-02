open Lwt.Infix

type t = SchemaCommandTypes.t = {
  operationName : string option;
  queryText : string;
  variables : Yojson.Safe.t option;
  requestedDirectiveName : string option;
}

let usageText = SchemaArgumentParser.usageText
let helpRequestedOfArguments = SchemaArgumentParser.helpRequestedOfArguments
let invocationOfArguments = SchemaArgumentParser.invocationOfArguments

let filterDirectiveResponse ~directiveName responseBody =
  let filteredDirectives directives =
    directives
    |> List.filter (function
      | `Assoc association -> (
          match List.assoc_opt "name" association with
          | Some (`String name) -> String.equal name directiveName
          | _ -> false)
      | _ -> false)
  in
  try
    match Yojson.Safe.from_string responseBody with
    | `Assoc rootAssociation -> (
        let preservedTopLevelFields =
          rootAssociation
          |> List.filter (fun (fieldName, _) ->
              not (String.equal fieldName "data"))
        in
        match List.assoc_opt "data" rootAssociation with
        | Some (`Assoc dataAssociation) -> (
            match List.assoc_opt "__schema" dataAssociation with
            | Some (`Assoc schemaAssociation) -> (
                match List.assoc_opt "directives" schemaAssociation with
                | Some (`List directives) ->
                    Ok
                      (`Assoc
                         (( "data",
                            `Assoc
                              [
                                ( "__schema",
                                  `Assoc
                                    [
                                      ( "directives",
                                        `List (filteredDirectives directives) );
                                    ] );
                              ] )
                         :: preservedTopLevelFields)
                      |> Yojson.Safe.pretty_to_string)
                | _ ->
                    Error
                      "Schema directive response did not contain a directives \
                       list.")
            | _ -> Error "Schema directive response did not contain __schema.")
        | _ -> Error "Schema directive response did not contain data.")
    | _ -> Error "Schema directive response was not a JSON object."
  with Yojson.Json_error message ->
    Error
      (Printf.sprintf "Failed to parse schema directive response: %s" message)

let execute ~headers ~endpoint (schemaCommand : t) =
  GraphQlTransport.executeQuery ~operationName:schemaCommand.operationName
    ~variables:schemaCommand.variables ~headers ~endpoint
    ~query:schemaCommand.queryText
  >>= fun (statusCode, responseBody) ->
  match schemaCommand.requestedDirectiveName with
  | None -> Lwt.return (statusCode, responseBody)
  | Some _ when statusCode < 200 || statusCode >= 300 ->
      Lwt.return (statusCode, responseBody)
  | Some directiveName -> (
      match filterDirectiveResponse ~directiveName responseBody with
      | Ok filteredResponse -> Lwt.return (statusCode, filteredResponse)
      | Error message -> Lwt.fail_with message)
