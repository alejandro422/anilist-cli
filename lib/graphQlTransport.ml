open Lwt.Infix

let defaultEndpoint = "https://graphql.anilist.co"

let endpointOfEnvironment () =
  match Sys.getenv_opt "ANILIST_GRAPHQL_ENDPOINT" with
  | Some endpoint when endpoint <> "" -> endpoint
  | _ -> defaultEndpoint

let executeQuery ~operationName ~variables ~headers:additionalHeaders ~endpoint
    ~query =
  let requestBodyFields = [ ("query", `String query) ] in
  let requestBodyFields =
    match operationName with
    | Some operationName ->
        requestBodyFields @ [ ("operationName", `String operationName) ]
    | None -> requestBodyFields
  in
  let requestBody =
    match variables with
    | Some variables ->
        `Assoc (requestBodyFields @ [ ("variables", variables) ])
        |> Yojson.Safe.to_string
    | None -> `Assoc requestBodyFields |> Yojson.Safe.to_string
  in
  let headers =
    List.fold_left
      (fun requestHeaders (headerName, headerValue) ->
        Cohttp.Header.replace requestHeaders headerName headerValue)
      (Cohttp.Header.of_list
         [
           ("content-type", "application/json"); ("accept", "application/json");
         ])
      additionalHeaders
  in
  let uri = Uri.of_string endpoint in
  let body = Cohttp_lwt.Body.of_string requestBody in
  Cohttp_lwt_unix.Client.post ~headers ~body uri
  >>= fun (response, responseBody) ->
  Cohttp_lwt.Body.to_string responseBody >|= fun responseBodyText ->
  ( Cohttp.Response.status response |> Cohttp.Code.code_of_status,
    responseBodyText )

let prettyPrintedJsonOrOriginal responseBody =
  try responseBody |> Yojson.Safe.from_string |> Yojson.Safe.pretty_to_string
  with Yojson.Json_error _ -> responseBody
