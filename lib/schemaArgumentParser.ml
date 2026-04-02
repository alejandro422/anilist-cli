type parsedArguments = {
  typeName : string option;
  directiveName : string option;
}

let usageText =
  String.concat "\n"
    [
      "Usage:";
      "  anilist schema";
      "  anilist schema --type <Type>";
      "  anilist schema --directive <Directive>";
      "  anilist ... --header 'Authorization: Bearer <token>'";
      "";
      "Examples:";
      "  anilist schema";
      "  anilist schema --type Media";
      "  anilist schema --directive include";
    ]

let helpRequestedOfArguments = function
  | [ "schema"; "--help" ] | [ "schema"; "help" ] -> true
  | _ -> false

let parseArguments arguments =
  let valueOfOption optionName optionValue =
    if String.starts_with ~prefix:"--" optionValue then
      Error
        (Printf.sprintf "Schema --%s requires a value.\n\n%s" optionName
           usageText)
    else Ok optionValue
  in
  let setOption optionName optionValue currentValue =
    match currentValue with
    | None -> Ok (Some optionValue)
    | Some _ ->
        Error
          (Printf.sprintf "Schema command accepts at most one --%s option."
             optionName)
  in
  let rec parse parsedArguments = function
    | [] -> Ok parsedArguments
    | "--type" :: [] ->
        Error (Printf.sprintf "Schema --type requires a value.\n\n%s" usageText)
    | "--directive" :: [] ->
        Error
          (Printf.sprintf "Schema --directive requires a value.\n\n%s" usageText)
    | "--type" :: typeName :: remainingArguments -> (
        match valueOfOption "type" typeName with
        | Error _ as error -> error
        | Ok typeName -> (
            match setOption "type" typeName parsedArguments.typeName with
            | Ok typeName ->
                parse { parsedArguments with typeName } remainingArguments
            | Error _ as error -> error))
    | "--directive" :: directiveName :: remainingArguments -> (
        match valueOfOption "directive" directiveName with
        | Error _ as error -> error
        | Ok directiveName -> (
            match
              setOption "directive" directiveName parsedArguments.directiveName
            with
            | Ok directiveName ->
                parse { parsedArguments with directiveName } remainingArguments
            | Error _ as error -> error))
    | argument :: remainingArguments
      when String.starts_with ~prefix:"--type=" argument -> (
        let typeName = String.sub argument 7 (String.length argument - 7) in
        match setOption "type" typeName parsedArguments.typeName with
        | Ok typeName ->
            parse { parsedArguments with typeName } remainingArguments
        | Error _ as error -> error)
    | argument :: remainingArguments
      when String.starts_with ~prefix:"--directive=" argument -> (
        let directiveName =
          String.sub argument 12 (String.length argument - 12)
        in
        match
          setOption "directive" directiveName parsedArguments.directiveName
        with
        | Ok directiveName ->
            parse { parsedArguments with directiveName } remainingArguments
        | Error _ as error -> error)
    | argument :: _ ->
        Error
          (Printf.sprintf "Invalid schema argument: %s\n\n%s" argument usageText)
  in
  parse { typeName = None; directiveName = None } arguments

let invocationOfArguments arguments =
  match arguments with
  | "schema" :: [] -> Ok (Some SchemaCommandTypes.fullSchemaCommand)
  | "schema" :: [ "--help" ] | "schema" :: [ "help" ] -> Error usageText
  | "schema" :: schemaArguments -> (
      match parseArguments schemaArguments with
      | Error _ as error -> error
      | Ok { typeName = Some _; directiveName = Some _ } ->
          Error
            (Printf.sprintf
               "Schema command accepts only one of --type or --directive.\n\n%s"
               usageText)
      | Ok { typeName = Some typeName; directiveName = None }
        when typeName <> "" ->
          Ok (Some (SchemaCommandTypes.typeCommand typeName))
      | Ok { typeName = None; directiveName = Some directiveName }
        when directiveName <> "" ->
          Ok (Some (SchemaCommandTypes.directiveCommand directiveName))
      | Ok { typeName = Some _; directiveName = None } ->
          Error
            (Printf.sprintf
               "Schema --type requires a non-empty type name.\n\n%s" usageText)
      | Ok { typeName = None; directiveName = Some _ } ->
          Error
            (Printf.sprintf
               "Schema --directive requires a non-empty directive name.\n\n%s"
               usageText)
      | Ok { typeName = None; directiveName = None } ->
          Error (Printf.sprintf "Invalid schema command.\n\n%s" usageText))
  | _ -> Ok None
