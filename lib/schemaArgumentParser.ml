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

let valueOrMissingValueError optionName optionValue =
  if String.starts_with ~prefix:"--" optionValue then
    Error
      (Printf.sprintf "Schema --%s requires a value.\n\n%s" optionName usageText)
  else Ok optionValue

let setOptionOrConflictError optionName optionValue currentValue =
  match currentValue with
  | None -> Ok (Some optionValue)
  | Some _ ->
      Error
        (Printf.sprintf "Schema command accepts at most one --%s option."
           optionName)

let ( let* ) = Result.bind

let parseArguments arguments =
  let applyOption optionName optionValue currentValue updateParsed remaining
      parse parsedArguments =
    let* checkedValue = valueOrMissingValueError optionName optionValue in
    let* updatedValue =
      setOptionOrConflictError optionName checkedValue currentValue
    in
    parse (updateParsed parsedArguments updatedValue) remaining
  in
  let rec parse parsedArguments = function
    | [] -> Ok parsedArguments
    | "--type" :: [] ->
        Error (Printf.sprintf "Schema --type requires a value.\n\n%s" usageText)
    | "--directive" :: [] ->
        Error
          (Printf.sprintf "Schema --directive requires a value.\n\n%s" usageText)
    | "--type" :: value :: rest ->
        applyOption "type" value parsedArguments.typeName
          (fun parsed typeName -> { parsed with typeName })
          rest parse parsedArguments
    | "--directive" :: value :: rest ->
        applyOption "directive" value parsedArguments.directiveName
          (fun parsed directiveName -> { parsed with directiveName })
          rest parse parsedArguments
    | argument :: rest when String.starts_with ~prefix:"--type=" argument ->
        let value = String.sub argument 7 (String.length argument - 7) in
        let* updatedValue =
          setOptionOrConflictError "type" value parsedArguments.typeName
        in
        parse { parsedArguments with typeName = updatedValue } rest
    | argument :: rest when String.starts_with ~prefix:"--directive=" argument
      ->
        let value = String.sub argument 12 (String.length argument - 12) in
        let* updatedValue =
          setOptionOrConflictError "directive" value
            parsedArguments.directiveName
        in
        parse { parsedArguments with directiveName = updatedValue } rest
    | argument :: _ ->
        Error
          (Printf.sprintf "Invalid schema argument: %s\n\n%s" argument usageText)
  in
  parse { typeName = None; directiveName = None } arguments

let commandOfParsedArguments = function
  | { typeName = Some _; directiveName = Some _ } ->
      Error
        (Printf.sprintf
           "Schema command accepts only one of --type or --directive.\n\n%s"
           usageText)
  | { typeName = Some name; directiveName = None } when name <> "" ->
      Ok (Some (SchemaCommandTypes.typeCommand name))
  | { typeName = None; directiveName = Some name } when name <> "" ->
      Ok (Some (SchemaCommandTypes.directiveCommand name))
  | { typeName = Some _; directiveName = None } ->
      Error
        (Printf.sprintf "Schema --type requires a non-empty type name.\n\n%s"
           usageText)
  | { typeName = None; directiveName = Some _ } ->
      Error
        (Printf.sprintf
           "Schema --directive requires a non-empty directive name.\n\n%s"
           usageText)
  | { typeName = None; directiveName = None } ->
      Error (Printf.sprintf "Invalid schema command.\n\n%s" usageText)

let invocationOfArguments = function
  | "schema" :: [] -> Ok (Some SchemaCommandTypes.fullSchemaCommand)
  | "schema" :: [ "--help" ] | "schema" :: [ "help" ] -> Error usageText
  | "schema" :: schemaArguments ->
      Result.bind (parseArguments schemaArguments) commandOfParsedArguments
  | _ -> Ok None
