type t = {
  operationName : string option;
  queryText : string;
  variables : Yojson.Safe.t option;
  requestedDirectiveName : string option;
}

let fullSchemaCommand =
  {
    operationName = Some "IntrospectionQuery";
    queryText = String.trim SchemaQueries.fullSchemaQueryText;
    variables = None;
    requestedDirectiveName = None;
  }

let typeCommand typeName =
  {
    operationName = Some "IntrospectionTypeQuery";
    queryText = String.trim SchemaQueries.typeQueryText;
    variables = Some (`Assoc [ ("name", `String typeName) ]);
    requestedDirectiveName = None;
  }

let directiveCommand directiveName =
  {
    operationName = Some "IntrospectionDirectiveQuery";
    queryText = String.trim SchemaQueries.directiveQueryText;
    variables = None;
    requestedDirectiveName = Some directiveName;
  }
