open Test_shared

module MyQuery =
[%graphql
{|
  query ($arg: VariousScalarsInput!) {
    scalarsInput(arg: $arg)
  }
|}]

let includes_non_nulled_arguments () =
  test_json_
    (MyQuery.makeVariables
       ~arg:
         {
           nullableString = Some "a nullable string";
           string = "a string";
           nullableInt = Some 456;
           int = 123;
           nullableFloat = Some 567.5;
           float = 1234.5;
           nullableBoolean = Some false;
           boolean = true;
           nullableID = Some "a nullable ID";
           id = "an ID";
         }
       ()
    |> MyQuery.serializeVariables |> MyQuery.variablesToJson)
    (Json.Read.from_string
       {| {
      "arg": {
        "nullableString": "a nullable string",
        "string": "a string",
        "nullableInt": 456,
        "int": 123,
        "nullableFloat": 567.5,
        "float": 1234.5,
        "nullableBoolean": false,
        "boolean": true,
        "nullableID": "a nullable ID",
        "id": "an ID"
      }
    } |})

let tests = [ ("Includes non-nulled arguments", includes_non_nulled_arguments) ]
