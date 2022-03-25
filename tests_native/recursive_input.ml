open Test_shared

module MyQuery =
[%graphql
{|
  query ($arg: RecursiveInput!) {
    recursiveInput(arg: $arg)
  }
|}]

let construct_recursive_input_type () =
  test_json_
    (MyQuery.makeVariables
       ~arg:
         {
           otherField = Some "test";
           enum = None;
           inner =
             Some
               { otherField = Some "inner"; enum = Some `SECOND; inner = None };
         }
       ()
    |> MyQuery.serializeVariables |> MyQuery.variablesToJson)
    (Json.Read.from_string
       {| {
      "arg": {
        "otherField": "test",
        "inner": {
          "otherField": "inner",
          "inner": null,
          "enum": "SECOND"
        },
        "enum": null
      }
    } |})

let tests =
  [ ("Constructing a recursive input type", construct_recursive_input_type) ]
