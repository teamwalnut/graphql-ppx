open Test_shared

module MyQuery =
[%graphql
{|
  query ($arg: NonrecursiveInput!) {
    nonrecursiveInput(arg: $arg)
  }
|}]

type qt = MyQuery.t

let construct_recursive_input_type () =
  test_json_
    (MyQuery.makeVariables
       ~arg:
         {
           field = Some "test";
           embeddedInput = Some [| Some { field = Some "test" } |];
           enum = Some `SECOND;
           custom = Some (`Int 1);
           nonNullableField = "Not null";
           nullableArray = None;
         }
       ()
    |> MyQuery.serializeVariables |> MyQuery.variablesToJson)
    (Yojson.Basic.from_string
       {| {
            "arg":
            {
              "nonNullableField":"Not null",
              "nullableArray": null,
              "field": "test",
              "enum": "SECOND",
              "embeddedInput": [ { "field": "test" } ],
              "custom": 1
            }
          } |})

let tests =
  [ ("Constructing a recursive input type", construct_recursive_input_type) ]
