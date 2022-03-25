open Test_shared

module MyQuery =
[%graphql
{|
  query ($arg: SampleField!) {
    enumInput(arg: $arg)
  }
|}]

let encodes_arguments () =
  test_json_
    (MyQuery.makeVariables ~arg:`FIRST ()
    |> MyQuery.serializeVariables |> MyQuery.variablesToJson)
    (Json.Read.from_string {| { "arg": "FIRST" } |})

let tests = [ ("Encodes enum arguments to strings", encodes_arguments) ]
