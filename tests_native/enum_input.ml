open Test_shared

module MyQuery =
[%graphql
{|
  query ($arg: SampleField!) {
    enumInput(arg: $arg)
  }
|}]

let encodes_arguments () =
  Alcotest.check yojson "json"
    (MyQuery.makeVariables ~arg:`FIRST ()
    |> MyQuery.serializeVariables |> MyQuery.variablesToJson)
    (Yojson.Basic.from_string {| { "arg": "FIRST" } |})

let tests = [ ("Encodes enum arguments to strings", `Quick, encodes_arguments) ]
