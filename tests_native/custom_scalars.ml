open Test_shared

module My_query =
[%graphql
{|
  query ($opt: CustomScalar, $req: CustomScalar!) {
    customScalarField(argOptional: $opt, argRequired: $req) {
      nullable
      nonNullable
    }
  }
|}]

type t = My_query.t

let equal_payload (a : t) (b : t) =
  a.customScalarField.nullable = b.customScalarField.nullable
  && a.customScalarField.nonNullable = b.customScalarField.nonNullable

let payload_to_str p = p |> My_query.serialize |> My_query.toJson |> pp_json

let test_payload (a : t) (b : t) =
  if equal_payload a b then Pass else Fail (payload_to_str a, payload_to_str b)

let encodes_json_objects () =
  test_json_
    (My_query.makeVariables ~opt:(`Int 123) ~req:(`Int 456) ()
    |> My_query.serializeVariables |> My_query.variablesToJson)
    (Yojson.Basic.from_string {| { "opt": 123, "req": 456 } |})

let decodes_to_json () =
  test_payload
    (Yojson.Basic.from_string
       {|{"customScalarField": { "nullable": 123, "nonNullable": 456 }}|}
    |> My_query.unsafe_fromJson |> My_query.parse)
    {
      customScalarField = { nullable = Some (`Int 123); nonNullable = `Int 456 };
    }

let tests =
  [
    ("Encodes custom scalar variables as JSON objects", encodes_json_objects);
    ("Decodes results to JSON", decodes_to_json);
  ]
