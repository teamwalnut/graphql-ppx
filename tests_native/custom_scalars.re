open Test_shared;

module My_query = [%graphql
  {|
  query ($opt: CustomScalar, $req: CustomScalar!) {
    customScalarField(argOptional: $opt, argRequired: $req) {
      nullable
      nonNullable
    }
  }
|}
];

type qt = My_query.t;

let my_query: module Alcotest.TESTABLE with type t = qt =
  (module
   {
     type t = qt;

     let pp = (formatter, obj: t) =>
       Format.fprintf(
         formatter,
         "< customScalarField = @[<><nullable = @[%a@]; nonNullable = @[%a@] >@] >",
         Yojson.Basic.pretty_print(~std=false) |> print_option,
         obj.customScalarField.nullable,
         Yojson.Basic.pretty_print(~std=false),
         obj.customScalarField.nonNullable,
       );

     let equal = (a: t, b: t) =>
       a.customScalarField.nullable == b.customScalarField.nullable
       && a.customScalarField.nonNullable == b.customScalarField.nonNullable;
   });

let encodes_json_objects = () =>
  test_json(
    My_query.makeVariables(~opt=`Int(123), ~req=`Int(456), ())
    |> My_query.serializeVariables

    |> My_query.variablesToJson,
    Yojson.Basic.from_string({| { "opt": 123, "req": 456 } |}),
  );

let decodes_to_json = () =>
  Alcotest.check(
    my_query,
    "query equality",
    Yojson.Basic.from_string(
      {|{"customScalarField": { "nullable": 123, "nonNullable": 456 }}|},
    )
    |> My_query.unsafe_fromJson
    |> My_query.parse,
    {
      customScalarField: {
        nullable: Some(`Int(123)),
        nonNullable: `Int(456),
      },
    },
  );

let tests = [
  (
    "Encodes custom scalar variables as Json objects",
    `Quick,
    encodes_json_objects,
  ),
  ("Decodes results to JSON", `Quick, decodes_to_json),
];
