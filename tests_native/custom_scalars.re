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

type qt = {
  .
  customScalarField: {
    .
    nullable: option(Yojson.Basic.t),
    nonNullable: Yojson.Basic.t,
  },
};

let my_query: module Alcotest.TESTABLE with type t = qt =
  (module
   {
     type t = qt;

     let pp = (formatter, obj: qt) =>
       Format.fprintf(
         formatter,
         "< customScalarField = @[<><nullable = @[%a@]; nonNullable = @[%a@] >@] >",
         Yojson.Basic.pretty_print(~std=false) |> print_option,
         obj#customScalarField#nullable,
         Yojson.Basic.pretty_print(~std=false),
         obj#customScalarField#nonNullable,
       );

     let equal = (a, b) =>
       a#customScalarField#nullable == b#customScalarField#nullable
       && a#customScalarField#nonNullable == b#customScalarField#nonNullable;
   });

let encodes_json_objects = () =>
  test_json(
    My_query.make(~opt=`Int(123), ~req=`Int(456), ())#variables,
    Yojson.Basic.from_string({| { "opt": 123, "req": 456 } |}),
  );

let encodes_json_objects_from_obj = () =>
  test_json(
    My_query.makeWithVariables({
      as _;
      pub opt = Some(`Int(123));
      pub req = `Int(456)
    })#
      variables,
    Yojson.Basic.from_string({| { "opt": 123, "req": 456 } |}),
  );

let decodes_to_json = () =>
  Alcotest.check(
    my_query,
    "query equality",
    My_query.parse(
      Yojson.Basic.from_string(
        {|{"customScalarField": { "nullable": 123, "nonNullable": 456 }}|},
      ),
    ),
    {
      as _;
      pub customScalarField = {
        as _;
        pub nullable = Some(`Int(123));
        pub nonNullable = `Int(456)
      }
    },
  );

let tests = [
  (
    "Encodes custom scalar variables as Json objects",
    `Quick,
    encodes_json_objects,
  ),
  (
    "Encodes nullable scalar variables as optional Json objects",
    `Quick,
    encodes_json_objects_from_obj,
  ),
  ("Decodes results to JSON", `Quick, decodes_to_json),
];
