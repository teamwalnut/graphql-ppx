open Test_shared;

module MyQuery = [%graphql
  {|
  query ($arg: NonrecursiveInput!) {
    nonrecursiveInput(arg: $arg)
  }
|}
];

type qt = {. nonrecursiveInput: string};

let my_query: module Alcotest.TESTABLE with type t = qt =
  (module
   {
     type t = qt;

     let pp = (formatter, t: qt) =>
       Format.fprintf(
         formatter,
         "< nonrecursiveInput = %a >",
         Format.pp_print_string,
         t#nonrecursiveInput,
       );

     let equal = (a: qt, b: qt) => a#nonrecursiveInput == b#nonrecursiveInput;
   });

let construct_recursive_input_type = () =>
  test_json(
    MyQuery.make(
      ~arg={as _; pub field = Some("test"); pub enum = Some(`SECOND)},
      (),
    )#
      variables,
    Yojson.Basic.from_string(
      {| {
      "arg": {
        "field": "test",
        "enum": "SECOND"
      }
    } |},
    ),
  );

let tests = [
  (
    "Constructing a recursive input type",
    `Quick,
    construct_recursive_input_type,
  ),
];
