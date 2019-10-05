open Test_shared;

module MyQuery = [%graphql
  {|
  query ($arg: ListsInput!) {
    listsInput(arg: $arg)
  }
|}
];

type qt = {. listsInput: string};

let my_query: module Alcotest.TESTABLE with type t = qt =
  (module
   {
     type t = qt;

     let pp = (formatter, obj: qt) =>
       Format.fprintf(formatter, "< listsInput = @[%s@] >", obj#listsInput);

     let equal = (a: qt, b: qt) => a#listsInput == b#listsInput;
   });

let allows_none_in_lists_of_nullable = () =>
  test_json(
    MyQuery.make(
      ~arg={
        as _;
        pub nullableOfNullable = Some([|Some("x"), None, Some("y")|]);
        pub nullableOfNonNullable = None;
        pub nonNullableOfNullable = [|Some("a"), None, Some("b")|];
        pub nonNullableOfNonNullable = [|"1", "2", "3"|]
      },
      (),
    )#
      variables,
    Yojson.Basic.from_string(
      {| {
      "arg": {
        "nullableOfNullable": ["x", null, "y"],
        "nullableOfNonNullable": null,
        "nonNullableOfNullable": ["a", null, "b"],
        "nonNullableOfNonNullable": ["1", "2", "3"]
      }
    } |},
    ),
  );

let tests = [
  (
    "Allows None in lists of nullable types",
    `Quick,
    allows_none_in_lists_of_nullable,
  ),
];
