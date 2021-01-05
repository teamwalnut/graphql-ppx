open Test_shared;

module MyQuery = [%graphql
  {|
  query ($arg: RecursiveInput!) {
    recursiveInput(arg: $arg)
  }
|}
];

let construct_recursive_input_type = () =>
  Alcotest.check(
    yojson,
    "json equality",
    MyQuery.make(
      ~arg={
        as _;
        pub otherField = Some("test");
        pub enum = None;
        pub inner =
          Some({
            as _;
            pub otherField = Some("inner");
            pub enum = Some(`SECOND);
            pub inner = None
          })
      },
      (),
    )#
      variables,
    Yojson.Basic.from_string(
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
