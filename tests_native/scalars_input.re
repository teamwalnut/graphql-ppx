open Test_shared;

module MyQuery = [%graphql
  {|
  query ($arg: VariousScalarsInput!) {
    scalarsInput(arg: $arg)
  }
|}
];

let includes_non_nulled_arguments = () =>
  Alcotest.check(
    yojson,
    "json equality",
    MyQuery.make(
      ~arg={
        as _;
        pub nullableString = Some("a nullable string");
        pub string = "a string";
        pub nullableInt = Some(456);
        pub int = 123;
        pub nullableFloat = Some(567.5);
        pub float = 1234.5;
        pub nullableBoolean = Some(false);
        pub boolean = true;
        pub nullableID = Some("a nullable ID");
        pub id = "an ID"
      },
      (),
    )#
      variables,
    Yojson.Basic.from_string(
      {| {
      "arg": {
        "nullableString": "a nullable string",
        "string": "a string",
        "nullableInt": 456,
        "int": 123,
        "nullableFloat": 567.5,
        "float": 1234.5,
        "nullableBoolean": false,
        "boolean": true,
        "nullableID": "a nullable ID",
        "id": "an ID"
      }
    } |},
    ),
  );

let tests = [
  ("Includes non-nulled arguments", `Quick, includes_non_nulled_arguments),
];
