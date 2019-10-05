open Test_shared;

module MyQuery = [%graphql
  {|
  {
    variousScalars {
      nullableString
      string
      nullableInt
      int
      nullableFloat
      float
      nullableBoolean
      boolean
      nullableID
      id
    }
  }
|}
];

type qt = {
  .
  variousScalars: {
    .
    nullableString: option(string),
    string: string,
    nullableInt: option(int),
    int: int,
    nullableFloat: option(float),
    float: float,
    nullableBoolean: option(bool),
    boolean: bool,
    nullableID: option(string),
    id: string,
  },
};

let my_query: module Alcotest.TESTABLE with type t = qt =
  (module
   {
     type t = qt;

     let pp = (formatter, obj: qt) =>
       Format.fprintf(
         formatter,
         "< variousScalars = @[<>< nullablleString = %a ; string = %a ; nullableInt = %a ; int = %a ; nullableFloat = %a ; float = %a ; nullableBoolean = %a ; boolean = %a ; nullableID = %a ; id = %a >@]",
         Format.pp_print_string |> print_option,
         obj#variousScalars#nullableString,
         Format.pp_print_string,
         obj#variousScalars#string,
         Format.pp_print_int |> print_option,
         obj#variousScalars#nullableInt,
         Format.pp_print_int,
         obj#variousScalars#int,
         Format.pp_print_float |> print_option,
         obj#variousScalars#nullableFloat,
         Format.pp_print_float,
         obj#variousScalars#float,
         Format.pp_print_bool |> print_option,
         obj#variousScalars#nullableBoolean,
         Format.pp_print_bool,
         obj#variousScalars#boolean,
         Format.pp_print_string |> print_option,
         obj#variousScalars#nullableID,
         Format.pp_print_string,
         obj#variousScalars#id,
       );

     let equal = (a: qt, b: qt) =>
       a#variousScalars#nullableString == b#variousScalars#nullableString
       && a#variousScalars#string == b#variousScalars#string
       && a#variousScalars#nullableInt == b#variousScalars#nullableInt
       && a#variousScalars#int == b#variousScalars#int
       && a#variousScalars#nullableFloat == b#variousScalars#nullableFloat
       && a#variousScalars#float == b#variousScalars#float
       && a#variousScalars#nullableBoolean == b#variousScalars#nullableBoolean
       && a#variousScalars#boolean == b#variousScalars#boolean
       && a#variousScalars#nullableID == b#variousScalars#nullableID
       && a#variousScalars#id == b#variousScalars#id;
   });

let decodes_non_null_scalars = () =>
  Alcotest.check(
    my_query,
    "query result equality",
    MyQuery.parse(
      Yojson.Basic.from_string(
        {| {
      "variousScalars": {
        "nullableString": "a nullable string",
        "string": "a string",
        "nullableInt": 456,
        "int": 123,
        "nullableFloat": 678.5,
        "float": 1234.5,
        "nullableBoolean": false,
        "boolean": true,
        "nullableID": "a nullable ID",
        "id": "an ID"
      }
    } |},
      ),
    ),
    {
      as _;
      pub variousScalars = {
        as _;
        pub nullableString = Some("a nullable string");
        pub string = "a string";
        pub nullableInt = Some(456);
        pub int = 123;
        pub nullableFloat = Some(678.5);
        pub float = 1234.5;
        pub nullableBoolean = Some(false);
        pub boolean = true;
        pub nullableID = Some("a nullable ID");
        pub id = "an ID"
      }
    },
  );

let decodes_null_scalars = () =>
  Alcotest.check(
    my_query,
    "query result equality",
    MyQuery.parse(
      Yojson.Basic.from_string(
        {| {
      "variousScalars": {
        "nullableString": null,
        "string": "a string",
        "nullableInt": null,
        "int": 123,
        "nullableFloat": null,
        "float": 1234.5,
        "nullableBoolean": null,
        "boolean": true,
        "nullableID": null,
        "id": "an ID"
      }
    } |},
      ),
    ),
    {
      as _;
      pub variousScalars = {
        as _;
        pub nullableString = None;
        pub string = "a string";
        pub nullableInt = None;
        pub int = 123;
        pub nullableFloat = None;
        pub float = 1234.5;
        pub nullableBoolean = None;
        pub boolean = true;
        pub nullableID = None;
        pub id = "an ID"
      }
    },
  );

let decodes_omitted_scalars = () =>
  Alcotest.check(
    my_query,
    "query result equality",
    MyQuery.parse(
      Yojson.Basic.from_string(
        {| {
      "variousScalars": {
        "string": "a string",
        "int": 123,
        "float": 1234.5,
        "boolean": true,
        "id": "an ID"
      }
    } |},
      ),
    ),
    {
      as _;
      pub variousScalars = {
        as _;
        pub nullableString = None;
        pub string = "a string";
        pub nullableInt = None;
        pub int = 123;
        pub nullableFloat = None;
        pub float = 1234.5;
        pub nullableBoolean = None;
        pub boolean = true;
        pub nullableID = None;
        pub id = "an ID"
      }
    },
  );

let tests = [
  ("Decodes non-null scalars", `Quick, decodes_non_null_scalars),
  ("Decodes null scalars", `Quick, decodes_null_scalars),
  ("Decodes omitted scalars", `Quick, decodes_omitted_scalars),
];
