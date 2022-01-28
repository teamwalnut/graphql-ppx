open Test_shared

module MyQuery =
[%graphql
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
|}]

type qt = MyQuery.t

let my_query =
  (module struct
    type t = qt

    let pp formatter (obj : qt) =
      Format.fprintf formatter
        ("< variousScalars = @[<>< nullablleString = %a ; string = %a ; \
          nullableInt = %a ; int = %a ; nullableFloat = %a ; float = %a ; \
          nullableBoolean = %a ; boolean = %a ; nullableID = %a ; id = %a >@]"
        [@reason.raw_literal
          "< variousScalars = @[<>< nullablleString = %a ; string = %a ; \
           nullableInt = %a ; int = %a ; nullableFloat = %a ; float = %a ; \
           nullableBoolean = %a ; boolean = %a ; nullableID = %a ; id = %a >@]"])
        (Format.pp_print_string |> print_option)
        obj.variousScalars.nullableString Format.pp_print_string
        obj.variousScalars.string
        (Format.pp_print_int |> print_option)
        obj.variousScalars.nullableInt Format.pp_print_int
        obj.variousScalars.int
        (Format.pp_print_float |> print_option)
        obj.variousScalars.nullableFloat Format.pp_print_float
        obj.variousScalars.float
        (Format.pp_print_bool |> print_option)
        obj.variousScalars.nullableBoolean Format.pp_print_bool
        obj.variousScalars.boolean
        (Format.pp_print_string |> print_option)
        obj.variousScalars.nullableID Format.pp_print_string
        obj.variousScalars.id

    let equal (a : qt) (b : qt) =
      a.variousScalars.nullableString = b.variousScalars.nullableString
      && a.variousScalars.string = b.variousScalars.string
      && a.variousScalars.nullableInt = b.variousScalars.nullableInt
      && a.variousScalars.int = b.variousScalars.int
      && a.variousScalars.nullableFloat = b.variousScalars.nullableFloat
      && a.variousScalars.float = b.variousScalars.float
      && a.variousScalars.nullableBoolean = b.variousScalars.nullableBoolean
      && a.variousScalars.boolean = b.variousScalars.boolean
      && a.variousScalars.nullableID = b.variousScalars.nullableID
      && a.variousScalars.id = b.variousScalars.id
  end : Alcotest.TESTABLE
    with type t = qt)

let decodes_non_null_scalars () =
  Alcotest.check my_query "query result equality"
    ({| {
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
    } |}
   |> Yojson.Basic.from_string |> MyQuery.unsafe_fromJson |> MyQuery.parse)
    {
      variousScalars =
        {
          nullableString = Some "a nullable string";
          string = "a string";
          nullableInt = Some 456;
          int = 123;
          nullableFloat = Some 678.5;
          float = 1234.5;
          nullableBoolean = Some false;
          boolean = true;
          nullableID = Some "a nullable ID";
          id = "an ID";
        };
    }

let decodes_null_scalars () =
  Alcotest.check my_query "query result equality"
    ({| {
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
    } |}
   |> Yojson.Basic.from_string |> MyQuery.unsafe_fromJson |> MyQuery.parse)
    {
      variousScalars =
        {
          nullableString = None;
          string = "a string";
          nullableInt = None;
          int = 123;
          nullableFloat = None;
          float = 1234.5;
          nullableBoolean = None;
          boolean = true;
          nullableID = None;
          id = "an ID";
        };
    }

let decodes_omitted_scalars () =
  Alcotest.check my_query "query result equality"
    ({| {
      "variousScalars": {
        "string": "a string",
        "int": 123,
        "float": 1234.5,
        "boolean": true,
        "id": "an ID"
      }
    } |}
   |> Yojson.Basic.from_string |> MyQuery.unsafe_fromJson |> MyQuery.parse)
    {
      variousScalars =
        {
          nullableString = None;
          string = "a string";
          nullableInt = None;
          int = 123;
          nullableFloat = None;
          float = 1234.5;
          nullableBoolean = None;
          boolean = true;
          nullableID = None;
          id = "an ID";
        };
    }

let tests =
  [
    ("Decodes non-null scalars", `Quick, decodes_non_null_scalars);
    ("Decodes null scalars", `Quick, decodes_null_scalars);
    ("Decodes omitted scalars", `Quick, decodes_omitted_scalars);
  ]
