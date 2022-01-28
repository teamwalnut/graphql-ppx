open Test_shared

module MyQuery =
[%graphql
{|
  query ($arg: NonrecursiveInput!) {
    nonrecursiveInput(arg: $arg)
  }
|}]

type qt = MyQuery.t

let my_query =
  (module struct
    type t = qt

    let pp formatter (t : qt) =
      Format.fprintf formatter "< nonrecursiveInput = %a >"
        Format.pp_print_string t.nonrecursiveInput

    let equal (a : qt) (b : qt) = a.nonrecursiveInput = b.nonrecursiveInput
  end : Alcotest.TESTABLE
    with type t = qt)

let construct_recursive_input_type () =
  test_json
    (MyQuery.makeVariables
       ~arg:
         {
           field = Some "test";
           embeddedInput = Some [| Some { field = Some "test" } |];
           enum = Some `SECOND;
           custom = Some (`Int 1);
           nonNullableField = "Not null";
           nullableArray = None;
         }
       ()
    |> MyQuery.serializeVariables |> MyQuery.variablesToJson)
    (Yojson.Basic.from_string
       {| {
            "arg":
            {
              "nonNullableField":"Not null",
              "nullableArray": null,
              "field": "test",
              "enum": "SECOND",
              "embeddedInput": [ { "field": "test" } ],
              "custom": 1
            }
          } |})

let tests =
  [
    ( "Constructing a recursive input type",
      `Quick,
      construct_recursive_input_type );
  ]
