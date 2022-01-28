open Test_shared

module MyQuery =
[%graphql
{|
  query ($arg: ListsInput!) {
    listsInput(arg: $arg)
  }
|}]

type qt = MyQuery.t

let my_query =
  (module struct
    type t = qt

    let pp formatter (obj : qt) =
      Format.fprintf formatter
        ("< listsInput = @[%s@] >"
        [@reason.raw_literal "< listsInput = @[%s@] >"]) obj.listsInput

    let equal (a : qt) (b : qt) = a.listsInput = b.listsInput
  end : Alcotest.TESTABLE
    with type t = qt)

let allows_none_in_lists_of_nullable () =
  test_json
    (MyQuery.makeVariables
       ~arg:
         {
           nullableOfNullable =
             Some
               [|
                 (Some ("x" [@reason.raw_literal "x"]) [@explicit_arity]);
                 None;
                 (Some ("y" [@reason.raw_literal "y"]) [@explicit_arity]);
               |]
             [@explicit_arity];
           nullableOfNonNullable = None;
           nonNullableOfNullable =
             [|
               (Some ("a" [@reason.raw_literal "a"]) [@explicit_arity]);
               None;
               (Some ("b" [@reason.raw_literal "b"]) [@explicit_arity]);
             |];
           nonNullableOfNonNullable =
             [|
               ("1" [@reason.raw_literal "1"]);
               ("2" [@reason.raw_literal "2"]);
               ("3" [@reason.raw_literal "3"]);
             |];
         }
       ()
    |> MyQuery.serializeVariables |> MyQuery.variablesToJson)
    (Yojson.Basic.from_string
       {| {
      "arg": {
        "nullableOfNullable": ["x", null, "y"],
        "nullableOfNonNullable": null,
        "nonNullableOfNullable": ["a", null, "b"],
        "nonNullableOfNonNullable": ["1", "2", "3"]
      }
    } |})

let tests =
  [
    ( ("Allows None in lists of nullable types"
      [@reason.raw_literal "Allows None in lists of nullable types"]),
      `Quick,
      allows_none_in_lists_of_nullable );
  ]
