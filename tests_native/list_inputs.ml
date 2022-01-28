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
      Format.fprintf formatter "< listsInput = @[%s@] >" obj.listsInput

    let equal (a : qt) (b : qt) = a.listsInput = b.listsInput
  end : Alcotest.TESTABLE
    with type t = qt)

let allows_none_in_lists_of_nullable () =
  test_json
    (MyQuery.makeVariables
       ~arg:
         {
           nullableOfNullable = Some [| Some "x"; None; Some "y" |];
           nullableOfNonNullable = None;
           nonNullableOfNullable = [| Some "a"; None; Some "b" |];
           nonNullableOfNonNullable = [| "1"; "2"; "3" |];
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
    ( "Allows None in lists of nullable types",
      `Quick,
      allows_none_in_lists_of_nullable );
  ]
