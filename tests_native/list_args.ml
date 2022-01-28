open Test_shared

module MyQuery =
[%graphql
{|
  query (
    $nullableOfNullable: [String],
    $nullableOfNonNullable: [String!],
    $nonNullableOfNullable: [String]!,
    $nonNullableOfNonNullable: [String!]!,
  ) {
    lists: listsInput(arg: {
      nullableOfNullable: $nullableOfNullable,
      nullableOfNonNullable: $nullableOfNonNullable,
      nonNullableOfNullable: $nonNullableOfNullable,
      nonNullableOfNonNullable: $nonNullableOfNonNullable,
    })
  }
|}]

type qt = MyQuery.t

let my_query =
  (module struct
    type t = qt

    let pp formatter (obj : qt) =
      Format.fprintf formatter
        ("< lists = @[%s@] >" [@reason.raw_literal "< lists = @[%s@] >"])
        obj.lists

    let equal (a : qt) (b : qt) = a.lists = b.lists
  end : Alcotest.TESTABLE
    with type t = qt)

let omit_nullable_args () =
  test_json
    (MyQuery.makeVariables ~nonNullableOfNullable:[||]
       ~nonNullableOfNonNullable:[||] ()
    |> MyQuery.serializeVariables |> MyQuery.variablesToJson)
    (Yojson.Basic.from_string
       {| {
      "nullableOfNullable": null,
      "nullableOfNonNullable": null,
      "nonNullableOfNullable": [],
      "nonNullableOfNonNullable": []
    } |})

let allows_none_in_lists () =
  test_json
    (MyQuery.makeVariables
       ~nullableOfNullable:
         [|
           (Some ("x" [@reason.raw_literal "x"]) [@explicit_arity]);
           None;
           (Some ("y" [@reason.raw_literal "y"]) [@explicit_arity]);
         |]
       ~nonNullableOfNullable:
         [|
           (Some ("a" [@reason.raw_literal "a"]) [@explicit_arity]);
           None;
           (Some ("b" [@reason.raw_literal "b"]) [@explicit_arity]);
         |]
       ~nonNullableOfNonNullable:
         [|
           ("1" [@reason.raw_literal "1"]);
           ("2" [@reason.raw_literal "2"]);
           ("3" [@reason.raw_literal "3"]);
         |]
       ()
    |> MyQuery.serializeVariables |> MyQuery.variablesToJson)
    (Yojson.Basic.from_string
       {| {
      "nullableOfNullable": ["x", null, "y"],
      "nullableOfNonNullable": null,
      "nonNullableOfNullable": ["a", null, "b"],
      "nonNullableOfNonNullable": ["1", "2", "3"]
    } |})

let tests =
  [
    ( ("Can omit nullable arguments"
      [@reason.raw_literal "Can omit nullable arguments"]),
      `Quick,
      omit_nullable_args );
    ( ("Allows None in lists with nullable items"
      [@reason.raw_literal "Allows None in lists with nullable items"]),
      `Quick,
      allows_none_in_lists );
  ]
