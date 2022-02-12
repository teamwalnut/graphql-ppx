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

let omit_nullable_args () =
  test_json_
    (MyQuery.makeVariables ~nonNullableOfNullable:[||]
       ~nonNullableOfNonNullable:[||] ()
    |> MyQuery.serializeVariables |> MyQuery.variablesToJson)
    (Json.Read.from_string
       {| {
      "nullableOfNullable": null,
      "nullableOfNonNullable": null,
      "nonNullableOfNullable": [],
      "nonNullableOfNonNullable": []
    } |})

let allows_none_in_lists () =
  test_json_
    (MyQuery.makeVariables
       ~nullableOfNullable:[| Some "x"; None; Some "y" |]
       ~nonNullableOfNullable:[| Some "a"; None; Some "b" |]
       ~nonNullableOfNonNullable:[| "1"; "2"; "3" |] ()
    |> MyQuery.serializeVariables |> MyQuery.variablesToJson)
    (Json.Read.from_string
       {| {
      "nullableOfNullable": ["x", null, "y"],
      "nullableOfNonNullable": null,
      "nonNullableOfNullable": ["a", null, "b"],
      "nonNullableOfNonNullable": ["1", "2", "3"]
    } |})

let tests =
  [
    ("Can omit nullable arguments", omit_nullable_args);
    ("Allows None in lists with nullable items", allows_none_in_lists);
  ]
