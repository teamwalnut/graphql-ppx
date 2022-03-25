open Test_shared

module MyQuery =
[%graphql
{|
  {
    lists {
      nullableOfNullable
      nullableOfNonNullable
      nonNullableOfNullable
      nonNullableOfNonNullable
    }
  }
|}]

type qt = MyQuery.t

let pp fmt (obj : qt) =
  pp_record fmt
    [
      ( "nullableOfNullable",
        fun fmt ->
          (Format.pp_print_string |> pp_option |> pp_array |> pp_option)
            fmt obj.lists.nullableOfNullable );
      ( "nullableOfNonNullable",
        fun fmt ->
          (Format.pp_print_string |> pp_array |> pp_option)
            fmt obj.lists.nullableOfNonNullable );
      ( "nonNullableOfNullable",
        fun fmt ->
          (Format.pp_print_string |> pp_option |> pp_array)
            fmt obj.lists.nonNullableOfNullable );
      ( "nonNullableOfNonNullable",
        fun fmt ->
          (Format.pp_print_string |> pp_array)
            fmt obj.lists.nonNullableOfNonNullable );
    ]

let equal (a : qt) (b : qt) =
  a.lists.nullableOfNullable = b.lists.nullableOfNullable
  && a.lists.nullableOfNonNullable = b.lists.nullableOfNonNullable
  && a.lists.nonNullableOfNullable = b.lists.nonNullableOfNullable
  && a.lists.nonNullableOfNonNullable = b.lists.nonNullableOfNonNullable

let null_in_lists () =
  test_exp
    (MyQuery.parse
       (Json.Read.from_string
          {|{"lists": {"nullableOfNullable": [null, "123"], "nonNullableOfNullable": [null, "123"], "nonNullableOfNonNullable": ["a", "b"]}}|}
       |> MyQuery.unsafe_fromJson))
    {
      lists =
        {
          nullableOfNullable = Some [| None; Some "123" |];
          nullableOfNonNullable = None;
          nonNullableOfNullable = [| None; Some "123" |];
          nonNullableOfNonNullable = [| "a"; "b" |];
        };
    }
    equal pp

let tests = [ ("Null in nullable lists", null_in_lists) ]
