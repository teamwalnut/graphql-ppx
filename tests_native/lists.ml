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

let my_query =
  (module struct
    type t = qt

    let pp formatter (obj : qt) =
      Format.fprintf formatter
        ("< nullableOfNullable = @[%a@]; nullableOfNonNullable = @[%a@]; \
          nonNullableOfNullable = @[%a@]; nonNullableOfNonNullable = @[%a@] >"
        [@reason.raw_literal
          "< nullableOfNullable = @[%a@]; nullableOfNonNullable = @[%a@]; \
           nonNullableOfNullable = @[%a@]; nonNullableOfNonNullable = @[%a@] >"])
        (Format.pp_print_string |> print_option |> print_array |> print_option)
        obj.lists.nullableOfNullable
        (Format.pp_print_string |> print_array |> print_option)
        obj.lists.nullableOfNonNullable
        (Format.pp_print_string |> print_option |> print_array)
        obj.lists.nonNullableOfNullable
        (Format.pp_print_string |> print_array)
        obj.lists.nonNullableOfNonNullable

    let equal (a : qt) (b : qt) =
      a.lists.nullableOfNullable = b.lists.nullableOfNullable
      && a.lists.nullableOfNonNullable = b.lists.nullableOfNonNullable
      && a.lists.nonNullableOfNullable = b.lists.nonNullableOfNullable
      && a.lists.nonNullableOfNonNullable = b.lists.nonNullableOfNonNullable
  end : Alcotest.TESTABLE
    with type t = qt)

let null_in_lists () =
  Alcotest.check my_query "query result equality"
    (MyQuery.parse
       (Yojson.Basic.from_string
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

let tests = [ ("Null in nullable lists", `Quick, null_in_lists) ]
