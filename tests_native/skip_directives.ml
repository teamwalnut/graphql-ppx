open Test_shared

module MyQuery =
[%graphql
{|
  query ($var: Boolean!) {
    v1: variousScalars {
      nullableString @skip(if: $var)
      string @skip(if: $var)
    }
    v2: variousScalars {
      nullableString @include(if: $var)
      string @include(if: $var)
    }
  }
|}]

type qt = MyQuery.t

let pp formatter (obj : qt) =
  Format.fprintf formatter
    "< v1 = @[<>< nullableString = %a ; string = %a >@] ; @[<>< nullableString \
     = %a ; string = %a >@] >"
    (Format.pp_print_string |> print_option)
    obj.v1.nullableString
    (Format.pp_print_string |> print_option)
    obj.v1.string
    (Format.pp_print_string |> print_option)
    obj.v2.nullableString
    (Format.pp_print_string |> print_option)
    obj.v2.string

let equal (a : qt) (b : qt) =
  a.v1.nullableString = b.v1.nullableString
  && a.v1.string = b.v1.string
  && a.v2.nullableString = b.v2.nullableString
  && a.v2.string = b.v2.string

let responds_with_none_to_nulled_fields () =
  test_exp
    ({|{"v1": {"nullableString": null, "string": null}, "v2": {"nullableString": null, "string": null}}|}
   |> Yojson.Basic.from_string |> MyQuery.unsafe_fromJson |> MyQuery.parse)
    {
      v1 = { nullableString = None; string = None };
      v2 = { nullableString = None; string = None };
    }
    equal pp

let responds_with_none_to_omitted_fields () =
  test_exp
    ({|{"v1": {}, "v2": {}}|} |> Yojson.Basic.from_string
   |> MyQuery.unsafe_fromJson |> MyQuery.parse)
    {
      v1 = { nullableString = None; string = None };
      v2 = { nullableString = None; string = None };
    }
    equal pp

let tests =
  [
    ("Responds with None to nulled fields", responds_with_none_to_nulled_fields);
    ( "Responds with None to omitted fields",
      responds_with_none_to_omitted_fields );
  ]
