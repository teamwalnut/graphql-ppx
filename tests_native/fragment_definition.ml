open Test_shared;;

[%graphql
  {|
  fragment ListFragment on Lists {
    nullableOfNullable
    nullableOfNonNullable
  }
|}]

module MyQuery =
[%graphql
{|
  query {
    l1: lists {
      ...ListFragment
    }

    l2: lists {
      ...ListFragment @bsField(name: "frag1")
      ...ListFragment @bsField(name: "frag2")
    }
  }
|}]

type qt = MyQuery.t
type ft = ListFragment.t

let print_fragment formatter (obj : ft) =
  Format.fprintf formatter
    "{ nullableOfNullable = @[%a@]; nullableOfNonNullable = @[%a@] }"
    (Format.pp_print_string |> print_option |> print_array |> print_option)
    obj.nullableOfNullable
    (Format.pp_print_string |> print_array |> print_option)
    obj.nullableOfNonNullable

let fragment_equal (a : ft) (b : ft) =
  a.nullableOfNullable = b.nullableOfNullable
  && a.nullableOfNonNullable = b.nullableOfNonNullable

let pp_payload (obj : qt) formatter () =
  Format.fprintf formatter
    "{ l1 = @[%a@]; l2 = @[<>< frag1 = @[%a@]; frag2 = @[%a@] >@] }"
    print_fragment obj.l1 print_fragment obj.l2.frag1 print_fragment
    obj.l2.frag2

let payload_equal (a : qt) (b : qt) =
  fragment_equal a.l1 b.l1
  && fragment_equal a.l2.frag1 b.l2.frag1
  && fragment_equal a.l2.frag2 b.l2.frag2

let decodes_the_fragment () =
  test_exp
    ({|{
        "l1": {"nullableOfNullable": ["a", null, "b"]},
        "l2": {"nullableOfNullable": ["a", null, "b"]}
      }
    |}
   |> Yojson.Basic.from_string |> MyQuery.unsafe_fromJson |> MyQuery.parse)
    {
      l1 =
        {
          nullableOfNullable = Some [| Some "a"; None; Some "b" |];
          nullableOfNonNullable = None;
        };
      l2 =
        {
          frag1 =
            {
              nullableOfNullable = Some [| Some "a"; None; Some "b" |];
              nullableOfNonNullable = None;
            };
          frag2 =
            {
              nullableOfNullable = Some [| Some "a"; None; Some "b" |];
              nullableOfNonNullable = None;
            };
        };
    }
    payload_equal pp_payload

let tests = [ ("Decodes the fragment", decodes_the_fragment) ]
