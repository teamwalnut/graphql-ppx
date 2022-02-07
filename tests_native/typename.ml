open Test_shared

module MyQuery =
[%graphql
{|
  {
    first: nestedObject {
      __typename
      inner {
        __typename
        inner {
          __typename
          field
        }
      }
    }
  }
|}]

type qt = MyQuery.t

let pp formatter (obj : qt) =
  Format.fprintf formatter "< first = @[<>< __typename = %a ; inner = %a >@] >"
    Format.pp_print_string obj.first.__typename
    (fun formatter -> function
      | Some (v : MyQuery.t_first_inner) ->
        Format.fprintf formatter "Some @[<>< __typename = %a ; inner = %a >@]"
          Format.pp_print_string v.__typename
          (fun formatter -> function
            | Some (v : MyQuery.t_first_inner_inner) ->
              Format.fprintf formatter
                "Some @[<>< __typename = %a ; field = %a >@]"
                Format.pp_print_string v.__typename Format.pp_print_string
                v.field
            | None -> Format.fprintf formatter "None")
          v.inner
      | None -> Format.fprintf formatter "None")
    obj.first.inner

let equal (a : qt) (b : qt) =
  a.first.__typename = b.first.__typename
  && opt_eq
       (fun (a : MyQuery.t_first_inner) (b : MyQuery.t_first_inner) ->
         a.__typename = b.__typename
         && opt_eq
              (fun (a : MyQuery.t_first_inner_inner)
                 (b : MyQuery.t_first_inner_inner) ->
                a.__typename = b.__typename && a.field = b.field)
              a.inner b.inner)
       a.first.inner b.first.inner

let decodes_typename () =
  test_exp
    ({|
      {"first": {"__typename": "NestedObject", "inner": null}}
    |}
   |> Yojson.Basic.from_string |> MyQuery.unsafe_fromJson |> MyQuery.parse)
    { first = { __typename = "NestedObject"; inner = None } }
    equal pp

let tests = [ ("Decodes typename as a non-nullable string", decodes_typename) ]
