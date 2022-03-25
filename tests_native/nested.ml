open Test_shared

type record = { f1 : string; f2 : string }

module MyQuery =
[%graphql
{|
  {
    first: nestedObject {
      inner {
        inner {
          field
        }
      }
    }

    second: nestedObject {
      inner {
        inner @ppxAs(type: record) {
          f1: field
          f2: field
        }
      }
    }
  }
|}]

type qt = MyQuery.t

let my_query =
  (module struct
    type t = qt

    let pp formatter (obj : qt) =
      Format.fprintf formatter
        ("< first = < inner = @[%a@] > ; second = < inner = @[%a@] > >"
        [@reason.raw_literal
          "< first = < inner = @[%a@] > ; second = < inner = @[%a@] > >"])
        ((fun formatter (v : MyQuery.t_first_inner) ->
           Format.fprintf formatter "< inner = @[%a@] >"
             ((fun formatter (v : MyQuery.t_first_inner_inner) ->
                Format.fprintf formatter "< field = %a >" Format.pp_print_string
                  v.field)
             |> print_option)
             v.inner)
        |> print_option)
        obj.first.inner
        ((fun formatter (v : MyQuery.t_second_inner) ->
           Format.fprintf formatter "< inner = @[%a@] >"
             ((fun formatter (v : MyQuery.t_second_inner_inner) ->
                Format.fprintf formatter "{ f1 = %a ; f2 = %a }"
                  Format.pp_print_string v.f1 Format.pp_print_string v.f2)
             |> print_option)
             v.inner)
        |> print_option)
        obj.second.inner

    let equal (a : qt) (b : qt) =
      opt_eq
        (fun (a : MyQuery.t_first_inner) (b : MyQuery.t_first_inner) ->
          opt_eq
            (fun (a : MyQuery.t_first_inner_inner)
               (b : MyQuery.t_first_inner_inner) -> a.field = b.field)
            a.inner b.inner)
        a.first.inner b.first.inner
      && opt_eq
           (fun (a : MyQuery.t_second_inner) (b : MyQuery.t_second_inner) ->
             opt_eq
               (fun (a : MyQuery.t_second_inner_inner)
                  (b : MyQuery.t_second_inner_inner) ->
                 a.f1 = b.f1 && a.f2 = b.f2)
               a.inner b.inner)
           a.second.inner b.second.inner
  end : Alcotest.TESTABLE
    with type t = qt)

let decodes_recursively () =
  Alcotest.check my_query "query result equality"
    (Yojson.Basic.from_string
       {| {
      "first": {"inner": {"inner": {"field": "second"}}},
      "second": {"inner": null}
    } |}
    |> MyQuery.unsafe_fromJson |> MyQuery.parse)
    {
      first = { inner = Some { inner = Some { field = "second" } } };
      second = { inner = None };
    }

let tests = [ ("Decodes recursively", `Quick, decodes_recursively) ]
