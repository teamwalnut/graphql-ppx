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

let pp fmt (obj : qt) =
  pp_record fmt
    [
      ( "first",
        pp_inner_record
          [
            ( "inner",
              pp_inner_option obj.first.inner (fun inner ->
                pp_inner_record
                  [
                    ( "inner",
                      pp_inner_option inner.inner (fun inner ->
                        pp_inner_record
                          [ ("field", pp_inner_string_literal inner.field) ]) );
                  ]) );
          ] );
      ( "second",
        pp_inner_record
          [
            ( "inner",
              pp_inner_option obj.second.inner (fun inner ->
                pp_inner_record
                  [
                    ( "inner",
                      pp_inner_option inner.inner (fun inner ->
                        pp_inner_record
                          [
                            ( "inner",
                              pp_inner_record
                                [
                                  ("f1", pp_inner_string_literal inner.f1);
                                  ("f2", pp_inner_string_literal inner.f2);
                                ] );
                          ]) );
                  ]) );
          ] );
    ]

let equal (a : qt) (b : qt) =
  opt_eq
    (fun (a : MyQuery.t_first_inner) (b : MyQuery.t_first_inner) ->
      opt_eq
        (fun (a : MyQuery.t_first_inner_inner) (b : MyQuery.t_first_inner_inner) ->
          a.field = b.field)
        a.inner b.inner)
    a.first.inner b.first.inner
  && opt_eq
       (fun (a : MyQuery.t_second_inner) (b : MyQuery.t_second_inner) ->
         opt_eq
           (fun (a : MyQuery.t_second_inner_inner)
              (b : MyQuery.t_second_inner_inner) -> a.f1 = b.f1 && a.f2 = b.f2)
           a.inner b.inner)
       a.second.inner b.second.inner

let decodes_recursively () =
  test_exp
    (Yojson.Basic.from_string
       {| {
      "first": {"inner": {"inner": {"field": "secon"}}},
      "second": {"inner": null}
    } |}
    |> MyQuery.unsafe_fromJson |> MyQuery.parse)
    {
      first = { inner = Some { inner = Some { field = "second" } } };
      second = { inner = None };
    }
    equal pp

let tests = [ ("Decodes recursively", decodes_recursively) ]
