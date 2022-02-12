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
        print_record
          [
            ( "inner",
              print_option obj.first.inner (fun inner ->
                print_record
                  [
                    ( "inner",
                      print_option inner.inner (fun inner ->
                        print_record
                          [ ("field", print_string_literal inner.field) ]) );
                  ]) );
          ] );
      ( "second",
        print_record
          [
            ( "inner",
              print_option obj.second.inner (fun inner ->
                print_record
                  [
                    ( "inner",
                      print_option inner.inner (fun inner ->
                        print_record
                          [
                            ( "inner",
                              print_record
                                [
                                  ("f1", print_string_literal inner.f1);
                                  ("f2", print_string_literal inner.f2);
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
    (Json.Read.from_string
       {| {
         "first": {"inner": {"inner": {"field": "second"}}},
         "second": {"inner": null}
        }
        |}
    |> MyQuery.unsafe_fromJson |> MyQuery.parse)
    {
      first = { inner = Some { inner = Some { field = "second" } } };
      second = { inner = None };
    }
    equal pp

let tests = [ ("Decodes recursively", decodes_recursively) ]
