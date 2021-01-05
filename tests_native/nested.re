open Test_shared;

type record = {
  f1: string,
  f2: string,
};

module MyQuery = [%graphql
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
        inner @bsRecord {
          f1: field
          f2: field
        }
      }
    }
  }
|}
];

type qt = {
  .
  first: {. inner: option({. inner: option({. field: string})})},
  second: {. inner: option({. inner: option(record)})},
};

let my_query: module Alcotest.TESTABLE with type t = qt =
  (module
   {
     type t = qt;

     let pp = (formatter, obj: qt) =>
       Format.fprintf(
         formatter,
         "< first = < inner = @[%a@] > ; second = < inner = @[%a@] > >",
         (
           (formatter, v) =>
             Format.fprintf(
               formatter,
               "< inner = @[%a@] >",
               (
                 (formatter, v) =>
                   Format.fprintf(
                     formatter,
                     "< field = %a >",
                     Format.pp_print_string,
                     v#field,
                   )
               )
               |> print_option,
               v#inner,
             )
         )
         |> print_option,
         obj#first#inner,
         (
           (formatter, v) =>
             Format.fprintf(
               formatter,
               "< inner = @[%a@] >",
               (
                 (formatter, v) =>
                   Format.fprintf(
                     formatter,
                     "{ f1 = %a ; f2 = %a }",
                     Format.pp_print_string,
                     v.f1,
                     Format.pp_print_string,
                     v.f2,
                   )
               )
               |> print_option,
               v#inner,
             )
         )
         |> print_option,
         obj#second#inner,
       );

     let equal = (a: qt, b: qt) =>
       opt_eq(
         (a, b) => opt_eq((a, b) => a#field == b#field, a#inner, b#inner),
         a#first#inner,
         b#first#inner,
       )
       && opt_eq(
            (a, b) =>
              opt_eq(
                (a, b) => a.f1 == b.f1 && a.f2 == b.f2,
                a#inner,
                b#inner,
              ),
            a#second#inner,
            b#second#inner,
          );
   });

let decodes_recursively = () =>
  Alcotest.check(
    my_query,
    "query result equality",
    MyQuery.parse(
      Yojson.Basic.from_string(
        {| {
      "first": {"inner": {"inner": {"field": "second"}}},
      "second": {"inner": null}
    } |},
      ),
    ),
    {
      as _;
      pub first = {
        as _;
        pub inner =
          Some({as _; pub inner = Some({as _; pub field = "second"})})
      };
      pub second = {as _; pub inner = None}
    },
  );

let tests = [("Decodes recursively", `Quick, decodes_recursively)];
