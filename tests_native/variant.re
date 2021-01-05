open Test_shared;

module MyQuery = [%graphql
  {|
  mutation {
    mutationWithError @bsVariant {
      value {
        stringField
      }

      errors {
        field
        message
      }
    }
  }
|}
];

type qt = {
  .
  mutationWithError: [
    | `Value({. stringField: string})
    | `Errors(
        array({
          .
          field: [ | `FIRST | `SECOND | `THIRD],
          message: string,
        }),
      )
  ],
};

let my_query: module Alcotest.TESTABLE with type t = qt =
  (module
   {
     type t = qt;

     let pp = (formatter, obj: qt) =>
       Format.fprintf(
         formatter,
         "< mutationWithError = %a >",
         formatter =>
           fun
           | `Value(v) =>
             Format.fprintf(
               formatter,
               "`Value @[<>< stringField = %a >@]",
               Format.pp_print_string,
               v#stringField,
             )
           | `Errors(v) =>
             Format.fprintf(
               formatter,
               "`Errors %a",
               print_array((formatter, v) =>
                 Format.fprintf(
                   formatter,
                   "< field = %a ; message = %a >",
                   Format.pp_print_string,
                   switch (v#field) {
                   | `FIRST => "FIRST"
                   | `SECOND => "SECOND"
                   | `THIRD => "THIRD"
                   },
                   Format.pp_print_string,
                   v#message,
                 )
               ),
               v,
             ),
         obj#mutationWithError,
       );

     let equal = (a: qt, b: qt) =>
       switch (a#mutationWithError, b#mutationWithError) {
       | (`Value(a), `Value(b)) => a#stringField == b#stringField
       | (`Errors(a), `Errors(b)) =>
         array_zipmap(
           (a, b) => a#field == b#field && a#message == b#message,
           a,
           b,
         )
         |> Array.to_list
         |> List.for_all(x => x)
       | _ => false
       };
   });

let converts_into_variant = () =>
  Alcotest.check(
    my_query,
    "result equality",
    MyQuery.parse(
      Yojson.Basic.from_string(
        {| {
    "mutationWithError": {
      "value": {
        "stringField": "a string"
      }
    }
  } |},
      ),
    ),
    {
      as _;
      pub mutationWithError = `Value({as _; pub stringField = "a string"})
    },
  );

let tests = [
  ("Converts object into variant", `Quick, converts_into_variant),
];
