open Test_shared;

module MyQuery = [%graphql
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
|}
];

type qt = {
  .
  v1: {
    .
    nullableString: option(string),
    string: option(string),
  },
  v2: {
    .
    nullableString: option(string),
    string: option(string),
  },
};

let my_query: module Alcotest.TESTABLE with type t = qt =
  (module
   {
     type t = qt;

     let pp = (formatter, obj: qt) =>
       Format.fprintf(
         formatter,
         "< v1 = @[<>< nullableString = %a ; string = %a >@] ; @[<>< nullableString = %a ; string = %a >@] >",
         Format.pp_print_string |> print_option,
         obj#v1#nullableString,
         Format.pp_print_string |> print_option,
         obj#v1#string,
         Format.pp_print_string |> print_option,
         obj#v2#nullableString,
         Format.pp_print_string |> print_option,
         obj#v2#string,
       );

     let equal = (a: qt, b: qt) =>
       a#v1#nullableString == b#v1#nullableString
       && a#v1#string == b#v1#string
       && a#v2#nullableString == b#v2#nullableString
       && a#v2#string == b#v2#string;
   });

let responds_with_none_to_nulled_fields = () =>
  Alcotest.check(
    my_query,
    "query equality",
    MyQuery.parse(
      Yojson.Basic.from_string(
        {|{"v1": {"nullableString": null, "string": null}, "v2": {"nullableString": null, "string": null}}|},
      ),
    ),
    {
      as _;
      pub v1 = {as _; pub nullableString = None; pub string = None};
      pub v2 = {as _; pub nullableString = None; pub string = None}
    },
  );

let responds_with_none_to_omitted_fields = () =>
  Alcotest.check(
    my_query,
    "query equality",
    MyQuery.parse(Yojson.Basic.from_string({|{"v1": {}, "v2": {}}|})),
    {
      as _;
      pub v1 = {as _; pub nullableString = None; pub string = None};
      pub v2 = {as _; pub nullableString = None; pub string = None}
    },
  );

let tests = [
  (
    "Responds with None to nulled fields",
    `Quick,
    responds_with_none_to_nulled_fields,
  ),
  (
    "Responds with None to omitted fields",
    `Quick,
    responds_with_none_to_omitted_fields,
  ),
];
