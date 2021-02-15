type scalars = {
  string,
  int,
};

let scalars: module Alcotest.TESTABLE with type t = scalars =
  (module
   {
     type t = scalars;

     let pp = (formatter, obj: scalars) =>
       Format.fprintf(
         formatter,
         "{ string = %a ; int = %a }",
         Format.pp_print_string,
         obj.string,
         Format.pp_print_int,
         obj.int,
       );

     let equal = (a: scalars, b: scalars) =>
       a.string == b.string && a.int == b.int;
   });

module MyQuery = [%graphql
  {|
  {
    variousScalars @ppxAs(type: "scalars") {
      string
      int
    }
  }
|}
];

type qt = MyQuery.t;

let my_query: module Alcotest.TESTABLE with type t = qt =
  (module
   {
     type t = qt;

     let pp = (formatter, obj: qt) =>
       Format.fprintf(
         formatter,
         "< variousScalars = @[%a@] >",
         Alcotest.pp(scalars),
         obj.variousScalars,
       );

     let equal = (a: qt, b: qt) =>
       Alcotest.equal(scalars, a.variousScalars, b.variousScalars);
   });

[%graphql
  {|
  fragment ExternalFragment on VariousScalars @ppxAs(type: "scalars") {
    string
    int
  }

  query ExternalFragmentQuery{
    variousScalars {
      ...ExternalFragment
    }
  }
|}
];

let external_fragment_query:
  module Alcotest.TESTABLE with type t = ExternalFragmentQuery.t =
  (module
   {
     type t = ExternalFragmentQuery.t;

     let pp = (formatter, obj: ExternalFragmentQuery.t) =>
       Format.fprintf(
         formatter,
         "< variousScalars = @[%a@] >",
         Alcotest.pp(scalars),
         obj.variousScalars,
       );

     let equal = (a: ExternalFragmentQuery.t, b: ExternalFragmentQuery.t) =>
       Alcotest.equal(scalars, a.variousScalars, b.variousScalars);
   });

module InlineFragmentQuery = [%graphql
  {|
  {
    dogOrHuman {
      ...on Dog {
        name
        barkVolume
      }
    }
  }
|}
];

type if_qt = InlineFragmentQuery.t;

let inline_fragment_query: module Alcotest.TESTABLE with type t = if_qt =
  (module
   {
     type t = if_qt;

     let pp = (formatter, obj: if_qt) =>
       Format.fprintf(
         formatter,
         "< dogOrHuman = @[%a@] >",
         (formatter, v) =>
           switch (v) {
           | `FutureAddedValue(_) =>
             Format.fprintf(formatter, "`FutureAddedValue")
           | `Dog(dog: InlineFragmentQuery.t_dogOrHuman_Dog) =>
             Format.fprintf(
               formatter,
               "`Dog @[<>< name = %a ; barkVolume = %a >@]",
               Format.pp_print_string,
               dog.name,
               Format.pp_print_float,
               dog.barkVolume,
             )
           },
         obj.dogOrHuman,
       );

     let equal = (a: if_qt, b: if_qt) =>
       switch (a.dogOrHuman, b.dogOrHuman) {
       | (`Dog(a), `Dog(b)) => a.name == b.name
       | (`FutureAddedValue(a), `FutureAddedValue(b)) => a == b
       | _ => false
       };
   });

[%graphql
  {|
  fragment DogFragment on Dog @bsRecord {
    name
    barkVolume
  }

  query UnionExternalFragmentQuery {
    dogOrHuman {
      ...on Dog {
        ...DogFragment
      }
    }
  }
|}
];
let union_external_fragment_query:
  module Alcotest.TESTABLE with type t = UnionExternalFragmentQuery.t =
  (module
   {
     type t = UnionExternalFragmentQuery.t;

     let pp = (formatter, obj: UnionExternalFragmentQuery.t) =>
       Format.fprintf(
         formatter,
         "< dogOrHuman = @[%a@] >",
         (formatter, v) =>
           switch (v) {
           | `FutureAddedValue(_) =>
             Format.fprintf(formatter, "`FutureAddedValue")
           | `Dog(dog: DogFragment.t) =>
             Format.fprintf(
               formatter,
               "`Dog @[<>< name = %a ; barkVolume = %a >@]",
               Format.pp_print_string,
               dog.name,
               Format.pp_print_float,
               dog.barkVolume,
             )
           },
         obj.dogOrHuman,
       );

     let equal =
         (a: UnionExternalFragmentQuery.t, b: UnionExternalFragmentQuery.t) =>
       switch (a.dogOrHuman, b.dogOrHuman) {
       | (`Dog(a), `Dog(b)) => a.name == b.name
       | (`FutureAddedValue(a), `FutureAddedValue(b)) => a == b
       | _ => false
       };
   });

let decodes_record_in_selection = () =>
  Alcotest.check(
    my_query,
    "query result equality",
    {| {"variousScalars": {"string": "a string", "int": 123}} |}
    |> Yojson.Basic.from_string
    |> MyQuery.unsafe_fromJson
    |> MyQuery.parse,
    {
      variousScalars: {
        string: "a string",
        int: 123,
      },
    },
  );

let decodes_record_in_external_fragment = () =>
  Alcotest.check(
    external_fragment_query,
    "query result equality",
    {| {"variousScalars": {"string": "a string", "int": 123}} |}
    |> Yojson.Basic.from_string
    |> ExternalFragmentQuery.unsafe_fromJson
    |> ExternalFragmentQuery.parse,
    {
      variousScalars: {
        string: "a string",
        int: 123,
      },
    },
  );

let decodes_record_in_inline_fragment = () =>
  Alcotest.check(
    inline_fragment_query,
    "query result equality",
    {| {"dogOrHuman": {"__typename": "Dog", "name": "name", "barkVolume": 123.0}} |}
    |> Yojson.Basic.from_string
    |> InlineFragmentQuery.unsafe_fromJson
    |> InlineFragmentQuery.parse,
    {dogOrHuman: `Dog({name: "name", barkVolume: 123.0})},
  );

let decodes_record_in_external_fragment_on_union_selections = () =>
  Alcotest.check(
    union_external_fragment_query,
    "query result equality",
    {| {"dogOrHuman": {"__typename": "Dog", "name": "name", "barkVolume": 123.0}} |}
    |> Yojson.Basic.from_string
    |> UnionExternalFragmentQuery.unsafe_fromJson
    |> UnionExternalFragmentQuery.parse,
    {dogOrHuman: `Dog({name: "name", barkVolume: 123.0})},
  );

let tests = [
  ("Decodes a record in a selection", `Quick, decodes_record_in_selection),
  (
    "Decodes a record in an external fragment",
    `Quick,
    decodes_record_in_external_fragment,
  ),
  (
    "Decodes a record in an inline fragment",
    `Quick,
    decodes_record_in_inline_fragment,
  ),
  (
    "Decodes a record in an external fragment on union selections",
    `Quick,
    decodes_record_in_external_fragment_on_union_selections,
  ),
];
