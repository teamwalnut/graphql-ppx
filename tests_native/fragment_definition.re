open Test_shared;

type record = {
  nullableOfNullable: option(array(option(string))),
  nullableOfNonNullable: option(array(string)),
};

let concat = ({nullableOfNullable, nullableOfNonNullable}) => {
  let x =
    switch (nullableOfNullable) {
    | None => [||]
    | Some(arr) =>
      arr
      |> Array.map(v =>
           switch (v) {
           | None => [||]
           | Some(s) => [|s|]
           }
         )
      |> Array.to_list
      |> Array.concat
    };

  let y =
    switch (nullableOfNonNullable) {
    | None => [||]
    | Some(a) => a
    };

  Array.append(x, y);
};

module Fragments = [%graphql
  {|
  fragment listFragment on Lists {
    nullableOfNullable
    nullableOfNonNullable
  }

  fragment concatFragment on Lists @bsRecord @bsDecoder(fn: "concat") {
    nullableOfNullable
    nullableOfNonNullable
  }
|}
];

type ft = {
  .
  nullableOfNullable: option(array(option(string))),
  nullableOfNonNullable: option(array(string)),
};

module MyQuery = [%graphql
  {|
  query {
    l1: lists {
      ...Fragments.ListFragment
    }

    l2: lists {
      ...Fragments.ListFragment @bsField(name: "frag1")
      ...Fragments.ListFragment @bsField(name: "frag2")
    }

    l3: lists {
      ...Fragments.ConcatFragment
    }
  }
|}
];

type qt = {
  .
  l1: ft,
  l2: {
    .
    frag1: ft,
    frag2: ft,
  },
  l3: array(string),
};

let print_fragment = (formatter, obj: ft) =>
  Format.fprintf(
    formatter,
    "< nullableOfNullable = @[%a@]; nullableOfNonNullable = @[%a@] >",
    Format.pp_print_string |> print_option |> print_array |> print_option,
    obj#nullableOfNullable,
    Format.pp_print_string |> print_array |> print_option,
    obj#nullableOfNonNullable,
  );

let fragment_equal = (a, b) =>
  a#nullableOfNullable == b#nullableOfNullable
  && a#nullableOfNonNullable == b#nullableOfNonNullable;

let fragment: module Alcotest.TESTABLE with type t = ft =
  (module
   {
     type t = ft;

     let pp = print_fragment;

     let equal = fragment_equal;
   });

let my_query: module Alcotest.TESTABLE with type t = qt =
  (module
   {
     type t = qt;

     let pp = (formatter, obj: qt) =>
       Format.fprintf(
         formatter,
         "< l1 = @[%a@]; l2 = @[<>< frag1 = @[%a@]; frag2 = @[%a@] >@] >",
         print_fragment,
         obj#l1,
         print_fragment,
         obj#l2#frag1,
         print_fragment,
         obj#l2#frag2,
       );

     let equal = (a, b) =>
       fragment_equal(a#l1, b#l1)
       && fragment_equal(a#l2#frag1, b#l2#frag1)
       && fragment_equal(a#l2#frag2, b#l2#frag2)
       && a#l3 == b#l3;
   });

let decodes_the_fragment = () =>
  Alcotest.check(
    my_query,
    "query result equality",
    MyQuery.parse(
      Yojson.Basic.from_string(
        {|
      {
        "l1": {"nullableOfNullable": ["a", null, "b"]},
        "l2": {"nullableOfNullable": ["a", null, "b"]},
        "l3": {
          "nullableOfNullable": ["a", null, "b", null, "c"],
          "nullableOfNonNullable": ["d", "e"]
        }
      }|},
      ),
    ),
    {
      as _;
      pub l1 = {
        as _;
        pub nullableOfNullable = Some([|Some("a"), None, Some("b")|]);
        pub nullableOfNonNullable = None
      };
      pub l2 = {
        as _;
        pub frag1 = {
          as _;
          pub nullableOfNullable = Some([|Some("a"), None, Some("b")|]);
          pub nullableOfNonNullable = None
        };
        pub frag2 = {
          as _;
          pub nullableOfNullable = Some([|Some("a"), None, Some("b")|]);
          pub nullableOfNonNullable = None
        }
      };
      pub l3 = [|"a", "b", "c", "d", "e"|]
    },
  );

let tests = [("Decodes the fragment", `Quick, decodes_the_fragment)];
