type record = {
  nullableOfNullable: option(array(option(string))),
  nullableOfNonNullable: option(array(string)),
};

module Fragments: {
  module ListFragment: {
    type t = {
      .
      "nullableOfNullable": option(array(option(string))),
      "nullableOfNonNullable": option(array(string)),
    };

    let query: string;
    let name: string;
    let parse: Js.Json.t => t;
  };
};

module MyQuery: {
  type t = {
    .
    "l1": {
      .
      "nullableOfNullable": option(array(option(string))),
      "nullableOfNonNullable": option(array(string)),
    },
    "l2": {
      .
      "frag1": {
        .
        "nullableOfNullable": option(array(option(string))),
        "nullableOfNonNullable": option(array(string)),
      },
      "frag2": {
        .
        "nullableOfNullable": option(array(option(string))),
        "nullableOfNonNullable": option(array(string)),
      },
    },
    "l3": array(string),
  };

  let make:
    unit =>
    {
      .
      "parse": Js.Json.t => t,
      "query": string,
      "variables": Js.Json.t,
    };
  let makeWithVariables:
    Js.t({.}) =>
    {
      .
      "parse": Js.Json.t => t,
      "query": string,
      "variables": Js.Json.t,
    };
  let makeVariables: unit => Js.Json.t;
};