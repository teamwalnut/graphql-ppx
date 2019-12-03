module MyQuery: {
  type t = {
    .
    "lists": {
      .
      "nullableOfNullable": option(array(option(string))),
      "nullableOfNonNullable": option(array(string)),
      "nonNullableOfNullable": array(option(string)),
      "nonNullableOfNonNullable": array(string),
    },
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
  let query: string;
  let makeVariables: unit => Js.Json.t;
};
