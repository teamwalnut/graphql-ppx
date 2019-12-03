type record = {
  f1: string,
  f2: string,
};

module MyQuery: {
  type t = {
    .
    "first": {. "inner": option({. "inner": option({. "field": string})})},
    "second": {. "inner": option({. "inner": option(record)})},
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
