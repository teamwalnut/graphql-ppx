module MyQuery: {
  type t = {
    .
    "variousScalars": {
      .
      "string": int,
      "int": string,
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
