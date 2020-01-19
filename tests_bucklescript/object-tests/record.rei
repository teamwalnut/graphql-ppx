type scalars = {
  string,
  int,
};

module MyQuery: {
  type t = {. "variousScalars": scalars};

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

  let query: string;
};
