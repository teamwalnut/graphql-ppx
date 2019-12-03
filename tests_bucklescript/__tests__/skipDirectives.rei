module MyQuery: {
  type t = {
    .
    "v1": {
      .
      "nullableString": option(string),
      "string": option(string),
    },
    "v2": {
      .
      "nullableString": option(string),
      "string": option(string),
    },
  };

  let make:
    (~var: bool, unit) =>
    {
      .
      "parse": Js.Json.t => t,
      "query": string,
      "variables": Js.Json.t,
    };

  let makeWithVariables:
    {. "var": bool} =>
    {
      .
      "parse": Js.Json.t => t,
      "query": string,
      "variables": Js.Json.t,
    };

  let query: string;
  let makeVariables: (~var: bool, unit) => Js.Json.t;
};
