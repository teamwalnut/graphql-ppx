module MyQuery: {
  type t = {. "nonrecursiveInput": string};

  let make:
    (
      ~arg: {
              .
              "enum": option([ | `FIRST | `SECOND | `THIRD]),
              "field": option(string),
            },
      unit
    ) =>
    {
      .
      "parse": Js.Json.t => t,
      "query": string,
      "variables": Js.Json.t,
    };
  let makeWithVariables:
    {
      .
      "arg":
        {
          .
          "enum": option([ | `FIRST | `SECOND | `THIRD]),
          "field": option(string),
        } as 'a,
    } =>
    {
      .
      "parse": Js.Json.t => t,
      "query": string,
      "variables": Js.Json.t,
    };

  let query: string;
  let makeVariables:
    (
      ~arg: {
              .
              "enum": option([ | `FIRST | `SECOND | `THIRD]),
              "field": option(string),
            },
      unit
    ) =>
    Js.Json.t;
};
