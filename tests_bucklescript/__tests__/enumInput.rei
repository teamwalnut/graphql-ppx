module MyQuery: {
  type t = {. "enumInput": string};

  let make:
    (~arg: [ | `FIRST | `SECOND | `THIRD], unit) =>
    {
      .
      "parse": Js.Json.t => t,
      "query": string,
      "variables": Js.Json.t,
    };
  let makeWithVariables:
    {. "arg": [ | `FIRST | `SECOND | `THIRD]} =>
    {
      .
      "parse": Js.Json.t => t,
      "query": string,
      "variables": Js.Json.t,
    };

  let query: string;
  let makeVariables: (~arg: [ | `FIRST | `SECOND | `THIRD], unit) => Js.Json.t;
};
