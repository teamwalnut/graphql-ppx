module MyQuery: {
  type t = {. "argNamedQuery": int};

  let make:
    (~query: string, unit) =>
    {
      .
      "parse": Js.Json.t => t,
      "query": string,
      "variables": Js.Json.t,
    };
  let makeWithVariables:
    {. "query": string} =>
    {
      .
      "parse": Js.Json.t => t,
      "query": string,
      "variables": Js.Json.t,
    };
  let query: string;
};
