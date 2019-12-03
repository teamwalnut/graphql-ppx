module MyQuery: {
  type t = {. "recursiveInput": string};

  let make:
    (
      ~arg: {
              .
              "inner": option('a),
              "enum": option([ | `FIRST | `SECOND | `THIRD]),
              "otherField": option(string),
            } as 'a,
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
          "inner": option('a),
          "enum": option([ | `FIRST | `SECOND | `THIRD]),
          "otherField": option(string),
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
              "inner": option('a),
              "enum": option([ | `FIRST | `SECOND | `THIRD]),
              "otherField": option(string),
            } as 'a,
      unit
    ) =>
    Js.Json.t;
};
