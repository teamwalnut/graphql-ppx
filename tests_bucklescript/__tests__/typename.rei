module MyQuery: {
  type t = {
    .
    "first": {
      .
      "__typename": string,
      "inner":
        option({
          .
          "__typename": string,
          "inner":
            option({
              .
              "__typename": string,
              "field": string,
            }),
        }),
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
