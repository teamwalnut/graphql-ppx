module MyQuery: {
  type t = {
    .
    "dogOrHuman": [
      | `Dog(
          {
            .
            "name": string,
            "barkVolume": float,
          },
        )
      | `Human({. "name": string})
    ],
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
