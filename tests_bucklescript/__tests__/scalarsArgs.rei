module MyQuery: {
  type t = {. "scalarsInput": string};

  let make:
    (
      ~nullableString: string=?,
      ~string: string,
      ~nullableInt: int=?,
      ~int: int,
      ~nullableFloat: float=?,
      ~float: float,
      ~nullableBoolean: bool=?,
      ~boolean: bool,
      ~nullableID: string=?,
      ~id: string,
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
      "nullableString": option(string),
      "string": string,
      "nullableInt": option(int),
      "int": int,
      "nullableFloat": option(float),
      "float": float,
      "nullableBoolean": option(bool),
      "boolean": bool,
      "nullableID": option(string),
      "id": string,
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
      ~nullableString: string=?,
      ~string: string,
      ~nullableInt: int=?,
      ~int: int,
      ~nullableFloat: float=?,
      ~float: float,
      ~nullableBoolean: bool=?,
      ~boolean: bool,
      ~nullableID: string=?,
      ~id: string,
      unit
    ) =>
    Js.Json.t;
};
