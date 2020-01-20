module MyQuery: {
  type t = {
    first: t_first,
    second: t_second,
  }
  and t_first = {inner: option(t_first_inner)}
  and t_first_inner = {inner: option(t_first_inner_inner)}
  and t_first_inner_inner = {field: string}
  and t_second = {inner: option(t_second_inner)}
  and t_second_inner = {inner: option(t_second_inner_inner)}
  and t_second_inner_inner = {
    f1: string,
    f2: string,
  };

  let query: string;
  let makeVariables: unit => Js.Json.t;
};
