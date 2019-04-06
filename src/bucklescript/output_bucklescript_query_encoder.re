open Graphql_ppx_base;
open Graphql_ast;
open Source_pos;
open Schema;

open Ast_402;
open Asttypes;

open Type_utils;
open Generator_utils;
open Output_bucklescript_utils;

let generate_encoders = (config, structure) => {
  switch%expr (value) {
  | v => Js.Json.number(float_of_int(1))
  };
};
