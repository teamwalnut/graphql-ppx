open Base;
open Graphql_ast;
open Result_structure;
open Source_pos;
open Schema;

open Ast_402;
open Asttypes;
open Parsetree;

open Type_utils;
open Generator_utils;
open Output_bucklescript_utils;

exception Unimplemented(string);
exception Invalid_type(string);

let rec generate_encoder = (config, structure) =>
  switch (structure) {
  | Res_nullable(loc, inner) =>
    let child_parser = generate_encoder(config, inner);
    [@metaloc conv_loc(loc)]
    [%expr
      (
        v =>
          switch (v) {
          | None => Js.Json.null
          | Some(v) => [%e child_parser](v)
          }
      )
    ];
  | Res_array(loc, inner) =>
    let child_parser = generate_encoder(config, inner);
    [@metaloc conv_loc(loc)]
    [%expr (v => Js.Json.array(Js.Array.map([%e child_parser], v)))];
  | Res_id(loc)
  | Res_string(loc) =>
    %expr
    Js.Json.string
  | Res_int(loc) =>
    %expr
    (v => Js.Json.number(float_of_int(v)))
  | Res_float(loc) =>
    %expr
    Js.Json.number
  | Res_boolean(loc) =>
    %expr
    Js.Json.boolean
  | Res_raw_scalar(_) =>
    %expr
    (v => v)
  | Res_poly_enum(loc, enum_meta) =>
    let encoder =
      enum_meta.em_values
      |> List.map(({evm_name, _}) => {
           let pattern = Ast_helper.Pat.variant(evm_name, None);
           let expr = Ast_helper.Exp.constant(Const_string(evm_name, None));
           Ast_helper.Exp.case(pattern, [%expr Js.Json.string([%e expr])]);
         })
      |> Ast_helper.Exp.match([%expr v]);

    %expr
    (v => [%e encoder]);
  | Res_custom_decoder(loc, ident, inner) =>
    // TODO: this one is problematic because we should provide both decode and encode
    // probably requires changes to the custom decoder result type to support two way serialization
    %expr
    (v => v)
  | Res_record(loc, name, fields) =>
    [@metaloc conv_loc(loc)]
    [%expr
      (
        v => [%e
          generate_record_fields_encoder(config, loc, fields, [%expr v])
        ]
      )
    ]
  | Res_object(loc, name, fields) =>
    [@metaloc conv_loc(loc)]
    [%expr
      (
        v => [%e
          generate_object_fields_encoder(config, loc, fields, [%expr v])
        ]
      )
    ]
  | Res_poly_variant_selection_set(loc, name, fields) =>
    %expr
    (v => v)
  | Res_poly_variant_union(loc, name, fragments, exhaustive) =>
    %expr
    (v => v)
  | Res_poly_variant_interface(loc, name, base, fragments) =>
    // let encoder = v =>
    //   [base, ...fragments]
    //   |> List.map(((name, structure)) => {
    //        let pattern = Ast_helper.Pat.variant(name, Ptyp_var("v"));
    //        print_endline(name);
    //        Ast_helper.Exp.case(
    //          pattern,
    //          [%expr [%e generate_encoder(config, structure)]],
    //        );
    //      })
    //   |> Ast_helper.Exp.match([%expr v]);

    // %expr
    // (v => [%e encoder([%expr v])]);
    %expr
    (v => v)

  | Res_solo_fragment_spread(loc, name) =>
    %expr
    (v => v)
  | Res_error(loc, message) =>
    let loc = conv_loc(loc);
    let ext =
      Ast_mapper.extension_of_error(Location.error(~loc, message))
      |> Ast_helper.Exp.extension(~loc);

    %expr
    (_v => [%e ext]);
  }
and generate_object_fields_encoder = (config, loc, fields, expr) => {
  let field_array_exprs =
    fields
    |> List.map(
         fun
         | Fr_named_field(name, loc, structure) => {
             let parser = generate_encoder(config, structure);
             [@metaloc conv_loc(loc)]
             [%expr
               (
                 [%e Ast_helper.Exp.constant(Const_string(name, None))],
                 [%e parser](
                   [%e expr]##[%e
                                Output_bucklescript_variables_encoder.ident_from_string(
                                  conv_loc(loc),
                                  name,
                                )
                              ],
                 ),
               )
             ];
           }
         | Fr_fragment_spread(_name, _loc, _string) => [%expr [%e expr]],
       );

  let field_array = Ast_helper.Exp.array(field_array_exprs);
  [@metaloc conv_loc(loc)]
  [%expr Js.Json.object_([%e field_array] |> Js.Dict.fromArray)];
}
and generate_record_fields_encoder = (config, loc, fields, expr) => {
  let field_array_exprs =
    fields
    |> List.map(
         fun
         | Fr_named_field(name, loc, structure) => {
             let parser = generate_encoder(config, structure);
             [@metaloc conv_loc(loc)]
             [%expr
               (
                 [%e Ast_helper.Exp.constant(Const_string(name, None))],
                 [%e parser](
                   [%e
                     Ast_helper.Exp.field(
                       expr,
                       {txt: Longident.parse(name), loc: conv_loc(loc)},
                     )
                   ],
                 ),
               )
             ];
           }
         | Fr_fragment_spread(_name, _loc, _string) => [%expr [%e expr]],
       );

  let field_array = Ast_helper.Exp.array(field_array_exprs);
  [@metaloc conv_loc(loc)]
  [%expr Js.Json.object_([%e field_array] |> Js.Dict.fromArray)];
};