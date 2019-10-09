open Graphql_ppx_base;
open Graphql_ast;
open Result_structure;
open Source_pos;
open Schema;

open Ast_406;
open Asttypes;
open Parsetree;

open Type_utils;
open Generator_utils;
open Output_bucklescript_utils;

exception Unimplemented(string);
exception Invalid_type(string);

let gen_field_array = (loc, typename, field_array_exprs) =>
  Ast_helper.Exp.array(
    switch (typename) {
    | Some(typename) => [
        [@metaloc conv_loc(loc)]
        [%expr
          (
            [%e Ast_helper.Exp.constant(Pconst_string("__typename", None))],
            Js.Json.string([%e typename]),
          )
        ],
        ...field_array_exprs,
      ]
    | _ => field_array_exprs
    },
  );

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
           let expr = Ast_helper.Exp.constant(Pconst_string(evm_name, None));
           Ast_helper.Exp.case(pattern, [%expr Js.Json.string([%e expr])]);
         })
      |> Ast_helper.Exp.match([%expr v]);

    %expr
    (v => [%e encoder]);
  | Res_custom_decoder(loc, ident, inner) =>
    print_endline("Custom decoder: " ++ ident);
    /* TODO: this one is problematic because we should provide both decode and encode
       probably requires changes to the custom decoder result type to support two way serialization */
    %expr
    (v => v);
  | Res_record(loc, name, fields) =>
    [@metaloc conv_loc(loc)]
    [%expr
      (
        v => [%e
          generate_record_fields_encoder(config, loc, fields, None, [%expr v])
        ]
      )
    ]
  | Res_object(loc, name, fields) =>
    [@metaloc conv_loc(loc)]
    [%expr
      (
        v => [%e
          generate_object_fields_encoder(config, loc, fields, None, [%expr v])
        ]
      )
    ]
  | Res_poly_variant_selection_set(loc, name, fields) =>
    // print_endline("Res_poly_variant_selection_set: " ++ name);
    %expr
    (v => v)
  | Res_poly_variant_union(loc, name, fragments, exhaustive) =>
    %expr
    (
      v => [%e
        generage_poly_variant_decoder(~exhaustive, config, loc, fragments)
      ]
    )
  | Res_poly_variant_interface(loc, name, base, fragments) =>
    %expr
    (v => [%e generate_interface_decoder(config, loc, base, fragments)])
  | Res_solo_fragment_spread(loc, name) =>
    let loc = conv_loc(loc);
    let ident =
      Ast_helper.Exp.ident({
        loc,
        txt: Longident.parse(name ++ ".serialize"),
      });
    %expr
    (v => [%e ident](v));
  | Res_error(loc, message) =>
    let loc = conv_loc(loc);
    let ext =
      Location.error(~loc, message)
      |> Ast_mapper.extension_of_error
      |> Ast_helper.Exp.extension(~loc);

    %expr
    (_v => [%e ext]);
  }
and generate_object_fields_encoder = (config, loc, fields, typename, expr) => {
  let field_array_exprs =
    fields
    |> List.map(
         fun
         | Fr_named_field(name, loc, structure) => {
             let parser = generate_encoder(config, structure);
             let ident =
               Output_bucklescript_variables_encoder.ident_from_string(
                 conv_loc(loc),
                 name,
               );

             [@metaloc conv_loc(loc)]
             [%expr
               (
                 [%e Ast_helper.Exp.constant(Pconst_string(name, None))],
                 [%e parser]([%e expr]##[%e ident]),
               )
             ];
           }
         | Fr_fragment_spread(name, loc, fragment) => {
             raise_error_with_loc(
               loc,
               "Fragment spreads with @bsField cannot be serialized, field: "
               ++ name
               ++ ", fragment: "
               ++ fragment,
             );
           },
       );

  let field_array = gen_field_array(loc, typename, field_array_exprs);

  [@metaloc conv_loc(loc)]
  [%expr Js.Json.object_([%e field_array] |> Js.Dict.fromArray)];
}
and generate_record_fields_encoder = (config, loc, fields, typename, expr) => {
  let field_array_exprs =
    fields
    |> List.map(
         fun
         | Fr_named_field(name, loc, structure) => {
             let parser = generate_encoder(config, structure);
             [@metaloc conv_loc(loc)]
             [%expr
               (
                 [%e Ast_helper.Exp.constant(Pconst_string(name, None))],
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
         | Fr_fragment_spread(name, loc, fragment) => {
             raise_error_with_loc(
               loc,
               "Fragment spreads with @bsField cannot be serialized, field: "
               ++ name
               ++ ", fragment: "
               ++ fragment,
             );
           },
       );

  let field_array = gen_field_array(loc, typename, field_array_exprs);

  [@metaloc conv_loc(loc)]
  [%expr Js.Json.object_([%e field_array] |> Js.Dict.fromArray)];
}
and generage_poly_variant_decoder = (~exhaustive, config, loc, fragments) => {
  let fragment_cases =
    fragments
    |> List.map(({poly_variant_name, res_structure, typename}) => {
         let pattern =
           Ast_helper.Pat.variant(
             poly_variant_name,
             Some({
               ppat_desc: Ppat_var({txt: "v", loc: conv_loc(loc)}),
               ppat_loc: conv_loc(loc),
               ppat_attributes: [],
             }),
           );

         let encoder =
           gen_body_encoder(
             config,
             loc,
             res_structure,
             Ast_helper.Exp.constant(Pconst_string(typename, None)),
           );

         Ast_helper.Exp.case(pattern, [%expr [%e encoder](v)]);
       });

  let cases =
    switch (exhaustive) {
    | Exhaustive => fragment_cases
    | Nonexhaustive =>
      raise_error_with_loc(
        loc,
        "Query on unions must be exhaustive to be serializable",
      )
    };

  cases |> Ast_helper.Exp.match([%expr v]);
}
and generate_interface_decoder = (config, loc, base, fragments) => {
  let fragment_cases =
    fragments
    |> List.map(({poly_variant_name, res_structure, typename}) => {
         let pattern =
           Ast_helper.Pat.variant(
             poly_variant_name,
             Some({
               ppat_desc: Ppat_var({txt: "v", loc: conv_loc(loc)}),
               ppat_loc: conv_loc(loc),
               ppat_attributes: [],
             }),
           );

         let encoder =
           gen_body_encoder(
             config,
             loc,
             res_structure,
             Ast_helper.Exp.constant(Pconst_string(typename, None)),
           );

         Ast_helper.Exp.case(pattern, [%expr [%e encoder](v)]);
       });

  let pattern =
    Ast_helper.Pat.variant(
      base.poly_variant_name,
      Some({
        ppat_desc:
          Ppat_tuple([
            {
              ppat_desc: Ppat_var({txt: "typename", loc: conv_loc(loc)}),
              ppat_loc: conv_loc(loc),
              ppat_attributes: [],
            },
            {
              ppat_desc: Ppat_var({txt: "v", loc: conv_loc(loc)}),
              ppat_loc: conv_loc(loc),
              ppat_attributes: [],
            },
          ]),
        ppat_loc: conv_loc(loc),
        ppat_attributes: [],
      }),
    );

  let encoder =
    gen_body_encoder(config, loc, base.res_structure, [%expr typename]);

  let base_case = Ast_helper.Exp.case(pattern, [%expr [%e encoder](v)]);

  Ast_helper.Exp.match([%expr v], [base_case, ...fragment_cases]);
}
and gen_body_encoder = (config, loc, res_structure, typename) =>
  switch (res_structure) {
  | Res_record(loc, name, fields) =>
    [@metaloc conv_loc(loc)]
    [%expr
      (
        v => [%e
          generate_record_fields_encoder(
            config,
            loc,
            fields,
            Some(typename),
            [%expr v],
          )
        ]
      )
    ]
  | Res_object(loc, name, fields) =>
    [@metaloc conv_loc(loc)]
    [%expr
      (
        v => [%e
          generate_object_fields_encoder(
            config,
            loc,
            fields,
            Some(typename),
            [%expr v],
          )
        ]
      )
    ]
  | ty => generate_encoder(config, ty)
  };
