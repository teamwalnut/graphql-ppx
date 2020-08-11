open Graphql_ast;
open Source_pos;
open Schema;

open Type_utils;
open Generator_utils;
open Result_structure;

let get_with_default = (value, default_value) => {
  switch (value) {
  | Some(value) => Some(value)
  | None => default_value
  };
};

let make_error = (error_marker, map_loc, span, message) => {
  let () = error_marker.has_error = true;
  Res_error({loc: map_loc(span), message});
};

let has_directive = (~prepend=false, name, directives) =>
  List.exists(
    ({item: {d_name: {item, _}, _}, _}) =>
      prepend ? item == "bs" ++ name || item == "ppx" ++ name : item == name,
    directives,
  );

let find_directive = (~prepend=false, name, directives) =>
  switch (
    List.find(
      ({item: {d_name: {item, _}, _}, _}) =>
        prepend ? item == "bs" ++ name || item == "ppx" ++ name : item == name,
      directives,
    )
  ) {
  | d => Some(d)
  | exception Not_found => None
  };

let find_argument = (name, arguments) =>
  arguments
  |> Option.flat_map(({item: arguments, _}) =>
       switch (
         List.find(
           (({item: arg_name, _}, _)) => arg_name == name,
           arguments,
         )
       ) {
       | a => Some(a)
       | exception Not_found => None
       }
     );

let find_fragment_arguments =
    (directives: list(Source_pos.spanning(Graphql_ast.directive))) => {
  switch (directives |> List.find(d => d.item.d_name.item == "arguments")) {
  | {item: {d_arguments: Some(arguments), _}, _} =>
    arguments.item
    |> List.fold_left(
         acc =>
           fun
           | ({item: name, _}, {item: Iv_variable(variable_name), _})
               when name == variable_name => [
               name,
               ...acc,
             ]
           | _ => acc,
         [],
       )
  | _ => []
  | exception Not_found => []
  };
};

let get_ppx_as = directives => {
  switch (directives |> find_directive(~prepend=true, "As")) {
  | None => None
  | Some({item: {d_arguments, _}, _}) =>
    switch (find_argument("type", d_arguments)) {
    | Some((_, {item: Iv_string(type_name), _})) => Some(type_name)
    | _ => None
    }
  };
};

let rec unify_type =
        (
          ~has_decoder,
          error_marker,
          as_record,
          existing_record,
          omit_future_value,
          config,
          span,
          ty,
          selection_set: option(spanning(list(selection))),
        ) =>
  switch (ty) {
  | Ntr_nullable(t) =>
    Res_nullable({
      loc: config.map_loc(span),
      inner:
        unify_type(
          ~has_decoder,
          error_marker,
          as_record,
          existing_record,
          omit_future_value,
          config,
          span,
          t,
          selection_set,
        ),
    })
  | Ntr_list(t) =>
    Res_array({
      loc: config.map_loc(span),
      inner:
        unify_type(
          ~has_decoder,
          error_marker,
          as_record,
          existing_record,
          omit_future_value,
          config,
          span,
          t,
          selection_set,
        ),
    })
  | Ntr_named(n) =>
    switch (lookup_type(config.schema, n)) {
    | None => raise_error(config.map_loc, span, "Could not find type " ++ n)
    | Some(Scalar({sm_name: "ID", _}))
    | Some(Scalar({sm_name: "String", _})) =>
      Res_string({loc: config.map_loc(span)})
    | Some(Scalar({sm_name: "Int", _})) =>
      Res_int({loc: config.map_loc(span)})
    | Some(Scalar({sm_name: "Float", _})) =>
      Res_float({loc: config.map_loc(span)})
    | Some(Scalar({sm_name: "Boolean", _})) =>
      Res_boolean({loc: config.map_loc(span)})
    | Some(Scalar({sm_name})) when !has_decoder =>
      try({
        let decoderModule = Hashtbl.find(Ppx_config.custom_fields(), sm_name);
        Res_custom_decoder({
          loc: config.map_loc(span),
          ident: decoderModule,
          inner: Res_raw_scalar({loc: config.map_loc(span)}),
        });
      }) {
      | Not_found => Res_raw_scalar({loc: config.map_loc(span)})
      | other => raise(other)
      }
    | Some(Scalar(_)) => Res_raw_scalar({loc: config.map_loc(span)})
    | Some(Object(_) as ty) =>
      unify_selection_set(
        error_marker,
        as_record,
        existing_record,
        config,
        span,
        ty,
        selection_set,
      )
    | Some(Enum(enum_meta)) =>
      Res_poly_enum({
        loc: config.map_loc(span),
        enum_meta,
        omit_future_value,
      })
    | Some(Interface(im) as ty) =>
      unify_interface(
        error_marker,
        as_record,
        config,
        span,
        im,
        ty,
        selection_set,
        existing_record,
      )
    | Some(InputObject(_)) =>
      make_error(
        error_marker,
        config.map_loc,
        span,
        "Can't have fields on input objects",
      )
    | Some(Union(um)) =>
      unify_union(
        error_marker,
        config,
        span,
        um,
        omit_future_value,
        selection_set,
      )
    }
  }
and unify_interface =
    (
      error_marker,
      _as_record,
      config,
      span,
      interface_meta,
      ty,
      selection_set,
      existing_record,
    ) =>
  switch (selection_set) {
  | None =>
    make_error(
      error_marker,
      config.map_loc,
      span,
      "Interface types must have subselections",
    )
  | Some(selection_set) =>
    let unwrap_type_conds = ((selections, fragments), selection) =>
      switch (selection) {
      | InlineFragment({item: {if_type_condition: None, _}, span}) =>
        raise_error(
          config.map_loc,
          span,
          "Inline fragments must have a type condition",
        )
      | InlineFragment(frag) => (selections, [frag, ...fragments])
      | selection => ([selection, ...selections], fragments)
      };

    let (base_selection_set, fragments) =
      List.fold_left(unwrap_type_conds, ([], []), selection_set.item);

    let generate_case = (selection, ty, name) => (
      name,
      Res_object({
        loc: config.map_loc(span),
        name,
        fields:
          List.map(unify_selection(error_marker, config, ty), selection),
        type_name: None,
        interface_fragments: None,
      }),
    );

    let generate_fragment_case =
        ({item: {if_type_condition, if_selection_set, _}, _}) =>
      switch (if_type_condition) {
      | Some(if_type_condition) =>
        let {item, _} = if_selection_set;
        let selection = item;
        let ty =
          switch (lookup_type(config.schema, if_type_condition.item)) {
          | Some(ty) => ty
          | None => ty
          };

        generate_case(selection, ty, if_type_condition.item);
      | None => assert(false)
      };

    let fragment_cases = List.map(generate_fragment_case, fragments);

    let interface =
      Res_poly_variant_interface({
        loc: config.map_loc(span),
        name: interface_meta.im_name,
        fragments: fragment_cases,
      });

    switch (base_selection_set) {
    | [] => interface
    | _ =>
      Res_object({
        loc: config.map_loc(span),
        name: interface_meta.im_name,
        fields:
          List.map(
            unify_selection(error_marker, config, ty),
            base_selection_set,
          ),
        type_name: existing_record,
        interface_fragments: Some((interface_meta.im_name, fragment_cases)),
      })
    };
  }
and unify_union =
    (error_marker, config, span, union_meta, omit_future_value, selection_set) =>
  switch (selection_set) {
  | None =>
    make_error(
      error_marker,
      config.map_loc,
      span,
      "Union types must have subselections",
    )
  | Some(selection_set) =>
    let unwrap_type_conds = selection =>
      switch (selection) {
      | Field({span, _})
      | FragmentSpread({span, _}) =>
        raise_error(
          config.map_loc,
          span,
          "Only inline fragments can appear on unions",
        )
      | InlineFragment({item: {if_type_condition: None, _}, span}) =>
        raise_error(
          config.map_loc,
          span,
          "Inline fragments must have a type condition",
        )
      | InlineFragment(frag) => frag
      };

    let type_cond_name = ({item: {if_type_condition, _}, _}) =>
      switch (if_type_condition) {
      | Some({item, _}) => item
      | None => assert(false)
      };

    let generate_case =
        ({item: {if_type_condition, if_selection_set, if_directives}, _}) =>
      switch (if_type_condition) {
      | Some(if_type_condition) =>
        let type_cond_ty =
          switch (lookup_type(config.schema, if_type_condition.item)) {
          | None =>
            raise_error(
              config.map_loc,
              if_type_condition.span,
              "Could not find type",
            )
          | Some(ty) => ty
          };

        let is_record = has_directive(~prepend=true, "Record", if_directives);
        let existing_record = get_ppx_as(if_directives);

        let result_decoder =
          unify_selection_set(
            error_marker,
            is_record,
            existing_record,
            config,
            if_selection_set.span,
            type_cond_ty,
            Some(if_selection_set),
          );
        (if_type_condition, result_decoder);
      | None => assert(false)
      };

    let fragments = List.map(unwrap_type_conds, selection_set.item);
    let covered_cases =
      List.map(type_cond_name, fragments) |> List.sort(compare);
    let possible_cases = List.sort(compare, union_meta.um_of_types);

    Res_poly_variant_union({
      loc: config.map_loc(span),
      name: union_meta.um_name,
      fragments: List.map(generate_case, fragments),
      exhaustive:
        if (covered_cases == possible_cases) {
          Exhaustive;
        } else {
          Nonexhaustive;
        },
      omit_future_value,
    });
  }
and unify_variant = (error_marker, config, span, ty, selection_set) =>
  switch (ty) {
  | Ntr_nullable(t) =>
    Res_nullable({
      loc: config.map_loc(span),
      inner: unify_variant(error_marker, config, span, t, selection_set),
    })
  | Ntr_list(t) =>
    Res_array({
      loc: config.map_loc(span),
      inner: unify_variant(error_marker, config, span, t, selection_set),
    })
  | Ntr_named(n) =>
    switch (lookup_type(config.schema, n)) {
    | None =>
      make_error(
        error_marker,
        config.map_loc,
        span,
        "Could not find type " ++ n,
      )
    | Some(Scalar(_))
    | Some(Enum(_))
    | Some(Interface(_))
    | Some(Union(_))
    | Some(InputObject(_)) =>
      make_error(
        error_marker,
        config.map_loc,
        span,
        "Variant fields can only be applied to object types",
      )
    | Some(Object(_) as ty) =>
      switch (selection_set) {
      | None =>
        make_error(
          error_marker,
          config.map_loc,
          span,
          "Variant fields need a selection set",
        )
      | Some({item, _}) =>
        let fields =
          item
          |> List.map(selection =>
               switch (selection) {
               | Field({item, _}) =>
                 switch (lookup_field(ty, item.fd_name.item)) {
                 | None =>
                   raise_error(
                     config.map_loc,
                     span,
                     "Unknown field on type " ++ type_name(ty),
                   )
                 | Some(field_meta) =>
                   let key = some_or(item.fd_alias, item.fd_name);
                   let inner_type =
                     switch (to_native_type_ref(field_meta.fm_field_type)) {
                     | Ntr_list(_)
                     | Ntr_named(_) =>
                       raise_error(
                         config.map_loc,
                         span,
                         "Variant field must only contain nullable fields",
                       )
                     | Ntr_nullable(i) => i
                     };
                   (
                     key,
                     unify_type(
                       ~has_decoder=false,
                       error_marker,
                       false,
                       None,
                       false,
                       config,
                       span,
                       inner_type,
                       item.fd_selection_set,
                     ),
                   );
                 }
               | FragmentSpread({span, _}) =>
                 raise_error(
                   config.map_loc,
                   span,
                   "Variant selections can only contain fields",
                 )
               | InlineFragment({span, _}) =>
                 raise_error(
                   config.map_loc,
                   span,
                   "Variant selections can only contain fields",
                 )
               }
             );

        Res_poly_variant_selection_set({
          loc: config.map_loc(span),
          name: n,
          fragments: fields,
        });
      }
    }
  }
and unify_field = (error_marker, config, field_span, ty) => {
  let ast_field = field_span.item;
  let field_name = ast_field.fd_name.item;
  let field_meta = lookup_field(ty, field_name);
  let key = some_or(ast_field.fd_alias, ast_field.fd_name);
  let key_span = key.span;
  let key = key.item;
  let is_variant =
    has_directive(~prepend=true, "Variant", ast_field.fd_directives);
  let is_record =
    has_directive(~prepend=true, "Record", ast_field.fd_directives);
  let omit_future_value =
    has_directive(~prepend=true, "OmitFutureValue", ast_field.fd_directives)
    || !config.future_added_value;
  let has_decoder =
    has_directive(~prepend=true, "Decoder", ast_field.fd_directives)
    || has_directive(~prepend=true, "Custom", ast_field.fd_directives);

  let existing_record = get_ppx_as(ast_field.fd_directives);

  let has_skip =
    has_directive("skip", ast_field.fd_directives)
    || has_directive("include", ast_field.fd_directives);
  let sub_unifier =
    if (is_variant) {
      unify_variant(error_marker);
    } else {
      unify_type(
        ~has_decoder,
        error_marker,
        is_record,
        existing_record,
        omit_future_value,
      );
    };

  let parser_expr =
    switch (field_meta) {
    | None =>
      make_error(
        error_marker,
        config.map_loc,
        ast_field.fd_name.span,
        "Unknown field '" ++ field_name ++ "' on type " ++ type_name(ty),
      )
    | Some(field_meta) =>
      let field_ty = to_native_type_ref(field_meta.fm_field_type);
      let sub_unifier =
        sub_unifier(
          config,
          field_span.span,
          field_ty,
          ast_field.fd_selection_set,
        );
      if (has_skip && !is_nullable(field_ty)) {
        Res_nullable({
          loc: config.map_loc(field_span.span),
          inner: sub_unifier,
        });
      } else {
        sub_unifier;
      };
    };

  let loc = config.map_loc(field_span.span);
  let loc_key = config.map_loc(key_span);
  let arguments =
    switch (ast_field.fd_arguments) {
    | None => []
    | Some({item}) => item
    };
  switch (
    switch (
      ast_field.fd_directives |> find_directive(~prepend=true, "Decoder"),
      ast_field.fd_directives |> find_directive(~prepend=true, "Custom"),
    ) {
    | (_, Some(decoder)) => Some(decoder)
    | (Some(decoder), _) => Some(decoder)
    | (None, None) => None
    }
  ) {
  | None =>
    Fr_named_field({name: key, loc_key, loc, type_: parser_expr, arguments})
  | Some({item: {d_arguments, _}, span}) =>
    switch (find_argument("module", d_arguments)) {
    | None =>
      Fr_named_field({
        name: key,
        loc,
        loc_key,
        type_:
          make_error(
            error_marker,
            config.map_loc,
            span,
            "ppxDecoder must be given 'module' argument",
          ),
        arguments,
      })
    | Some((_, {item: Iv_string(module_name), span})) =>
      switch (parser_expr) {
      | Res_nullable({loc, inner: t}) =>
        Fr_named_field({
          name: key,
          loc_key,
          loc,
          type_:
            Res_nullable({
              loc,
              inner:
                Res_custom_decoder({
                  loc: config.map_loc(span),
                  ident: module_name,
                  inner: t,
                }),
            }),
          arguments,
        })
      | Res_array({loc, inner: t}) =>
        Fr_named_field({
          name: key,
          loc_key,
          loc,
          type_:
            Res_array({
              loc,
              inner:
                Res_custom_decoder({
                  loc: config.map_loc(span),
                  ident: module_name,
                  inner: t,
                }),
            }),
          arguments,
        })
      | _ =>
        Fr_named_field({
          name: key,
          loc_key,
          loc,
          type_:
            Res_custom_decoder({
              loc: config.map_loc(span),
              ident: module_name,
              inner: parser_expr,
            }),
          arguments,
        })
      }
    | Some((_, {span, _})) =>
      Fr_named_field({
        name: key,
        loc_key,
        loc,
        type_:
          make_error(
            error_marker,
            config.map_loc,
            span,
            "The 'module' argument must be a string",
          ),
        arguments,
      })
    }
  };
}
and unify_selection = (error_marker, config, ty, selection) =>
  switch (selection) {
  | Field(field_span) => unify_field(error_marker, config, field_span, ty)
  | FragmentSpread({item: {fs_directives, fs_name}, span}) =>
    let arguments = find_fragment_arguments(fs_directives);
    switch (find_directive(~prepend=true, "Field", fs_directives)) {
    | None =>
      let key =
        fs_name.item
        |> String.split_on_char('.')
        |> List.rev
        |> List.hd
        |> String.uncapitalize_ascii;
      Fr_fragment_spread({
        key,
        loc: config.map_loc(span),
        name: fs_name.item,
        type_name:
          switch (ty) {
          | Object({om_name, _}) => Some(om_name)
          | _ => None
          },
        arguments,
      });
    | Some({item: {d_arguments, _}, span}) =>
      switch (find_argument("name", d_arguments)) {
      | None =>
        raise_error(
          config.map_loc,
          span,
          "ppxField must be given 'name' argument",
        )
      | Some((_, {item: Iv_string(key), span})) =>
        Fr_fragment_spread({
          key,
          loc: config.map_loc(span),
          name: fs_name.item,
          type_name:
            switch (ty) {
            | Object({om_name, _}) => Some(om_name)
            | _ => None
            },
          arguments,
        })
      | Some(_) =>
        raise_error(
          config.map_loc,
          span,
          "The 'name' argument must be a string",
        )
      }
    };
  | InlineFragment({span, _}) =>
    raise_error(
      config.map_loc,
      span,
      "Inline fragments are not yet supported",
    )
  }
and unify_selection_set =
    (
      error_marker,
      as_record,
      existing_record,
      config,
      span,
      ty,
      selection_set,
    ) =>
  switch (selection_set) {
  | None =>
    make_error(
      error_marker,
      config.map_loc,
      span,
      "Must select subfields on objects",
    )
  | Some({item: [FragmentSpread({item: {fs_directives, fs_name}, _})], _}) =>
    let arguments = find_fragment_arguments(fs_directives);

    if (as_record) {
      make_error(
        error_marker,
        config.map_loc,
        span,
        "@ppxRecord can not be used with fragment spreads, place @ppxRecord on the fragment definition instead",
      );
    } else {
      Res_solo_fragment_spread({
        loc: config.map_loc(span),
        name: fs_name.item,
        arguments,
      });
    };
  | Some({item, _}) when as_record =>
    Res_record({
      loc: config.map_loc(span),
      name: type_name(ty),
      fields: List.map(unify_selection(error_marker, config, ty), item),
      type_name: existing_record,
      interface_fragments: None,
    })
  | Some({item, _}) =>
    Res_object({
      loc: config.map_loc(span),
      name: type_name(ty),
      fields: List.map(unify_selection(error_marker, config, ty), item),
      type_name: existing_record,
      interface_fragments: None,
    })
  };

let unify_operation = (error_marker, config) =>
  fun
  | {item: {o_type: Query, o_selection_set, _}, span} =>
    unify_selection_set(
      error_marker,
      false,
      None,
      config,
      span,
      query_type(config.schema),
      Some(o_selection_set),
    )
  | {item: {o_type: Mutation, o_selection_set, _}, span} =>
    switch (mutation_type(config.schema)) {
    | Some(mutation_type) =>
      unify_selection_set(
        error_marker,
        false,
        None,
        config,
        span,
        mutation_type,
        Some(o_selection_set),
      )
    | None =>
      make_error(
        error_marker,
        config.map_loc,
        span,
        "This schema does not contain any mutations",
      )
    }
  | {item: {o_type: Subscription, o_selection_set, _}, span} =>
    switch (subscription_type(config.schema)) {
    | Some(subscription_type) =>
      unify_selection_set(
        error_marker,
        false,
        None,
        config,
        span,
        subscription_type,
        Some(o_selection_set),
      )
    | None =>
      make_error(
        error_marker,
        config.map_loc,
        span,
        "This schema does not contain any subscriptions",
      )
    };

let getFragmentArgumentDefinitions =
    (directives: list(Source_pos.spanning(Graphql_ast.directive))) => {
  switch (
    directives |> List.find(d => {d.item.d_name.item == "argumentDefinitions"})
  ) {
  | {item: {d_arguments: Some(arguments), _}, _} =>
    arguments.item
    |> List.fold_left(
         acc =>
           fun
           | (
               {item: key, span},
               {item: Iv_object(values), span: type_span},
             ) => {
               let type_ =
                 values
                 |> List.fold_left(
                      acc =>
                        fun
                        | ({item: "type", _}, {item: Iv_string(type_), _}) =>
                          Some(type_)
                        | _ => acc,
                      None,
                    );
               switch (type_) {
               | Some(type_) => [(key, type_, span, type_span), ...acc]
               | _ => acc
               };
             }
           | _ => acc,
         [],
       )
  | _ => []
  | exception Not_found => []
  };
};

type query_config = {
  schema: option(string),
  records: option(bool),
  objects: option(bool),
  inline: option(bool),
  template_tag: option(string),
  template_tag_location: option(string),
  template_tag_import: option(string),
  template_tag_return_type: option(string),
  tagged_template: option(bool),
  future_added_value: option(bool),
  extend: option(string),
  fragment_in_query: option(Ppx_config.fragment_in_query),
  apollo_mode: option(bool),
};

let get_template_tag = query_config => {
  switch (query_config.tagged_template) {
  | Some(false) => (None, None, None)
  | _ =>
    switch (
      get_with_default(query_config.template_tag, Ppx_config.template_tag()),
      get_with_default(
        query_config.template_tag_location,
        Ppx_config.template_tag_location(),
      ),
      get_with_default(
        query_config.template_tag_import,
        Ppx_config.template_tag_import(),
      ),
    ) {
    | (Some(tag), Some(location), Some(import)) => (
        Some(tag),
        Some(location),
        Some(import),
      )
    | (None, Some(location), Some(import)) => (
        Some(import),
        Some(location),
        Some(import),
      )
    | (Some(tag), Some(location), None) => (
        Some(tag),
        Some(location),
        Some("default"),
      )
    | (None, Some(location), None) => (
        None,
        Some(location),
        Some("default"),
      )
    | (Some(tag), None, Some(_))
    | (Some(tag), None, None) => (Some(tag), None, None)
    | (None, _, _) => (None, None, None)
    }
  };
};

let get_config_arguments = directives => {
  Graphql_ast.(
    directives
    |> filter_map(
         fun
         | {
             item: {
               d_name: {item: "ppxConfig"},
               d_arguments: Some({item: arguments}),
             },
           } =>
           Some(arguments)
         | _ => None,
       )
    |> List.concat
  );
};

/**
  Allow configuration per definition using directives
*/

let config_arguments_to_config =
    (existing_query_config, directive_arguments): query_config => {
  Graphql_ast.(
    directive_arguments
    |> List.fold_left(
         config =>
           fun
           | ({item: "schema"}, {item: Iv_string(location)}) => {
               ...config,
               schema: Some(location),
             }
           | _ => config,
         existing_query_config,
       )
  );
};

let to_output_config =
    (~map_loc, ~delimiter, ~document, (definition, query_config)) => {
  let schema = Lazy.force(Read_schema.get_schema(query_config.schema));
  let definition =
    switch (
      {
        (
          if (switch (query_config.apollo_mode) {
              | None => Ppx_config.apollo_mode()
              | Some(apollo_mode) => apollo_mode
              }) {
            [definition]
            |> Ast_transforms.add_typename_to_selection_set(schema);
          } else {
            [definition];
          }
        )
        |> Ast_transforms.remove_typename_from_union(schema);
      }
    ) {
    | [definition] => definition
    | _ => definition
    };

  let template_tag = get_template_tag(query_config);

  (
    definition,
    {
      Generator_utils.map_loc,
      delimiter,
      full_document: document,
      records:
        switch (query_config.records, query_config.objects) {
        | (Some(value), _) => value
        | (_, Some(true)) => false
        | (_, Some(false)) => true
        | (None, None) => Ppx_config.records()
        },
      inline:
        switch (query_config.inline) {
        | Some(value) => value
        | None => false
        },
      future_added_value:
        switch (query_config.future_added_value) {
        | Some(value) => value
        | None => Ppx_config.future_added_value()
        },
      /*  the only call site of schema, make it lazy! */
      schema,
      template_tag,
      template_tag_return_type:
        get_with_default(
          query_config.template_tag_return_type,
          Ppx_config.template_tag_return_type(),
        ),
      extend: query_config.extend,
      fragment_in_query:
        switch (query_config.fragment_in_query) {
        | Some(value) => value
        | None => Ppx_config.fragment_in_query()
        },
    },
  );
};

let rec generate_config =
        (~map_loc, ~delimiter, ~initial_query_config, document) => {
  switch (document) {
  | [Operation({item: {o_directives: directives}}) as definition, ...rest]
  | [Fragment({item: {fg_directives: directives}}) as definition, ...rest] =>
    let query_config =
      directives
      |> get_config_arguments
      |> config_arguments_to_config(initial_query_config);
    [
      to_output_config(
        ~document,
        ~map_loc,
        ~delimiter,
        (definition, query_config),
      ),
      ...generate_config(~map_loc, ~delimiter, ~initial_query_config, rest),
    ];
  | [] => []
  };
};

let rec unify_document_schema = document => {
  let error_marker = {Generator_utils.has_error: false};
  switch (document) {
  | [
      (Operation({item: {o_variable_definitions, _}, _} as op), config),
      ...rest,
    ] =>
    let structure = unify_operation(error_marker, config, op);
    [
      (
        Def_operation({
          variable_definitions: o_variable_definitions,
          has_error: error_marker.has_error,
          operation: op,
          inner: structure,
        }),
        config,
      ),
      ...unify_document_schema(rest),
    ];
  | [
      (
        Fragment(
          {
            item: {
              fg_name,
              fg_selection_set,
              fg_type_condition,
              fg_directives,
            },
            span,
          } as fg,
        ),
        config,
      ),
      ...rest,
    ] => [
      {
        open Result;
        let with_decoder =
          switch (
            switch (
              fg_directives |> find_directive(~prepend=true, "Decoder"),
              fg_directives |> find_directive(~prepend=true, "Custom"),
            ) {
            | (_, Some(decoder)) => Some(decoder)
            | (Some(decoder), _) => Some(decoder)
            | (None, None) => None
            }
          ) {
          | None => Ok(None)
          | Some({item: {d_arguments, _}, span}) =>
            switch (find_argument("fn", d_arguments)) {
            | None =>
              Error(
                make_error(
                  error_marker,
                  config.map_loc,
                  span,
                  "ppxDecoder must be given 'fn' argument",
                ),
              )
            | Some((_, {item: Iv_string(ident), span})) =>
              Ok(
                Some(
                  structure =>
                    Res_custom_decoder({
                      loc: config.map_loc(span),
                      ident,
                      inner: structure,
                    }),
                ),
              )
            | Some((_, {span, _})) =>
              Error(
                make_error(
                  error_marker,
                  config.map_loc,
                  span,
                  "The 'fn' argument must be a string",
                ),
              )
            }
          };

        let is_record = has_directive(~prepend=true, "Record", fg_directives);

        let argumentDefinitions =
          getFragmentArgumentDefinitions(fg_directives);

        (
          switch (Schema.lookup_type(config.schema, fg_type_condition.item)) {
          | None =>
            Def_fragment({
              name: fg_name.item,
              req_vars: argumentDefinitions,
              has_error: true,
              fragment: fg,
              type_name: None,
              inner:
                make_error(
                  error_marker,
                  config.map_loc,
                  fg_type_condition.span,
                  Printf.sprintf(
                    "Unknown type \"%s\"",
                    fg_type_condition.item,
                  ),
                ),
            })
          | Some(ty) =>
            let existing_record = get_ppx_as(fg_directives);
            let structure =
              unify_selection_set(
                error_marker,
                is_record,
                existing_record,
                config,
                span,
                ty,
                Some(fg_selection_set),
              );

            let argumentDefinitions =
              getFragmentArgumentDefinitions(fg_directives);

            switch (with_decoder) {
            | Error(err) =>
              Def_fragment({
                name: fg_name.item,
                req_vars: argumentDefinitions,
                has_error: true,
                fragment: fg,
                type_name: existing_record,
                inner: err,
              })
            | Ok(decoder) =>
              Def_fragment({
                name: fg_name.item,
                req_vars: argumentDefinitions,
                has_error: error_marker.has_error,
                fragment: fg,
                type_name: existing_record,
                inner:
                  switch (decoder) {
                  | Some(decoder) => decoder(structure)
                  | None => structure
                  },
              })
            };
          },
          config,
        );
      },
      ...unify_document_schema(rest),
    ]
  | [] => []
  };
};
