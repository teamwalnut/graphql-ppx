open Graphql_ast;
open Source_pos;
open Schema;

open Type_utils;
open Generator_utils;
open Result_structure;

exception Unimplemented(string);

let make_error = (error_marker, map_loc, span, message) => {
  let () = error_marker.has_error = true;
  Res_error(map_loc(span), message);
};

let has_directive = (name, directives) =>
  List.exists(
    ({item: {d_name: {item, _}, _}, _}) => item == name,
    directives,
  );

let find_directive = (name, directives) =>
  switch (
    List.find(
      ({item: {d_name: {item, _}, _}, _}) => item == name,
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
  switch (directives |> find_directive("ppxAs")) {
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
          error_marker,
          as_record,
          existing_record,
          config,
          span,
          ty,
          selection_set: option(spanning(list(selection))),
        ) =>
  switch (ty) {
  | Ntr_nullable(t) =>
    Res_nullable(
      config.map_loc(span),
      unify_type(
        error_marker,
        as_record,
        existing_record,
        config,
        span,
        t,
        selection_set,
      ),
    )
  | Ntr_list(t) =>
    Res_array(
      config.map_loc(span),
      unify_type(
        error_marker,
        as_record,
        existing_record,
        config,
        span,
        t,
        selection_set,
      ),
    )
  | Ntr_named(n) =>
    switch (lookup_type(config.schema, n)) {
    | None => raise_error(config.map_loc, span, "Could not find type " ++ n)
    | Some(Scalar({sm_name: "ID", _}))
    | Some(Scalar({sm_name: "String", _})) =>
      Res_string(config.map_loc(span))
    | Some(Scalar({sm_name: "Int", _})) => Res_int(config.map_loc(span))
    | Some(Scalar({sm_name: "Float", _})) =>
      Res_float(config.map_loc(span))
    | Some(Scalar({sm_name: "Boolean", _})) =>
      Res_boolean(config.map_loc(span))
    | Some(Scalar(_)) => Res_raw_scalar(config.map_loc(span))
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
      Res_poly_enum(config.map_loc(span), enum_meta)
    | Some(Interface(im) as ty) =>
      unify_interface(
        error_marker,
        as_record,
        config,
        span,
        im,
        ty,
        selection_set,
      )
    | Some(InputObject(_)) =>
      make_error(
        error_marker,
        config.map_loc,
        span,
        "Can't have fields on input objects",
      )
    | Some(Union(um)) =>
      unify_union(error_marker, config, span, um, selection_set)
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
      Res_object(
        config.map_loc(span),
        name,
        List.map(unify_selection(error_marker, config, ty), selection),
        None,
      ),
    );
    let generate_fragment_case =
        ({item: {if_type_condition, if_selection_set, _}, _}) =>
      switch (if_type_condition) {
      | Some(if_type_condition) =>
        let {item, _} = if_selection_set;
        let selection = List.append(base_selection_set, item);
        let ty =
          switch (lookup_type(config.schema, if_type_condition.item)) {
          | Some(ty) => ty
          | None => ty
          };

        generate_case(selection, ty, if_type_condition.item);
      | None => assert(false)
      };

    let fragment_cases = List.map(generate_fragment_case, fragments);
    let base_case =
      generate_case(base_selection_set, ty, interface_meta.im_name);

    Res_poly_variant_interface(
      config.map_loc(span),
      interface_meta.im_name,
      base_case,
      fragment_cases,
    );
  }
and unify_union = (error_marker, config, span, union_meta, selection_set) =>
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

        let is_record = has_directive("bsRecord", if_directives);

        let result_decoder =
          unify_selection_set(
            error_marker,
            is_record,
            None,
            config,
            if_selection_set.span,
            type_cond_ty,
            Some(if_selection_set),
          );
        (if_type_condition.item, result_decoder);
      | None => assert(false)
      };

    let fragments = List.map(unwrap_type_conds, selection_set.item);
    let covered_cases =
      List.map(type_cond_name, fragments) |> List.sort(compare);
    let possible_cases = List.sort(compare, union_meta.um_of_types);

    Res_poly_variant_union(
      config.map_loc(span),
      union_meta.um_name,
      List.map(generate_case, fragments),
      if (covered_cases == possible_cases) {
        Exhaustive;
      } else {
        Nonexhaustive;
      },
    );
  }
and unify_variant = (error_marker, config, span, ty, selection_set) =>
  switch (ty) {
  | Ntr_nullable(t) =>
    Res_nullable(
      config.map_loc(span),
      unify_variant(error_marker, config, span, t, selection_set),
    )
  | Ntr_list(t) =>
    Res_array(
      config.map_loc(span),
      unify_variant(error_marker, config, span, t, selection_set),
    )
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
                   let key = some_or(item.fd_alias, item.fd_name).item;
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
                       error_marker,
                       false,
                       None,
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

        Res_poly_variant_selection_set(config.map_loc(span), n, fields);
      }
    }
  }
and unify_field = (error_marker, config, field_span, ty) => {
  let ast_field = field_span.item;
  let field_name = ast_field.fd_name.item;
  let field_meta = lookup_field(ty, field_name);
  let key = some_or(ast_field.fd_alias, ast_field.fd_name).item;
  let is_variant = has_directive("bsVariant", ast_field.fd_directives);
  let is_record = has_directive("bsRecord", ast_field.fd_directives);
  let existing_record = get_ppx_as(ast_field.fd_directives);

  let has_skip =
    has_directive("skip", ast_field.fd_directives)
    || has_directive("include", ast_field.fd_directives);
  let sub_unifier =
    if (is_variant) {
      unify_variant(error_marker);
    } else {
      unify_type(error_marker, is_record, existing_record);
    };

  let parser_expr =
    switch (field_meta) {
    | None =>
      make_error(
        error_marker,
        config.map_loc,
        field_span.span,
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
        Res_nullable(config.map_loc(field_span.span), sub_unifier);
      } else {
        sub_unifier;
      };
    };

  let loc = config.map_loc(field_span.span);
  switch (ast_field.fd_directives |> find_directive("ppxDecoder")) {
  | None => Fr_named_field(key, loc, parser_expr)
  | Some({item: {d_arguments, _}, span}) =>
    switch (find_argument("module", d_arguments)) {
    | None =>
      Fr_named_field(
        key,
        loc,
        make_error(
          error_marker,
          config.map_loc,
          span,
          "pxxDecoder must be given 'module' argument",
        ),
      )
    | Some((_, {item: Iv_string(module_name), span})) =>
      switch (parser_expr) {
      | Res_nullable(loc, t) =>
        Fr_named_field(
          key,
          loc,
          Res_nullable(
            loc,
            Res_custom_decoder(config.map_loc(span), module_name, t),
          ),
        )
      | Res_array(loc, t) =>
        Fr_named_field(
          key,
          loc,
          Res_array(
            loc,
            Res_custom_decoder(config.map_loc(span), module_name, t),
          ),
        )
      | _ =>
        Fr_named_field(
          key,
          loc,
          Res_custom_decoder(config.map_loc(span), module_name, parser_expr),
        )
      }
    | Some((_, {span, _})) =>
      Fr_named_field(
        key,
        loc,
        make_error(
          error_marker,
          config.map_loc,
          span,
          "The 'module' argument must be a string",
        ),
      )
    }
  };
}
and unify_selection = (error_marker, config, ty, selection) =>
  switch (selection) {
  | Field(field_span) => unify_field(error_marker, config, field_span, ty)
  | FragmentSpread({item: {fs_directives, fs_name}, span}) =>
    let arguments = find_fragment_arguments(fs_directives);
    switch (find_directive("bsField", fs_directives)) {
    | None =>
      let key =
        fs_name.item
        |> String.split_on_char('.')
        |> List.rev
        |> List.hd
        |> String.uncapitalize_ascii;
      Fr_fragment_spread(
        key,
        config.map_loc(span),
        fs_name.item,
        switch (ty) {
        | Object({om_name, _}) => Some(om_name)
        | _ => None
        },
        arguments,
      );
    | Some({item: {d_arguments, _}, span}) =>
      switch (find_argument("name", d_arguments)) {
      | None =>
        raise_error(
          config.map_loc,
          span,
          "bsField must be given 'name' argument",
        )
      | Some((_, {item: Iv_string(key), span})) =>
        Fr_fragment_spread(
          key,
          config.map_loc(span),
          fs_name.item,
          switch (ty) {
          | Object({om_name, _}) => Some(om_name)
          | _ => None
          },
          arguments,
        )
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
        "@bsRecord can not be used with fragment spreads, place @bsRecord on the fragment definition instead",
      );
    } else {
      Res_solo_fragment_spread(
        config.map_loc(span),
        fs_name.item,
        arguments,
      );
    };
  | Some({item, _}) when as_record =>
    Res_record(
      config.map_loc(span),
      type_name(ty),
      List.map(unify_selection(error_marker, config, ty), item),
      existing_record,
    )
  | Some({item, _}) =>
    Res_object(
      config.map_loc(span),
      type_name(ty),
      List.map(unify_selection(error_marker, config, ty), item),
      existing_record,
    )
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

let rec unify_document_schema = (config, document) => {
  let error_marker = {Generator_utils.has_error: false};
  switch (document) {
  | [Operation({item: {o_variable_definitions, _}, _} as op), ...rest] =>
    let structure = unify_operation(error_marker, config, op);
    [
      Mod_default_operation(
        o_variable_definitions,
        error_marker.has_error,
        op,
        structure,
      ),
      ...unify_document_schema(config, rest),
    ];
  | [
      Fragment(
        {
          item: {fg_name, fg_selection_set, fg_type_condition, fg_directives},
          span,
        } as fg,
      ),
      ...rest,
    ] => [
      {
        open Result;

        let with_decoder =
          switch (fg_directives |> find_directive("bsDecoder")) {
          | None => Ok(None)
          | Some({item: {d_arguments, _}, span}) =>
            switch (find_argument("fn", d_arguments)) {
            | None =>
              Error(
                make_error(
                  error_marker,
                  config.map_loc,
                  span,
                  "bsDecoder must be given 'fn' argument",
                ),
              )
            | Some((_, {item: Iv_string(fn_name), span})) =>
              Ok(
                Some(
                  structure =>
                    Res_custom_decoder(
                      config.map_loc(span),
                      fn_name,
                      structure,
                    ),
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

        let is_record = has_directive("bsRecord", fg_directives);

        let argumentDefinitions =
          getFragmentArgumentDefinitions(fg_directives);
        switch (Schema.lookup_type(config.schema, fg_type_condition.item)) {
        | None =>
          Mod_fragment(
            fg_name.item,
            argumentDefinitions,
            true,
            fg,
            make_error(
              error_marker,
              config.map_loc,
              span,
              Printf.sprintf("Unknown type \"%s\"", fg_type_condition.item),
            ),
          )
        | Some(ty) =>
          let structure =
            unify_selection_set(
              error_marker,
              is_record,
              None,
              config,
              span,
              ty,
              Some(fg_selection_set),
            );

          let argumentDefinitions =
            getFragmentArgumentDefinitions(fg_directives);

          switch (with_decoder) {
          | Error(err) => Mod_fragment(fg_name.item, argumentDefinitions, true, fg, err)
          | Ok(decoder) =>
            Mod_fragment(
              fg_name.item,
              argumentDefinitions,
              error_marker.has_error,
              fg,
              switch (decoder) {
              | Some(decoder) => decoder(structure)
              | None => structure
              },
            )
          };
        };
      },
      ...unify_document_schema(config, rest),
    ]
  | [] => []
  };
};
