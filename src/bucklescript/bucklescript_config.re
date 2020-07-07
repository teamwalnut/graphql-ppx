open Graphql_ppx_base;

module Paths = {
  let bsconfig = "bsconfig.json";
  let bsbProjectRoot = ref("");
  let projectRoot = ref("");

  let rec findProjectRoot = (~dir) =>
    if (Sys.file_exists(Filename.concat(dir, bsconfig))) {
      dir;
    } else {
      let parent = dir |> Filename.dirname;
      if (parent == dir) {
        prerr_endline(
          "Error: cannot find project root containing " ++ bsconfig ++ ".",
        );
        assert(false);
      } else {
        findProjectRoot(~dir=parent);
      };
    };

  let setProjectRoot = () => {
    projectRoot := findProjectRoot(~dir=Sys.getcwd());
    bsbProjectRoot :=
      (
        switch (Sys.getenv_opt("BSB_PROJECT_ROOT")) {
        | None => projectRoot^
        | Some(s) => s
        }
      );
  };

  let concat = Filename.concat;

  let getBsConfigFile = () => {
    let bsconfig = concat(projectRoot^, bsconfig);
    bsconfig |> Sys.file_exists ? Some(bsconfig) : None;
  };
};

exception Config_error(string);

let defaultConfig =
  Ppx_config.{
    verbose_logging: false,
    output_mode: Ppx_config.String,
    verbose_error_handling:
      switch (Sys.getenv("NODE_ENV")) {
      | "production" => false
      | _ => true
      | exception Not_found => true
      },
    apollo_mode: false,
    schema_file: "graphql_schema.json",
    root_directory: Sys.getcwd(),
    raise_error_with_loc: (loc, message) => {
      let loc = Output_bucklescript_utils.conv_loc(loc);
      raise(Location.Error(Location.error(~loc, message)));
    },
    records: true,
    legacy: false,
    template_tag: None,
    template_tag_location: None,
    template_tag_import: None,
    custom_fields: Hashtbl.create(0),
    future_added_value: true,
    extend_query: None,
    extend_query_no_required_variables: None,
    extend_mutation: None,
    extend_mutation_no_required_variables: None,
    extend_subscription: None,
    extend_subscription_no_required_variables: None,
    extend_fragment: None,
    fragment_in_query: Include
  };

module JsonHelper = {
  open Yojson.Basic.Util;
  let mapBool = (option, update, json) =>
    try(update(json |> (json => json |> member(option) |> to_bool))) {
    | Yojson.Basic.Util.Type_error(_) => ()
    | other => raise(other)
    };
  let mapString = (option, update, json) =>
    try(update(json |> (json => json |> member(option) |> to_string))) {
    | Yojson.Basic.Util.Type_error(_) => ()
    | other => raise(other)
    };
};

let read_custom_fields = json => {
  Yojson.Basic.Util.(
    try({
      let custom_fields = Ppx_config.custom_fields() |> Hashtbl.copy;
      json
      |> (json => json |> member("custom-fields") |> to_assoc)
      |> List.map(((key, value)) => (key, value |> to_string))
      |> List.iter(((key, value)) => {
           Hashtbl.add(custom_fields, key, value)
         });
      Ppx_config.update_config(current => {...current, custom_fields});

      ();
    }) {
    | Yojson.Basic.Util.Type_error(_) => ()
    | other => raise(other)
    }
  );
};

let read_config = () => {
  Paths.setProjectRoot();

  Ppx_config.set_config(defaultConfig);
  open Yojson.Basic.Util;

  let parseConfig = (json: Yojson.Basic.t) => {
    let ppxConfig = json |> member("graphql");

    ppxConfig
    |> JsonHelper.mapBool("verbose", verbose_logging => {
         Ppx_config.update_config(current => {...current, verbose_logging})
       });
    ppxConfig
    |> JsonHelper.mapBool("future-added-value", future_added_value => {
         Ppx_config.update_config(current => {...current, future_added_value})
       });
    ppxConfig
    |> JsonHelper.mapBool("apollo-mode", apollo_mode => {
         Ppx_config.update_config(current => {...current, apollo_mode})
       });
    ppxConfig
    |> JsonHelper.mapString("schema", schema_file => {
         Ppx_config.update_config(current => {...current, schema_file})
       });
    ppxConfig
    |> JsonHelper.mapString("ast-out", ast_out => {
         Ppx_config.update_config(current =>
           {
             ...current,
             output_mode:
               switch (ast_out) {
               | "apollo" => Ppx_config.Apollo_AST
               | "string" => Ppx_config.String
               | other =>
                 raise(
                   Config_error(
                     "Error in graphql configuration: ast-out \""
                     ++ other
                     ++ "\" is not supported. Choose either apollo or string.",
                   ),
                 )
               },
           }
         )
       });
    ppxConfig
    |> JsonHelper.mapString("mode", mode => {
         switch (mode) {
         | "legacy" =>
           Ppx_config.update_config(current =>
             {...current, legacy: true, records: false}
           )
         | "objects" =>
           Ppx_config.update_config(current => {...current, records: false})
         | "records" =>
           Ppx_config.update_config(current => {...current, records: true})
         | other =>
           raise(
             Config_error(
               "Error in graphql-ppx configuration: mode \""
               ++ other
               ++ "\" is not supported. Choose either records, objects or legacy.",
             ),
           )
         }
       });

    ppxConfig
    |> JsonHelper.mapString("fragment-in-query", mode => {
         switch (mode) {
         | "include" =>
           Ppx_config.update_config(current =>
             {...current, fragment_in_query: Include}
           )
         | "exclude" =>
           Ppx_config.update_config(current => {...current, fragment_in_query: Exclude})
         | other =>
           raise(
             Config_error(
               "Error in graphql-ppx configuration: fragment-in-query\""
               ++ other
               ++ "\" is not supported. Choose either 'include' or 'exclude'.",
             ),
           )
         }
       });

    ppxConfig
    |> JsonHelper.mapString("extend-query", extend_query => {
         Ppx_config.update_config(current =>
           {...current, extend_query: Some(extend_query)}
         )
       });

    ppxConfig
    |> JsonHelper.mapString("extend-query-no-required-variables", extend_query => {
         Ppx_config.update_config(current =>
           {
             ...current,
             extend_query_no_required_variables: Some(extend_query),
           }
         )
       });

    ppxConfig
    |> JsonHelper.mapString("extend-mutation", extend_mutation => {
         Ppx_config.update_config(current =>
           {...current, extend_mutation: Some(extend_mutation)}
         )
       });

    ppxConfig
    |> JsonHelper.mapString(
         "extend-mutation-no-required-variables", extend_mutation => {
         Ppx_config.update_config(current =>
           {
             ...current,
             extend_mutation_no_required_variables: Some(extend_mutation),
           }
         )
       });

    ppxConfig
    |> JsonHelper.mapString("extend-subscription", extend_subscription => {
         Ppx_config.update_config(current =>
           {...current, extend_subscription: Some(extend_subscription)}
         )
       });

    ppxConfig
    |> JsonHelper.mapString(
         "extend-subscription-no-required-variables", extend_subscription => {
         Ppx_config.update_config(current =>
           {
             ...current,
             extend_subscription_no_required_variables:
               Some(extend_subscription),
           }
         )
       });

    ppxConfig
    |> JsonHelper.mapString("extend-fragment", extend_fragment => {
         Ppx_config.update_config(current =>
           {...current, extend_fragment: Some(extend_fragment)}
         )
       });

    ppxConfig
    |> JsonHelper.mapString("template-tag", template_tag => {
         Ppx_config.update_config(current =>
           {...current, template_tag: Some(template_tag)}
         )
       });
    ppxConfig
    |> JsonHelper.mapString("template-tag-import", template_tag_import => {
         Ppx_config.update_config(current =>
           {...current, template_tag_import: Some(template_tag_import)}
         )
       });
    ppxConfig
    |> JsonHelper.mapString("template-tag-location", template_tag_location => {
         Ppx_config.update_config(current =>
           {...current, template_tag_location: Some(template_tag_location)}
         )
       });
    ppxConfig |> read_custom_fields;
  };

  switch (Paths.getBsConfigFile()) {
  | Some(bsConfigFile) =>
    try(bsConfigFile |> Yojson.Basic.from_file |> parseConfig) {
    | Config_error(_) as e => raise(e)
    | _ => ()
    }
  | None => ()
  };
};
