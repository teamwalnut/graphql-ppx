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
    template_tag: None,
    template_tag_location: None,
    template_tag_import: None,
    template_tag_return_type: None,
    template_tag_is_function: None,
    custom_fields: Hashtbl.create(0),
    future_added_value: true,
    extend_query: None,
    extend_query_no_required_variables: None,
    extend_mutation: None,
    extend_mutation_no_required_variables: None,
    extend_subscription: None,
    extend_subscription_no_required_variables: None,
    extend_fragment: None,
    fragment_in_query: Include,
    native: true,
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
  open Yojson.Basic.Util;
  let custom_fields = Ppx_config.custom_fields() |> Hashtbl.copy;

  let custom_fields_result_depr =
    try(json |> member("custom-fields") |> to_assoc) {
    | Yojson.Basic.Util.Type_error(_) => []
    | other => raise(other)
    };
  let custom_fields_result =
    try(json |> member("customFields") |> to_assoc) {
    | Yojson.Basic.Util.Type_error(_) => []
    | other => raise(other)
    };

  List.append(custom_fields_result_depr, custom_fields_result)
  |> List.map(((key, value)) => {
       let value =
         try(Some(value |> to_string)) {
         | Yojson.Basic.Util.Type_error(_) => None
         | other => raise(other)
         };

       (key, value);
     })
  |> List.iter(((key, value)) => {
       switch (value) {
       | Some(value) => Hashtbl.add(custom_fields, key, value)
       | None => ()
       }
     });

  Ppx_config.update_config(current => {...current, custom_fields});

  ();
};

let read_config = () => {
  Paths.setProjectRoot();

  Ppx_config.set_config(defaultConfig);
  open Yojson.Basic.Util;

  let parseConfig = (json: Yojson.Basic.t) => {
    let ppxConfig = json |> member("graphql");

    let handleVerboseLogging = verbose_logging => {
      Ppx_config.update_config(current => {...current, verbose_logging});
    };
    let handleFutureAddedValue = future_added_value => {
      Ppx_config.update_config(current => {...current, future_added_value});
    };
    let handleOmitFutureAddedValue = omit_future_added_value => {
      Ppx_config.update_config(current =>
        {...current, future_added_value: !omit_future_added_value}
      );
    };
    let handleApolloMode = apollo_mode => {
      Ppx_config.update_config(current => {...current, apollo_mode});
    };

    let handleSchema = schema_file => {
      Ppx_config.update_config(current => {...current, schema_file});
    };
    let handleAstOut = ast_out => {
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
      );
    };
    let handleFragmentInQuery = mode => {
      switch (mode) {
      | "include" =>
        Ppx_config.update_config(current =>
          {...current, fragment_in_query: Include}
        )
      | "exclude" =>
        Ppx_config.update_config(current =>
          {...current, fragment_in_query: Exclude}
        )
      | other =>
        raise(
          Config_error(
            "Error in graphql-ppx configuration: fragment-in-query\""
            ++ other
            ++ "\" is not supported. Choose either 'include' or 'exclude'.",
          ),
        )
      };
    };

    let handleExtendQuery = extend_query => {
      Ppx_config.update_config(current =>
        {...current, extend_query: Some(extend_query)}
      );
    };

    let handleExtendQueryNoRequiredVariables = extend_query => {
      Ppx_config.update_config(current =>
        {...current, extend_query_no_required_variables: Some(extend_query)}
      );
    };
    let handleExtendMutation = extend_mutation => {
      Ppx_config.update_config(current =>
        {...current, extend_mutation: Some(extend_mutation)}
      );
    };
    let handleExtendMutationNoRequiredVariables = extend_mutation => {
      Ppx_config.update_config(current =>
        {
          ...current,
          extend_mutation_no_required_variables: Some(extend_mutation),
        }
      );
    };
    let handleExtendSubscription = extend_subscription => {
      Ppx_config.update_config(current =>
        {...current, extend_subscription: Some(extend_subscription)}
      );
    };
    let handleExtendSubscriptionNoRequiredVariables = extend_subscription => {
      Ppx_config.update_config(current =>
        {
          ...current,
          extend_subscription_no_required_variables:
            Some(extend_subscription),
        }
      );
    };
    let handleExtendFragment = extend_fragment => {
      Ppx_config.update_config(current =>
        {...current, extend_fragment: Some(extend_fragment)}
      );
    };
    let handleTemplateTag = template_tag => {
      Ppx_config.update_config(current =>
        {...current, template_tag: Some(template_tag)}
      );
    };

    let handleTemplateTagImport = template_tag_import => {
      Ppx_config.update_config(current =>
        {...current, template_tag_import: Some(template_tag_import)}
      );
    };
    let handleTemplateTagLocation = template_tag_location => {
      Ppx_config.update_config(current =>
        {...current, template_tag_location: Some(template_tag_location)}
      );
    };
    let handleTemplateTagReturnType = template_tag_return_type => {
      Ppx_config.update_config(current =>
        {
          ...current,
          template_tag_return_type: Some(template_tag_return_type),
        }
      );
    };
    let handleTemplateTagIsFunction = template_tag_is_function => {
      Ppx_config.update_config(current =>
        {
          ...current,
          template_tag_is_function: Some(template_tag_is_function),
        }
      );
    };

    let configBool = (key, value) =>
      ppxConfig |> JsonHelper.mapBool(key, value);
    let configString = (key, value) =>
      ppxConfig |> JsonHelper.mapString(key, value);

    configBool("verbose", handleVerboseLogging);
    configBool("future-added-value", handleFutureAddedValue);
    configBool("futureAddedValue", handleFutureAddedValue);
    configBool("omitFutureAddedValue", handleOmitFutureAddedValue);
    configBool("apollo-mode", handleApolloMode);
    configBool("apolloMode", handleApolloMode);
    configString("schema", handleSchema);
    configString("ast-out", handleAstOut);
    configString("astOut", handleAstOut);
    configString("fragment-in-query", handleFragmentInQuery);
    configString("fragmentInQuery", handleFragmentInQuery);
    configString("extend-query", handleExtendQuery);
    configString("extendQuery", handleExtendQuery);
    configString(
      "extend-query-no-required-variables",
      handleExtendQueryNoRequiredVariables,
    );
    configString(
      "extendQueryNoRequiredVariables",
      handleExtendQueryNoRequiredVariables,
    );
    configString("extend-mutation", handleExtendMutation);
    configString("extendMutation", handleExtendMutation);
    configString(
      "extend-mutation-no-required-variables",
      handleExtendMutationNoRequiredVariables,
    );
    configString(
      "extendMutationNoRequiredVariables",
      handleExtendMutationNoRequiredVariables,
    );

    configString("extend-subscription", handleExtendSubscription);
    configString("extendSubscription", handleExtendSubscription);
    configString(
      "extend-subscription-no-required-variables",
      handleExtendSubscriptionNoRequiredVariables,
    );
    configString(
      "extendSubscriptionNoRequiredVariables",
      handleExtendSubscriptionNoRequiredVariables,
    );
    configString("extend-fragment", handleExtendFragment);
    configString("extendFragment", handleExtendFragment);
    configString("template-tag", handleTemplateTag);
    configString("templateTag", handleTemplateTag);
    configString("template-tag-import", handleTemplateTagImport);
    configString("templateTagImport", handleTemplateTagImport);
    configString("template-tag-location", handleTemplateTagLocation);
    configString("templateTagLocation", handleTemplateTagLocation);
    configString("template-tag-return-type", handleTemplateTagReturnType);
    configString("templateTagReturnType", handleTemplateTagReturnType);
    configBool("template-tag-is-function", handleTemplateTagIsFunction);
    configBool("templateTagIsFunction", handleTemplateTagIsFunction);

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
