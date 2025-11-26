open Graphql_compiler

module Paths = struct
  let bsconfig = "bsconfig.json"
  let rescriptconfig = "rescript.json"
  let bsbProjectRoot = ref ""
  let projectRoot = ref ""

  let rec findProjectRoot ~dir =
    if
      Sys.file_exists (Filename.concat dir bsconfig)
      || Sys.file_exists (Filename.concat dir rescriptconfig)
    then dir
    else
      let parent = dir |> Filename.dirname in
      if parent = dir then
        if not (Ppx_config.native ()) then (
          prerr_endline
            ("Error: cannot find project root containing " ^ bsconfig ^ ".");
          assert false)
        else dir
      else findProjectRoot ~dir:parent

  let setProjectRoot () =
    projectRoot := findProjectRoot ~dir:(Sys.getcwd ());
    bsbProjectRoot :=
      match Sys.getenv_opt "BSB_PROJECT_ROOT" with
      | None -> !projectRoot
      | Some s -> s

  let concat = Filename.concat

  let getBsConfigFile () =
    let bsconfig = concat !projectRoot bsconfig in
    let rescriptconfig = concat !projectRoot rescriptconfig in
    match (bsconfig |> Sys.file_exists, rescriptconfig |> Sys.file_exists) with
    | _, true -> Some rescriptconfig
    | true, _ -> Some bsconfig
    | _ -> None
end

exception Config_error of string

let defaultConfig =
  let open Ppx_config in
  {
    verbose_logging = false;
    output_mode = Ppx_config.String;
    verbose_error_handling =
      (match Sys.getenv "NODE_ENV" with
      | "production" -> false
      | _ -> true
      | exception Not_found -> true);
    apollo_mode = false;
    schema_file = "graphql_schema.json";
    root_directory = Sys.getcwd ();
    raise_error_with_loc =
      (fun loc message ->
        let loc = Output_utils.conv_loc loc in
        raise (Location.Error (Location.error ~loc message)));
    template_tag = None;
    template_tag_location = None;
    template_tag_import = None;
    template_tag_return_type = None;
    template_tag_is_function = None;
    custom_fields = Hashtbl.create 0;
    future_added_value = true;
    extend_query = None;
    extend_query_no_required_variables = None;
    extend_mutation = None;
    extend_mutation_no_required_variables = None;
    extend_subscription = None;
    extend_subscription_no_required_variables = None;
    extend_fragment = None;
    fragment_in_query = Include;
    native = true;
    uncurried = true;
  }

module JsonHelper = struct
  open Json.Util

  let mapBool option update json =
    try update (json |> fun json -> json |> member option |> to_bool) with
    | Json.Util.Type_error _ -> ()
    | other -> raise other

  let mapString option update json =
    try update (json |> fun json -> json |> member option |> to_string) with
    | Json.Util.Type_error _ -> ()
    | other -> raise other
end

let read_custom_fields json =
  let open Json.Util in
  let custom_fields = Ppx_config.custom_fields () |> Hashtbl.copy in
  let custom_fields_result_depr =
    try json |> member "custom-fields" |> to_assoc with
    | Json.Util.Type_error _ -> []
    | other -> raise other
  in
  let custom_fields_result =
    try json |> member "customFields" |> to_assoc with
    | Json.Util.Type_error _ -> []
    | other -> raise other
  in
  List.append custom_fields_result_depr custom_fields_result
  |> List.map (fun (key, value) ->
         let value =
           try Some (value |> to_string) with
           | Json.Util.Type_error _ -> None
           | other -> raise other
         in
         (key, value))
  |> List.iter (fun (key, value) ->
         match value with
         | Some value -> Hashtbl.add custom_fields key value
         | None -> ());
  Ppx_config.update_config (fun current -> { current with custom_fields });
  ()

let read_config () =
  Ppx_config.set_config defaultConfig;
  Paths.setProjectRoot ();
  let open Json.Util in
  let parseConfig (json : Json.t) =
    let ppxConfig = json |> member "graphql" in
    let handleVerboseLogging verbose_logging =
      Ppx_config.update_config (fun current -> { current with verbose_logging })
    in
    let handleFutureAddedValue future_added_value =
      Ppx_config.update_config (fun current ->
          { current with future_added_value })
    in
    let handleOmitFutureAddedValue omit_future_added_value =
      Ppx_config.update_config (fun current ->
          { current with future_added_value = not omit_future_added_value })
    in
    let handleApolloMode apollo_mode =
      Ppx_config.update_config (fun current -> { current with apollo_mode })
    in
    let handleUncurried uncurried =
      Ppx_config.update_config (fun current -> { current with uncurried })
    in
    let handleSchema schema_file =
      Ppx_config.update_config (fun current -> { current with schema_file })
    in
    let handleAstOut ast_out =
      Ppx_config.update_config (fun current ->
          {
            current with
            output_mode =
              (match ast_out with
              | "apollo" -> Ppx_config.Apollo_AST
              | "string" -> Ppx_config.String
              | other ->
                raise
                  (Config_error
                     ("Error in graphql configuration: ast-out \"" ^ other
                    ^ "\" is not supported. Choose either apollo or string.")));
          })
    in
    let handleFragmentInQuery mode =
      match mode with
      | "include" ->
        Ppx_config.update_config (fun current ->
            { current with fragment_in_query = Include })
      | "exclude" ->
        Ppx_config.update_config (fun current ->
            { current with fragment_in_query = Exclude })
      | other ->
        raise
          (Config_error
             ("Error in graphql-ppx configuration: fragment-in-query\"" ^ other
            ^ "\" is not supported. Choose either 'include' or 'exclude'."))
    in
    let handleExtendQuery extend_query =
      Ppx_config.update_config (fun current ->
          { current with extend_query = Some extend_query })
    in
    let handleExtendQueryNoRequiredVariables extend_query =
      Ppx_config.update_config (fun current ->
          {
            current with
            extend_query_no_required_variables = Some extend_query;
          })
    in
    let handleExtendMutation extend_mutation =
      Ppx_config.update_config (fun current ->
          { current with extend_mutation = Some extend_mutation })
    in
    let handleExtendMutationNoRequiredVariables extend_mutation =
      Ppx_config.update_config (fun current ->
          {
            current with
            extend_mutation_no_required_variables = Some extend_mutation;
          })
    in
    let handleExtendSubscription extend_subscription =
      Ppx_config.update_config (fun current ->
          { current with extend_subscription = Some extend_subscription })
    in
    let handleExtendSubscriptionNoRequiredVariables extend_subscription =
      Ppx_config.update_config (fun current ->
          {
            current with
            extend_subscription_no_required_variables = Some extend_subscription;
          })
    in
    let handleExtendFragment extend_fragment =
      Ppx_config.update_config (fun current ->
          { current with extend_fragment = Some extend_fragment })
    in
    let handleTemplateTag template_tag =
      Ppx_config.update_config (fun current ->
          { current with template_tag = Some template_tag })
    in
    let handleTemplateTagImport template_tag_import =
      Ppx_config.update_config (fun current ->
          { current with template_tag_import = Some template_tag_import })
    in
    let handleTemplateTagLocation template_tag_location =
      Ppx_config.update_config (fun current ->
          { current with template_tag_location = Some template_tag_location })
    in
    let handleTemplateTagReturnType template_tag_return_type =
      Ppx_config.update_config (fun current ->
          {
            current with
            template_tag_return_type = Some template_tag_return_type;
          })
    in
    let handleTemplateTagIsFunction template_tag_is_function =
      Ppx_config.update_config (fun current ->
          {
            current with
            template_tag_is_function = Some template_tag_is_function;
          })
    in
    let configBool key value = ppxConfig |> JsonHelper.mapBool key value in
    let configString key value = ppxConfig |> JsonHelper.mapString key value in

    configBool "verbose" handleVerboseLogging;
    configBool "future-added-value" handleFutureAddedValue;
    configBool "futureAddedValue" handleFutureAddedValue;
    configBool "omitFutureAddedValue" handleOmitFutureAddedValue;
    configBool "apollo-mode" handleApolloMode;
    configBool "apolloMode" handleApolloMode;
    configString "schema" handleSchema;
    configString "ast-out" handleAstOut;
    configString "astOut" handleAstOut;
    configString "fragment-in-query" handleFragmentInQuery;
    configString "fragmentInQuery" handleFragmentInQuery;
    configString "extend-query" handleExtendQuery;
    configString "extendQuery" handleExtendQuery;
    configString "extend-query-no-required-variables"
      handleExtendQueryNoRequiredVariables;
    configString "extendQueryNoRequiredVariables"
      handleExtendQueryNoRequiredVariables;
    configString "extend-mutation" handleExtendMutation;
    configString "extendMutation" handleExtendMutation;
    configString "extend-mutation-no-required-variables"
      handleExtendMutationNoRequiredVariables;
    configString "extendMutationNoRequiredVariables"
      handleExtendMutationNoRequiredVariables;
    configString "extend-subscription" handleExtendSubscription;
    configString "extendSubscription" handleExtendSubscription;
    configString "extend-subscription-no-required-variables"
      handleExtendSubscriptionNoRequiredVariables;
    configString "extendSubscriptionNoRequiredVariables"
      handleExtendSubscriptionNoRequiredVariables;
    configString "extend-fragment" handleExtendFragment;
    configString "extendFragment" handleExtendFragment;
    configString "template-tag" handleTemplateTag;
    configString "templateTag" handleTemplateTag;
    configString "template-tag-import" handleTemplateTagImport;
    configString "templateTagImport" handleTemplateTagImport;
    configString "template-tag-location" handleTemplateTagLocation;
    configString "templateTagLocation" handleTemplateTagLocation;
    configString "template-tag-return-type" handleTemplateTagReturnType;
    configString "templateTagReturnType" handleTemplateTagReturnType;
    configBool "template-tag-is-function" handleTemplateTagIsFunction;
    configBool "templateTagIsFunction" handleTemplateTagIsFunction;
    configBool "uncurried" handleUncurried;
    read_custom_fields ppxConfig
  in
  match Paths.getBsConfigFile () with
  | Some bsConfigFile -> (
    try bsConfigFile |> Json.Read.from_file |> parseConfig with
    | Config_error _ as e -> raise e
    | _ -> ())
  | None -> ()
