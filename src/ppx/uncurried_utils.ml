open Ppxlib
open Ocaml_common
open Parsetree
open Graphql_compiler

let function_expression_uncurried ?(loc = Location.none) ~arity funExpr =
  let arity_to_attributes ~loc arity : Parsetree.attribute list =
    [
      {
        attr_name = Location.mknoloc "res.arity";
        attr_payload =
          Parsetree.PStr
            [
              Ast_helper.Str.eval
                (Ast_helper.Exp.constant
                   (Pconst_integer (string_of_int arity, None)));
            ];
        attr_loc = loc;
      };
    ]
  in
  Ast_helper.Exp.construct ~loc
    ~attrs:(arity_to_attributes ~loc arity)
    (Location.mknoloc (Longident.Lident "Function$"))
    (Some funExpr)

let wrap_function_exp_uncurried ?(arity = 1) expr =
  if Ppx_config.uncurried () then function_expression_uncurried ~arity expr
  else expr

let ctyp_arrow ?(loc = Location.none) ~arity tArg =
  let encode_arity_string arity = "Has_arity" ^ string_of_int arity in
  let arity_type ?(loc = Location.none) arity =
    Ast_helper.Typ.variant ~loc
      [
        {
          prf_loc = loc;
          prf_attributes = [];
          prf_desc = Rtag ({ txt = encode_arity_string arity; loc }, true, []);
        };
      ]
      Closed None
  in
  let tArity = arity_type ~loc arity in
  Ast_helper.Typ.constr ~loc { txt = Lident "function$"; loc } [ tArg; tArity ]

let wrap_core_type_uncurried ?(arity = 1) typ =
  match Ppx_config.uncurried () with
  | false -> typ
  | true -> ctyp_arrow ~loc:typ.ptyp_loc ~arity typ

let rec determineArity ~arity expr =
  match expr.pexp_desc with
  | Pexp_fun (_, _, _, fn) -> determineArity ~arity:(arity + 1) fn
  | _ -> arity

let wrap_as_uncurried_vb ?(arity = 1) item =
  match (Ppx_config.uncurried (), item) with
  | false, _ -> item
  | _, ({ pvb_expr = { pexp_desc = Pexp_fun _ } as fn } as outerV) ->
    {
      outerV with
      pvb_expr = function_expression_uncurried ~loc:outerV.pvb_loc ~arity fn;
    }
  | _ -> item

let wrap_as_uncurried_fn ?(arity = 1) item =
  match (Ppx_config.uncurried (), item.pstr_desc) with
  | false, _ -> item
  | _, Pstr_value (a1, [ value_binding ]) ->
    {
      item with
      pstr_desc = Pstr_value (a1, [ wrap_as_uncurried_vb ~arity value_binding ]);
    }
  | _ -> item

let wrap_as_uncurried_fn_multi ?(arity = 1) item =
  match (Ppx_config.uncurried (), item.pstr_desc) with
  | false, _ -> item
  | _, Pstr_value (a1, value_bindings) ->
    let new_value_bindings =
      value_bindings
      |> List.map (function
           | { pvb_expr = { pexp_desc = Pexp_fun _ } as fn } as outerV ->
             {
               outerV with
               pvb_expr =
                 function_expression_uncurried ~loc:outerV.pvb_loc ~arity fn;
             }
           | other -> other)
    in
    { item with pstr_desc = Pstr_value (a1, new_value_bindings) }
  | _ -> item

let rec determine_typ_arity ?(arity = 1) = function
  | { ptyp_desc = Ptyp_arrow (_, _t1, t2) } ->
    determine_typ_arity ~arity:(arity + 1) t2
  | _ -> arity

let handle_typ ~arity typ =
  match typ.ptyp_desc with
  | Ptyp_arrow (_, _t1, _t2) -> ctyp_arrow ~loc:typ.ptyp_loc ~arity typ
  | _ -> typ

let wrap_sig_uncurried_fn ?(arity = 1) item =
  match (Ppx_config.uncurried (), item.psig_desc) with
  | false, _ -> item
  | ( _,
      Psig_value
        ({ pval_type = { ptyp_desc = Ptyp_arrow _ } as type_ } as psig_value) )
    ->
    {
      item with
      psig_desc =
        Psig_value { psig_value with pval_type = handle_typ ~arity type_ };
    }
  | _ -> item

let handle_str_item item =
  match item.pstr_desc with
  | Pstr_value
      (a1, [ ({ pvb_expr = { pexp_desc = Pexp_fun _ } as fn } as outerV) ]) ->
    {
      item with
      pstr_desc =
        Pstr_value
          ( a1,
            [
              {
                outerV with
                pvb_expr =
                  function_expression_uncurried ~loc:outerV.pvb_loc
                    ~arity:(determineArity ~arity:1 fn)
                    fn;
              };
            ] );
    }
  | _ -> item

let attr_uapp : Ppxlib.Parsetree.attribute =
  {
    attr_name = { txt = "res.uapp"; loc = Location.none };
    attr_payload = PStr [];
    attr_loc = Location.none;
  }

let add_attrs attrs e = { e with pexp_attributes = attrs }

let add_uapp e =
  add_attrs
    (if Ppx_config.uncurried () then attr_uapp :: e.pexp_attributes
    else e.pexp_attributes)
    e
