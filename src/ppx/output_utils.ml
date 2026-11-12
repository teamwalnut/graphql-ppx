open Graphql_compiler
open Ppxlib

let conv_pos pos =
  {
    Lexing.pos_fname = pos.Source_pos.pos_fname;
    Lexing.pos_lnum = pos.Source_pos.pos_lnum;
    Lexing.pos_bol = pos.Source_pos.pos_bol;
    Lexing.pos_cnum = pos.Source_pos.pos_cnum;
  }

let extend_loc_from_start (loc : Location.t) cnum =
  {
    loc with
    loc_end = { loc.loc_start with pos_cnum = loc.loc_start.pos_cnum + cnum };
  }

let base_type_name ?(loc = Location.none) name =
  Ast_helper.Typ.constr { txt = Longident.parse name; loc } []

let base_type ?(inner = []) ?loc name =
  Ast_helper.Typ.constr
    {
      Location.txt = Longident.parse name;
      loc = (match loc with None -> Location.none | Some loc -> loc);
    }
    inner

let const_str_expr s =
  Ast_helper.Exp.constant (Pconst_string (s, Location.none, None))

let const_str_pat s =
  Ast_helper.Pat.constant (Pconst_string (s, Location.none, None))

let ident_from_string ?(loc = Location.none) ident =
  Ast_helper.Exp.ident ~loc { txt = Longident.parse ident; loc }

let mkloc txt loc = { Location.txt; loc }
let mknoloc txt = mkloc txt Location.none

module Conv = Ppxlib_ast.Select_ast (Ppxlib_ast__.Versions.OCaml_current)
module Of_ppxlib = Conv.To_ocaml
module To_ppxlib = Conv.Of_ocaml

let to_valid_ident ident =
  if ident.[0] >= '0' && ident.[0] <= '9' then "_" ^ ident
  else
    match
      [
        "and";
        "as";
        "asr";
        "assert";
        "begin";
        "class";
        "constraint";
        "do";
        "done";
        "downto";
        "else";
        "end";
        "esfun";
        "exception";
        "external";
        "false";
        "for";
        "fun";
        "function";
        "functor";
        "if";
        "in";
        "include";
        "inherit";
        "initializer";
        "land";
        "lazy";
        "let";
        "lor";
        "lsl";
        "lsr";
        "lxor";
        "match";
        "method";
        "mod";
        "module";
        "mutable";
        "new";
        "nonrec";
        "object";
        "of";
        "open";
        "open!";
        "or";
        "pri";
        "private";
        "pub";
        "public";
        "rec";
        "sig";
        "struct";
        "switch";
        "then";
        "to";
        "true";
        "try";
        "type";
        "val";
        "virtual";
        "when";
        "while";
        "with";
      ]
      |> List.exists (fun reserved_word -> ident = reserved_word)
    with
    | true -> ident ^ "_"
    | false -> ident

(* Like to_valid_ident but also handles uppercase-starting names
   which are invalid for record field names in ReScript.
   Uses the \"Name" escape syntax to preserve the original name. *)
let to_valid_field_name ident =
  let ident = to_valid_ident ident in
  if ident.[0] >= 'A' && ident.[0] <= 'Z' then "\\\"" ^ ident ^ "\"" else ident

let ppxlib_position (pos : Source_pos.ast_position) =
  {
    Ppxlib.pos_fname = pos.pos_fname;
    pos_lnum = pos.pos_lnum;
    pos_bol = pos.pos_bol;
    pos_cnum = pos.pos_cnum;
  }

let ppxlib_position_to_ast (pos : Ppxlib.position) =
  {
    Source_pos.pos_fname = pos.pos_fname;
    pos_lnum = pos.pos_lnum;
    pos_bol = pos.pos_bol;
    pos_cnum = pos.pos_cnum;
  }

let ppxlib_location (loc : Source_pos.ast_location) =
  {
    Ppxlib.loc_start = ppxlib_position loc.loc_start;
    loc_end = ppxlib_position loc.loc_end;
    loc_ghost = loc.loc_ghost;
  }

let ppxlib_location_to_ast (loc : Ppxlib.location) =
  {
    Source_pos.loc_start = ppxlib_position_to_ast loc.loc_start;
    loc_end = ppxlib_position_to_ast loc.loc_end;
    loc_ghost = loc.loc_ghost;
  }

let conv_loc loc = ppxlib_location loc
let conv_pos_from_ast pos = ppxlib_position pos
let conv_loc_from_ast loc = ppxlib_location_to_ast loc
