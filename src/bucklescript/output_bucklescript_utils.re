open Graphql_ppx_base;
open Migrate_parsetree;
open Ast_408;

let conv_pos = pos => {
  Lexing.pos_fname: pos.Source_pos.pos_fname,
  Lexing.pos_lnum: pos.Source_pos.pos_lnum,
  Lexing.pos_bol: pos.Source_pos.pos_bol,
  Lexing.pos_cnum: pos.Source_pos.pos_cnum,
};

let conv_loc = loc => {
  Location.loc_start: conv_pos(loc.Source_pos.loc_start),
  Location.loc_end: conv_pos(loc.Source_pos.loc_end),
  Location.loc_ghost: loc.Source_pos.loc_ghost,
};

let conv_pos_from_ast = pos => {
  Source_pos.pos_fname: pos.Lexing.pos_fname,
  Source_pos.pos_lnum: pos.Lexing.pos_lnum,
  Source_pos.pos_bol: pos.Lexing.pos_bol,
  Source_pos.pos_cnum: pos.Lexing.pos_cnum,
};

let conv_loc_from_ast = loc => {
  Source_pos.loc_start: conv_pos_from_ast(loc.Location.loc_start),
  Source_pos.loc_end: conv_pos_from_ast(loc.Location.loc_end),
  Source_pos.loc_ghost: loc.Location.loc_ghost,
};

let extend_loc_from_start = (loc: Location.t, cnum) => {
  {
    ...loc,
    loc_end: {
      ...loc.loc_start,
      pos_cnum: loc.loc_start.pos_cnum + cnum,
    },
  };
};

let base_type_name = name =>
  Ast_helper.(
    Typ.constr({txt: Longident.parse(name), loc: Location.none}, [])
  );
let const_str_expr = s => Ast_helper.(Exp.constant(Pconst_string(s, None)));
let const_str_pat = s => Ast_helper.(Pat.constant(Pconst_string(s, None)));
let ident_from_string = (~loc=Location.none, ident) =>
  Ast_helper.(Exp.ident(~loc, {txt: Longident.parse(ident), loc}));

let to_valid_ident = ident =>
  if (ident.[0] >= '0' && ident.[0] <= '9') {
    "_" ++ ident;
  } else {
    // from gist of sgrove, source:
    // https://gist.github.com/sgrove/335bf1759d8d2f685dfea80d4e6afac7
    [
      "and",
      "as",
      "asr",
      "assert",
      "begin",
      "class",
      "constraint",
      "do",
      "done",
      "downto",
      "else",
      "end",
      "esfun",
      "exception",
      "external",
      "false",
      "for",
      "fun",
      "function",
      "functor",
      "if",
      "in",
      "include",
      "inherit",
      "initializer",
      "land",
      "lazy",
      "let",
      "lor",
      "lsl",
      "lsr",
      "lxor",
      "match",
      "method",
      "mod",
      "module",
      "mutable",
      "new",
      "nonrec",
      "object",
      "of",
      "open",
      "open!",
      "or",
      "pri",
      "private",
      "pub",
      "public",
      "rec",
      "sig",
      "struct",
      "switch",
      "then",
      "to",
      "true",
      "try",
      "type",
      "val",
      "virtual",
      "when",
      "while",
      "with",
    ]
    |> List.exists(reserved_word => ident == reserved_word)
      ? ident ++ "_" : ident;
  };
