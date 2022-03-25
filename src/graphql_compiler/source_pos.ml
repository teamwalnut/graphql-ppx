type source_position = { index : int; line : int; col : int }
type span = source_position * source_position
type 'a spanning = { item : 'a; span : span }

let origin = { index = 0; line = 0; col = 0 }
let nopos = { index = -1; line = -1; col = -1 }
let nospan = (nopos, nopos)

let advance_line { index; line; col = _ } =
  { index = index + 1; line = line + 1; col = 0 }

let advance_col { index; line; col } =
  { index = index + 1; line; col = col + 1 }

let zero_width pos item = { span = (pos, pos); item }
let single_width pos item = { span = (pos, advance_col pos); item }
let start_end startPos endPos item = { span = (startPos, endPos); item }
let replace span item = { span = span.span; item }
let map f span = { span = span.span; item = f span.item }
let start_pos span = fst span.span
let end_pos span = snd span.span

type ast_position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}

type ast_location = {
  loc_start : ast_position;
  loc_end : ast_position;
  loc_ghost : bool;
}
