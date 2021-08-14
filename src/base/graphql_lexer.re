open Result;

open Source_pos;

type lexer = {
  source: string,
  length: int,
  mutable position: source_position,
  mutable has_reached_eof: bool,
};

type token =
  | Name(string)
  | Int(int)
  | Float(float)
  | String(string)
  | Exclamation_mark
  | Dollar
  | Paren_open
  | Paren_close
  | Bracket_open
  | Bracket_close
  | Curly_open
  | Curly_close
  | Ellipsis
  | Dot
  | Colon
  | Equals
  | At
  | Pipe
  | Ampersand
  | End_of_file;

let string_of_token = t =>
  switch (t) {
  | Name(s) => s
  | Int(i) => string_of_int(i)
  | Float(f) => Printf.sprintf("%.16g", f)
  | String(s) => "\"" ++ s ++ "\""
  | Exclamation_mark => "!"
  | Dollar => "$"
  | Paren_open => "("
  | Paren_close => ")"
  | Bracket_open => "["
  | Bracket_close => "]"
  | Curly_open => "{"
  | Curly_close => "}"
  | Ellipsis => "..."
  | Dot => "."
  | Colon => ":"
  | Equals => "="
  | At => "@"
  | Pipe => "|"
  | Ampersand => "&"
  | End_of_file => "[EOF]"
  };

type lexerError =
  | Unknown_character(char)
  | Unexpected_character(char)
  | Unterminated_string
  | Unknown_character_in_string(char)
  | Unknown_escape_sequence(string)
  | Unexpected_end_of_file
  | Invalid_number;

let make = source => {
  source,
  length: String.length(source),
  position: origin,
  has_reached_eof: false,
};

let peek_char = lexer =>
  if (lexer.position.index >= lexer.length) {
    None;
  } else {
    Some((lexer.position.index, lexer.source.[lexer.position.index]));
  };

let peek_char_only = lexer => lexer |> peek_char |> Option.map(snd);

let next_char = lexer => {
  let next = peek_char(lexer);
  let () =
    switch (peek_char_only(lexer)) {
    | Some('\n') => lexer.position = advance_line(lexer.position)
    | Some(_) => lexer.position = advance_col(lexer.position)
    | _ => ()
    };
  next;
};

exception Internal_lexer_error;

let emit_single_char = (lexer, token) => {
  let start_pos = lexer.position;
  let _ =
    switch (next_char(lexer)) {
    | Some(next) => next
    | None => raise(Internal_lexer_error)
    };

  single_width(start_pos, token);
};

let rec scan_over_whitespace = lexer =>
  switch (peek_char_only(lexer)) {
  | Some('\t') =>
    let _ = next_char(lexer);
    scan_over_whitespace(lexer);
  | Some(' ') =>
    let _ = next_char(lexer);
    scan_over_whitespace(lexer);
  | Some('\n') =>
    let _ = next_char(lexer);
    scan_over_whitespace(lexer);
  | Some('\r') =>
    let _ = next_char(lexer);
    scan_over_whitespace(lexer);
  | Some(',') =>
    let _ = next_char(lexer);
    scan_over_whitespace(lexer);
  | Some('#') =>
    let _ = next_char(lexer);
    scan_to_end_of_line(lexer);
  | _ => ()
  }
and scan_to_end_of_line = lexer =>
  switch (peek_char_only(lexer)) {
  | Some('\n') =>
    let _ = next_char(lexer);
    scan_over_whitespace(lexer);
  | Some('\r') =>
    let _ = next_char(lexer);
    scan_over_whitespace(lexer);
  | Some(_) =>
    let _ = next_char(lexer);
    scan_to_end_of_line(lexer);
  | None => ()
  };

let is_name_start = c =>
  c == '_' || c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z';

let is_digit = c => c >= '0' && c <= '9';

let is_name_cont = c => is_name_start(c) || is_digit(c);

let is_number_start = c => is_digit(c) || c == '-';

let scan_name = lexer => {
  let start_pos = lexer.position;
  switch (next_char(lexer)) {
  | None => Error(zero_width(lexer.position, Unexpected_end_of_file))
  | Some((start_idx, _)) =>
    let rec scan_loop = end_idx =>
      switch (peek_char(lexer)) {
      | Some((idx, ch)) when is_name_cont(ch) =>
        let _ = next_char(lexer);
        scan_loop(idx);
      | Some(_) => end_idx
      | None => end_idx
      };

    let endIdx = scan_loop(start_idx);
    Ok(
      start_end(
        start_pos,
        lexer.position,
        Name(String.sub(lexer.source, start_idx, endIdx - start_idx + 1)),
      ),
    );
  };
};

let scan_ellipsis_or_dot = lexer => {
  let start_pos = lexer.position;
  let rec scan_loop = i =>
    if (i == 0) {
      Ok(start_end(start_pos, lexer.position, Ellipsis));
    } else {
      switch (peek_char(lexer)) {
      | Some((_, '.')) =>
        let _ = next_char(lexer);
        scan_loop(i - 1);
      | Some((_, _)) when i == 2 =>
        Ok(start_end(start_pos, lexer.position, Dot))
      | Some((_, ch)) =>
        let _ = next_char(lexer);
        Error(single_width(lexer.position, Unexpected_character(ch)));
      | None => Error(zero_width(lexer.position, Unexpected_end_of_file))
      };
    };
  scan_loop(3);
};

let scan_digits = lexer => {
  let start_pos = lexer.position;
  switch (peek_char(lexer)) {
  | None => Error(zero_width(start_pos, Unexpected_end_of_file))
  | Some((start_idx, _)) =>
    let rec scan_loop = end_idx =>
      switch (peek_char(lexer)) {
      | Some((idx, ch)) when is_digit(ch) =>
        let _ = next_char(lexer);
        scan_loop(idx);
      | Some(_)
      | None => end_idx
      };

    let end_idx = scan_loop(start_idx);
    try(
      Ok(
        int_of_string(
          String.sub(lexer.source, start_idx, end_idx - start_idx + 1),
        ),
      )
    ) {
    | Failure(_) =>
      Error(start_end(start_pos, lexer.position, Invalid_number))
    };
  };
};

let scan_integer_part = lexer => {
  let is_negative =
    switch (peek_char_only(lexer)) {
    | Some('-') =>
      let _ = next_char(lexer);
      Ok(true);
    | Some(_) => Ok(false)
    | None => Error(zero_width(lexer.position, Unexpected_end_of_file))
    };

  switch (is_negative) {
  | Error(e) => Error(e)
  | Ok(neg) =>
    switch (scan_digits(lexer)) {
    | Error(e) => Error(e)
    | Ok(num) =>
      Ok(
        if (neg) {
          (-1) * num;
        } else {
          num;
        },
      )
    }
  };
};

let scan_number = lexer => {
  let start_pos = lexer.position;
  let build_number = (int_part, frac_part, exp_part) => {
    let mantissa =
      frac_part
      |> Option.map(float_of_int)
      |> Option.map(frac =>
           if (frac > 0.0) {
             frac /. 10.0 ** (frac |> log10 |> floor);
           } else {
             0.0;
           }
         )
      |> Option.map(m =>
           if (int_part < 0) {
             (-1.0) *. m;
           } else {
             m;
           }
         );

    let exp =
      exp_part |> Option.map(float_of_int) |> Option.map(e => 10.0 ** e);

    let num_token =
      switch (mantissa, exp) {
      | (None, None) => Int(int_part)
      | (None, Some(exp)) => Float(float_of_int(int_part) *. exp)
      | (Some(mantissa), None) => Float(float_of_int(int_part) +. mantissa)
      | (Some(mantissa), Some(exp)) =>
        Float((float_of_int(int_part) +. mantissa) ** exp)
      };

    Ok(start_end(start_pos, lexer.position, num_token));
  };

  let scan_exp_part = (int_part, frac_part) =>
    switch (peek_char_only(lexer)) {
    | Some('e')
    | Some('E') =>
      let _ = next_char(lexer);
      switch (scan_integer_part(lexer)) {
      | Error(e) => Error(e)
      | Ok(exp_part) => build_number(int_part, frac_part, Some(exp_part))
      };
    | None
    | Some(_) => build_number(int_part, frac_part, None)
    };

  switch (scan_integer_part(lexer)) {
  | Error(e) => Error(e)
  | Ok(int_part) =>
    switch (peek_char_only(lexer)) {
    | Some('.') =>
      let _ = next_char(lexer);
      switch (scan_digits(lexer)) {
      | Error(e) => Error(e)
      | Ok(digits) => scan_exp_part(int_part, Some(digits))
      };
    | Some(_)
    | None => scan_exp_part(int_part, None)
    }
  };
};

let scan_string = (~start_pos, lexer) => {
  switch (peek_char(lexer)) {
  | None => Error(zero_width(start_pos, Unexpected_end_of_file))
  | Some(_) =>
    let rec scan_loop = acc =>
      switch (peek_char_only(lexer)) {
      | None => Error(zero_width(lexer.position, Unterminated_string))
      | Some('\n')
      | Some('\r') =>
        Error(single_width(lexer.position, Unterminated_string))
      | Some('"') =>
        let _ = next_char(lexer);
        Ok(start_end(start_pos, lexer.position, String(acc)));
      | Some('\\') =>
        let _ = next_char(lexer);
        switch (peek_char_only(lexer)) {
        | None => Error(zero_width(lexer.position, Unterminated_string))
        | Some('"') =>
          let _ = next_char(lexer);
          // string concat here maybe not optimal, but strings do not occur
          // super frequently - and are short - and a Buffer also has some overhead
          acc ++ "\"" |> scan_loop;
        | Some('\\') =>
          let _ = next_char(lexer);
          acc ++ "\\" |> scan_loop;
        | Some('/') =>
          let _ = next_char(lexer);
          acc ++ "/" |> scan_loop;
        | Some('b') =>
          let _ = next_char(lexer);
          acc ++ "\n" |> scan_loop;
        | Some('f') =>
          let _ = next_char(lexer);
          acc ++ "\014" |> scan_loop;
        | Some('n') =>
          let _ = next_char(lexer);
          acc ++ "\n" |> scan_loop;
        | Some('r') =>
          let _ = next_char(lexer);
          acc ++ "\r" |> scan_loop;
        | Some('t') =>
          let _ = next_char(lexer);
          acc ++ "\t" |> scan_loop;
        | Some('u') =>
          Error(
            single_width(lexer.position, Unknown_escape_sequence("\\uXXXX")),
          )
        | Some(ch) =>
          Error(
            single_width(
              lexer.position,
              Unknown_escape_sequence("\\" ++ String.make(1, ch)),
            ),
          )
        };
      | Some(ch) =>
        let _ = next_char(lexer);
        acc ++ String.make(1, ch) |> scan_loop;
      };

    scan_loop("");
  };
};
let scan_block_string = (~start_pos, lexer) => {
  let rec scan_loop = acc =>
    switch (peek_char_only(lexer)) {
    | None => Error(zero_width(lexer.position, Unterminated_string))
    | Some('"') =>
      let _ = next_char(lexer);
      switch (peek_char_only(lexer)) {
      | Some('"') =>
        let _ = next_char(lexer);
        switch (peek_char_only(lexer)) {
        | Some('"') =>
          let _ = next_char(lexer);
          // TODO: format block string according to:
          // https://spec.graphql.org/June2018/#BlockStringValue()
          // acc = blockStringValue(acc)
          Ok(start_end(start_pos, lexer.position, String(acc)));
        | Some(_) => acc ++ "\"\"" |> scan_loop
        | None => Error(zero_width(lexer.position, Unterminated_string))
        };
      | Some(_) => acc ++ "\"" |> scan_loop
      | None => Error(zero_width(lexer.position, Unterminated_string))
      };
    | Some(ch) =>
      let _ = next_char(lexer);
      acc ++ String.make(1, ch) |> scan_loop;
    };

  scan_loop("");
};

let scan_single_token = lexer =>
  if (lexer.has_reached_eof) {
    None;
  } else {
    let () = scan_over_whitespace(lexer);
    Some(
      switch (peek_char_only(lexer)) {
      | Some('!') => Ok(emit_single_char(lexer, Exclamation_mark))
      | Some('$') => Ok(emit_single_char(lexer, Dollar))
      | Some('(') => Ok(emit_single_char(lexer, Paren_open))
      | Some(')') => Ok(emit_single_char(lexer, Paren_close))
      | Some('[') => Ok(emit_single_char(lexer, Bracket_open))
      | Some(']') => Ok(emit_single_char(lexer, Bracket_close))
      | Some('{') => Ok(emit_single_char(lexer, Curly_open))
      | Some('}') => Ok(emit_single_char(lexer, Curly_close))
      | Some(':') => Ok(emit_single_char(lexer, Colon))
      | Some('=') => Ok(emit_single_char(lexer, Equals))
      | Some('@') => Ok(emit_single_char(lexer, At))
      | Some('|') => Ok(emit_single_char(lexer, Pipe))
      | Some('&') => Ok(emit_single_char(lexer, Ampersand))
      | Some('.') => scan_ellipsis_or_dot(lexer)
      | Some('"') =>
        let start_pos = lexer.position;
        let _ = next_char(lexer);
        switch (peek_char_only(lexer)) {
        | Some('"') =>
          let _ = next_char(lexer);
          switch (peek_char_only(lexer)) {
          | Some('"') =>
            let _ = next_char(lexer);
            scan_block_string(~start_pos, lexer);
          | None
          | Some(_) => Ok(start_end(start_pos, lexer.position, String("")))
          };
        | None
        | Some(_) => scan_string(~start_pos, lexer)
        };
      | Some(ch) when is_number_start(ch) => scan_number(lexer)
      | Some(ch) when is_name_start(ch) => scan_name(lexer)
      | Some(ch) =>
        Error(single_width(lexer.position, Unknown_character(ch)))
      | None =>
        let () = lexer.has_reached_eof = true;
        Ok(zero_width(lexer.position, End_of_file));
      },
    );
  };

let consume = lexer => {
  let rec consumer = acc =>
    switch (scan_single_token(lexer)) {
    | Some(Ok(t)) => consumer([t, ...acc])
    | Some(Error(e)) => Error(e)
    | None => Ok(acc)
    };

  switch (consumer([])) {
  | Ok(l) => Ok(List.rev(l))
  | Error(e) => Error(e)
  };
};
