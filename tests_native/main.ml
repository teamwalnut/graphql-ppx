let suites =
  [
    ("Argument named 'query'", Arg_named_query.tests);
    ("Custom decoder", Custom_decoder.tests);
    ("Custom scalars", Custom_scalars.tests);
    ("Enum input", Enum_input.tests);
    ("Fragment definition", Fragment_definition.tests);
    ("Interface", Interface.tests)
    (* ("Lists", Lists.tests);
       ("List arguments", List_args.tests);
       ("List inputs", List_inputs.tests);
       ("Mutations", Mutation.tests);
       ("Nested decoding", Nested.tests);
       ("Nonrecursive input", Nonrecursive_input.tests);
       ("Records", Record.tests);
       ("Recursive input", Recursive_input.tests);
       ("Scalars", Scalars.tests);
       ("Scalar arguments", Scalars_args.tests);
       ("Scalar inputs", Scalars_input.tests);
       ("Skip directives", Skip_directives.tests);
       ("Typename", Typename.tests);
       ("Unions", Union.tests);
       ("Partial unions", Union_partial.tests);
       ("Variant conversion", Variant.tests); *);
  ]
;;

(* let yellow = "\027[0;33m" in *)
let green = "\027[32m" in
let red = "\027[31m" in
let reset = "\027[0m" in
let indent = "- " in

let print_success s = print_endline (indent ^ "✅ " ^ green ^ s ^ reset) in
let print_error s = print_endline (indent ^ red ^ "❌ " ^ s ^ reset) in
let failure = ref false in

suites
|> List.iter (fun (suite_name, tests) ->
     print_endline (suite_name ^ reset);
     tests
     |> List.iter (fun (test_name, test) ->
          match test () with
          | Test_shared.Pass -> print_success test_name
          | Fail (a, b) ->
            print_error test_name;
            (Format.fprintf Format.std_formatter "  a: @[%a@]@." a) ();
            (Format.fprintf Format.std_formatter "  b: @[%a@]@." b) ();
            failure := true));

if !failure then exit 1 else ()
