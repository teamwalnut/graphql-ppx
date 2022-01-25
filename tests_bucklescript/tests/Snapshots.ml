open TestFramework

let win = Sys.win32 || Sys.cygwin

let detect_platform () =
  let ic = Unix.open_process_in "uname" in
  let uname = input_line ic in
  let () = close_in ic in
  match (win, uname) with
  | true, _ -> "win32"
  | _, "Darwin" -> "darwin"
  | _ -> "linux"

let platform = detect_platform ()
let refmt_path = "./node_modules/rescript/" ^ platform ^ "/refmt.exe"
let ppx_path = "./_build/default/src/bucklescript_bin/bin.exe"

let rec really_read fd ~buf ?(start = 0) ?(length = 1024) () =
  if length <= 0 then ()
  else
    let bytes = Bytes.create length in
    let result = Unix.read fd bytes start length in
    match result with
    | r when r < length ->
      Buffer.add_subbytes buf bytes 0 r;
      Unix.close fd
    | r ->
      Buffer.add_subbytes buf bytes 0 r;
      really_read fd ~buf ~start:0 ~length ()

let start_ppx path opts =
  let in_read, in_write = Unix.pipe ~cloexec:true () in
  let err_read, err_write =
    try Unix.pipe ~cloexec:true ()
    with e ->
      Unix.close in_read;
      Unix.close in_write;
      raise e
  in
  let in_read_2, in_write_2 =
    try Unix.pipe ~cloexec:true ()
    with e ->
      Unix.close in_read;
      Unix.close in_write;
      Unix.close err_read;
      Unix.close err_write;
      raise e
  in
  let in_read_3, in_write_3 =
    try Unix.pipe ~cloexec:true ()
    with e ->
      Unix.close in_read;
      Unix.close in_write;
      Unix.close err_read;
      Unix.close err_write;
      Unix.close in_read_2;
      Unix.close in_write_2;
      raise e
  in
  let _ =
    try
      let _ =
        Unix.create_process refmt_path
          [| ""; "--parse=re"; "--print"; "binary"; path |]
          Unix.stdin in_write err_write
      in
      let output_opts =
        if win then [| "-"; "-o"; "-" |] else [| "/dev/stdin"; "/dev/stdout" |]
      in
      let _ =
        Unix.create_process ppx_path
          (Array.concat
             [ [| ""; "-schema"; "graphql_schema.json" |]; opts; output_opts ])
          in_read in_write_2 err_write
      in
      let _ =
        Unix.create_process refmt_path [| ""; "--parse=binary" |] in_read_2
          in_write_3 err_write
      in
      ()
    with e ->
      Unix.close in_write;
      Unix.close in_read;
      Unix.close err_write;
      Unix.close in_write_2;
      Unix.close in_read_2;
      Unix.close in_write_3;
      raise e
  in
  ( (in_read, in_write),
    (in_read_2, in_write_2),
    (in_read_3, in_write_3),
    (err_read, err_write) )

let continue_ppx
  ( (in_read, in_write),
    (in_read_2, in_write_2),
    (in_read_3, in_write_3),
    (err_read, err_write) ) =
  Unix.close in_read;
  Unix.close in_write;
  Unix.close in_read_2;
  Unix.close in_write_2;
  Unix.close err_write;
  Unix.close in_write_3;
  let output_buf = Buffer.create 1024 in
  let error_buf = Buffer.create 1024 in
  really_read in_read_3 ~buf:output_buf ();
  really_read err_read ~buf:error_buf ();
  let output = Buffer.contents output_buf in
  let error = Buffer.contents error_buf in
  (output, error)

type lineAction = Skip | Add | ModifyPath

let process_error error =
  let next_line_contains_filename = ref false in
  let buf = Buffer.create (String.length error) in
  error |> String.trim |> String.split_on_char '\n'
  |> List.iter (fun line ->
       let action =
         match (!next_line_contains_filename, String.sub line 0 13) with
         | true, _ ->
           next_line_contains_filename := false;
           ModifyPath
         | _, "Command line:" -> Skip
         | _, "We've found a" | _, "  We've found" ->
           next_line_contains_filename := true;
           Add
         | _ -> Add
         | exception Invalid_argument _ -> Add
       in
       match action with
       | Add ->
         Buffer.add_string buf line;
         Buffer.add_char buf '\n'
       | Skip -> ()
       | ModifyPath ->
         let segments = String.split_on_char '/' line in
         let first_segment = segments |> List.hd in
         let last_segment =
           match List.length segments > 1 with
           | true -> segments |> List.rev |> List.hd
           | false -> ""
         in
         Buffer.add_string buf first_segment;
         Buffer.add_string buf last_segment;
         Buffer.add_char buf '\n');
  String.trim (Buffer.contents buf)

let bsb_path = "./node_modules/rescript/" ^ platform ^ "/bsc.exe"

let start_bsb ~ppxOptions ~filename ~pathIn =
  let out_read, out_write = Unix.pipe ~cloexec:true () in
  let err_read, err_write =
    try Unix.pipe ~cloexec:true ()
    with e ->
      Unix.close out_read;
      Unix.close out_write;
      raise e
  in
  let _ =
    try
      let _ =
        Unix.create_process bsb_path
          [|
            "";
            "-I";
            "./utilities";
            "-w";
            "-30";
            "-ppx";
            ppx_path ^ " -schema=graphql_schema.json "
            ^ Array.fold_left
                (fun acc ppxOption -> if acc = "" then "" else " " ^ ppxOption)
                "" ppxOptions;
            pathIn ^ "/" ^ filename;
          |]
          Unix.stdin out_write err_write
      in
      ()
    with e ->
      Unix.close out_read;
      Unix.close out_write;
      Unix.close err_read;
      Unix.close err_write;
      raise e
  in
  ((out_read, out_write), (err_read, err_write))

let continue_bsb ((out_read, out_write), (err_read, err_write)) =
  Unix.close out_write;
  Unix.close err_write;
  let output_buf = Buffer.create 1024 in
  let error_buf = Buffer.create 1024 in
  really_read out_read ~buf:output_buf ();
  really_read err_read ~buf:error_buf ();
  let output = Buffer.contents output_buf in
  let error = process_error (Buffer.contents error_buf) in
  (output, error)

let filenames =
  Sys.readdir "tests_bucklescript/operations"
  |> Array.to_list
  |> List.filter (fun name ->
       match String.split_on_char '.' name |> List.rev with
       | [ "re"; _ ] -> true
       | _ -> false)

let error_filenames =
  Sys.readdir "tests_bucklescript/operations/errors"
  |> Array.to_list
  |> List.filter (fun name ->
       match String.split_on_char '.' name |> List.rev with
       | [ "re"; _ ] -> true
       | _ -> false)

type ppx_config = { name : string; options : string array; native : bool }

let ppx_configs =
  [
    { name = "Records"; options = [||]; native = false };
    {
      name = "Template";
      options = [| "-template-tag-location=gql" |];
      native = false;
    };
    { name = "Apollo"; options = [| "-apollo-mode" |]; native = false };
    { name = "Native"; options = [| "-native" |]; native = true };
  ]

type test_type = Generate | Compile | Error

let test_types = [ Generate; Compile; Error ]

type ('a, 'b) descriptor = Ppx of 'a | Bsb of 'b

type ('a, 'b) test = {
  id : int;
  test_type : test_type;
  ppx_config : ppx_config;
  filename : string;
  descriptors : ('a, 'b) descriptor option;
}

let increm = ref 1

let get_id () =
  increm := !increm + 1;
  !increm

let tests =
  ListLabels.map test_types ~f:(fun test_type ->
    ListLabels.map ppx_configs ~f:(fun ppx_config ->
      match test_type with
      | Generate | Compile ->
        if ppx_config.native && test_type = Compile then []
        else
          ListLabels.map filenames ~f:(fun filename ->
            {
              id = get_id ();
              test_type;
              ppx_config;
              filename;
              descriptors = None;
            })
      | Error ->
        if not ppx_config.native then
          ListLabels.map error_filenames ~f:(fun filename ->
            {
              id = get_id ();
              test_type;
              ppx_config;
              filename;
              descriptors = None;
            })
        else []))

let concurrent_processes = 30
let inflight_files = ref (tests |> List.flatten |> List.flatten)

let fill_inflight () =
  inflight_files :=
    ListLabels.mapi !inflight_files ~f:(fun i test ->
      match test with
      | { descriptors = None; _ } when i < concurrent_processes ->
        {
          test with
          descriptors =
            Some
              (match test with
              | { test_type = Generate; filename; ppx_config; _ } ->
                Ppx
                  (start_ppx
                     ("tests_bucklescript/operations/" ^ filename)
                     ppx_config.options)
              | { test_type = Compile; filename; ppx_config; _ } ->
                Bsb
                  (start_bsb ~ppxOptions:ppx_config.options ~filename
                     ~pathIn:"tests_bucklescript/operations")
              | { test_type = Error; filename; ppx_config; _ } ->
                Bsb
                  (start_bsb ~ppxOptions:ppx_config.options ~filename
                     ~pathIn:"tests_bucklescript/operations/errors"));
        }
      | test -> test)

let remove_inflight id =
  inflight_files :=
    !inflight_files |> List.filter (fun { id = this_id; _ } -> id <> this_id)

let get_descriptors id =
  match List.find (fun { id = this_id; _ } -> this_id = id) !inflight_files with
  | { descriptors = Some descriptors; _ } ->
    remove_inflight id;
    descriptors
  | { descriptors = None; _ } -> raise Not_found

let get_ppx_descriptors = function
  | Ppx descriptor -> descriptor
  | _ -> raise Not_found

let get_bsb_descriptors = function
  | Bsb descriptor -> descriptor
  | _ -> raise Not_found

let get_type_and_config tests =
  match tests with
  | { ppx_config; test_type; _ } :: _ -> (test_type, ppx_config)
  | _ -> raise Not_found
;;

ListLabels.iter tests ~f:(fun tests_by_type ->
  ListLabels.iter tests_by_type ~f:(fun tests_by_config ->
    match tests_by_config with
    | [] -> ()
    | _ ->
      let test_type, ppx_config = get_type_and_config tests_by_config in
      let typeName =
        match test_type with
        | Generate -> "Generate"
        | Compile -> "Compile"
        | Error -> "Error"
      in
      describe
        (typeName ^ " " ^ ppx_config.name)
        (fun { describe; _ } ->
          ListLabels.iter tests_by_config ~f:(fun { filename; id; _ } ->
            describe filename (fun { test; _ } ->
              test "output" (fun { expect; _ } ->
                fill_inflight ();
                match test_type with
                | Generate ->
                  let descriptors =
                    id |> get_descriptors |> get_ppx_descriptors
                  in
                  let output, error = continue_ppx descriptors in
                  (expect.string output).toMatchSnapshot ();
                  (expect.string error).toEqual ""
                | Compile ->
                  let descriptors =
                    id |> get_descriptors |> get_bsb_descriptors
                  in
                  let output, error = descriptors |> continue_bsb in
                  (expect.string output).toMatchSnapshot ();
                  (expect.string error).toEqual ""
                | Error ->
                  let descriptors =
                    id |> get_descriptors |> get_bsb_descriptors
                  in
                  let _, error = descriptors |> continue_bsb in
                  (expect.string error).toMatchSnapshot ()))))))
