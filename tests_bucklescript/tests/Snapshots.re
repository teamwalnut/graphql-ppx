open TestFramework;
let win = Sys.win32 || Sys.cygwin;

let detect_platform = () => {
  let ic = Unix.open_process_in("uname");
  let uname = input_line(ic);
  let () = close_in(ic);
  switch (win, uname) {
  | (true, _) => "win32"
  | (_, "Darwin") => "darwin"
  | _ => "linux"
  };
};
let platform = detect_platform();
let refmt_path = "./node_modules/bs-platform/" ++ platform ++ "/refmt.exe";
let ppx_path = "./_build/default/src/bucklescript_bin/bin.exe";

let rec really_read = (fd, ~buf, ~start=0, ~length=1024, ()) =>
  if (length <= 0) {
    ();
  } else {
    let bytes = Bytes.create(length);
    let result = Unix.read(fd, bytes, start, length);
    switch (result) {
    | r when r < length =>
      Buffer.add_subbytes(buf, bytes, 0, r);
      Unix.close(fd);
    | r =>
      Buffer.add_subbytes(buf, bytes, 0, r);
      really_read(fd, ~buf, ~start=0, ~length, ());
    };
  };

let start_ppx = (path, opts) => {
  let (in_read, in_write) = Unix.pipe(~cloexec=true, ());
  let (err_read, err_write) =
    try(Unix.pipe(~cloexec=true, ())) {
    | e =>
      Unix.close(in_read);
      Unix.close(in_write);
      raise(e);
    };
  let (in_read_2, in_write_2) =
    try(Unix.pipe(~cloexec=true, ())) {
    | e =>
      Unix.close(in_read);
      Unix.close(in_write);
      Unix.close(err_read);
      Unix.close(err_write);
      raise(e);
    };
  let (in_read_3, in_write_3) =
    try(Unix.pipe(~cloexec=true, ())) {
    | e =>
      Unix.close(in_read);
      Unix.close(in_write);
      Unix.close(err_read);
      Unix.close(err_write);
      Unix.close(in_read_2);
      Unix.close(in_write_2);
      raise(e);
    };

  let _ =
    try({
      let _ =
        Unix.create_process(
          refmt_path,
          [|"", "--parse=re", "--print", "binary", path|],
          Unix.stdin,
          in_write,
          err_write,
        );
      let output_opts =
        win ? [|"-", "-o", "-"|] : [|"/dev/stdin", "/dev/stdout"|];

      let _ =
        Unix.create_process(
          ppx_path,
          Array.concat([
            [|"", "-schema", "graphql_schema.json"|],
            opts,
            output_opts,
          ]),
          in_read,
          in_write_2,
          err_write,
        );
      let _ =
        Unix.create_process(
          refmt_path,
          [|"", "--parse=binary"|],
          in_read_2,
          in_write_3,
          err_write,
        );

      ();
    }) {
    | e =>
      Unix.close(in_write);
      Unix.close(in_read);
      Unix.close(err_write);
      Unix.close(in_write_2);
      Unix.close(in_read_2);
      Unix.close(in_write_3);
      raise(e);
    };

  (
    (in_read, in_write),
    (in_read_2, in_write_2),
    (in_read_3, in_write_3),
    (err_read, err_write),
  );
};
let continue_ppx =
    (
      (
        (in_read, in_write),
        (in_read_2, in_write_2),
        (in_read_3, in_write_3),
        (err_read, err_write),
      ),
    ) => {
  Unix.close(in_read);
  Unix.close(in_write);
  Unix.close(in_read_2);
  Unix.close(in_write_2);
  Unix.close(err_write);
  Unix.close(in_write_3);

  let output_buf = Buffer.create(1024);
  let error_buf = Buffer.create(1024);
  really_read(in_read_3, ~buf=output_buf, ());
  really_read(err_read, ~buf=error_buf, ());
  let output = Buffer.contents(output_buf);
  let error = Buffer.contents(error_buf);

  (output, error);
};

type lineAction =
  | Skip
  | Add
  | ModifyPath;
let process_error = error => {
  let next_line_contains_filename = ref(false);
  let buf = Buffer.create(String.length(error));

  error
  |> String.trim
  |> String.split_on_char('\n')
  |> List.iter(line => {
       let action =
         switch (next_line_contains_filename^, String.sub(line, 0, 13)) {
         // line starting with command line does not produce stable output
         | (true, _) =>
           next_line_contains_filename := false;
           ModifyPath;
         | (_, "Command line:") => Skip
         | (_, "We've found a")
         | (_, "  We've found") =>
           next_line_contains_filename := true;
           Add;
         | _ => Add
         | exception (Invalid_argument(_)) => Add
         };
       switch (action) {
       | Add =>
         Buffer.add_string(buf, line);
         Buffer.add_char(buf, '\n');
       | Skip => ()
       | ModifyPath =>
         let segments = String.split_on_char('/', line);
         let first_segment = segments |> List.hd;
         let last_segment =
           List.length(segments) > 1 ? segments |> List.rev |> List.hd : "";
         Buffer.add_string(buf, first_segment);
         Buffer.add_string(buf, last_segment);
         Buffer.add_char(buf, '\n');
       };
     });
  String.trim(Buffer.contents(buf));
};

let bsb_path = "./node_modules/bs-platform/" ++ platform ++ "/bsc.exe";
let start_bsb = (~ppxOptions, ~filename, ~pathIn) => {
  let (out_read, out_write) = Unix.pipe(~cloexec=true, ());
  let (err_read, err_write) =
    try(Unix.pipe(~cloexec=true, ())) {
    | e =>
      Unix.close(out_read);
      Unix.close(out_write);
      raise(e);
    };
  let _ =
    try({
      let _ =
        Unix.create_process(
          bsb_path,
          [|
            "",
            "-I",
            "./utilities",
            "-w",
            "-30",
            "-ppx",
            ppx_path
            ++ " -schema=graphql_schema.json "
            ++ Array.fold_left(
                 (acc, ppxOption) => (acc == "" ? "" : " ") ++ ppxOption,
                 "",
                 ppxOptions,
               ),
            pathIn ++ "/" ++ filename,
          |],
          Unix.stdin,
          out_write,
          err_write,
        );
      ();
    }) {
    | e =>
      Unix.close(out_read);
      Unix.close(out_write);
      Unix.close(err_read);
      Unix.close(err_write);
      raise(e);
    };
  ((out_read, out_write), (err_read, err_write));
};

let continue_bsb = (((out_read, out_write), (err_read, err_write))) => {
  Unix.close(out_write);
  Unix.close(err_write);

  let output_buf = Buffer.create(1024);
  let error_buf = Buffer.create(1024);
  really_read(out_read, ~buf=output_buf, ());
  really_read(err_read, ~buf=error_buf, ());
  let output = Buffer.contents(output_buf);
  let error = process_error(Buffer.contents(error_buf));

  (output, error);
};

let filenames =
  Sys.readdir("tests_bucklescript/operations")
  |> Array.to_list
  |> List.filter(name => {
       switch (String.split_on_char('.', name) |> List.rev) {
       | ["re", _] => true
       | _ => false
       }
     });

let error_filenames =
  Sys.readdir("tests_bucklescript/operations/errors")
  |> Array.to_list
  |> List.filter(name => {
       switch (String.split_on_char('.', name) |> List.rev) {
       | ["re", _] => true
       | _ => false
       }
     });

type ppx_config = {
  name: string,
  options: array(string),
  native: bool,
};

let ppx_configs = [
  {name: "Records", options: [||], native: false},
  {
    name: "Template",
    options: [|"-template-tag-location=gql"|],
    native: false,
  },
  {name: "Apollo", options: [|"-apollo-mode"|], native: false},
  {name: "Native", options: [|"-native"|], native: true},
];

type test_type =
  | Generate
  | Compile
  | Error;

let test_types = [Generate, Compile, Error];

type descriptor('a, 'b) =
  | Ppx('a)
  | Bsb('b);

type test('a, 'b) = {
  id: int,
  test_type,
  ppx_config,
  filename: string,
  descriptors: option(descriptor('a, 'b)),
};

let increm = ref(1);
let get_id = () => {
  increm := increm^ + 1;
  increm^;
};
let tests =
  test_types
  |> List.map(test_type =>
       ppx_configs
       |> List.map(ppx_config =>
            switch (test_type) {
            | Generate
            | Compile =>
              if (ppx_config.native && test_type == Compile) {
                [];
              } else {
                filenames
                |> List.map(filename =>
                     {
                       id: get_id(),
                       test_type,
                       ppx_config,
                       filename,
                       descriptors: None,
                     }
                   );
              }
            | Error =>
              if (!ppx_config.native) {
                error_filenames
                |> List.map(filename =>
                     {
                       id: get_id(),
                       test_type,
                       ppx_config,
                       filename,
                       descriptors: None,
                     }
                   );
              } else {
                [];
              }
            }
          )
     );

// on macOS the number of in-flight file descriptors is max 256
let concurrent_processes = 30;

let inflight_files = ref(tests |> List.flatten |> List.flatten);
let fill_inflight = () => {
  inflight_files :=
    inflight_files^
    |> List.mapi((i, test) =>
         switch (test) {
         | {descriptors: None, _} when i < concurrent_processes => {
             ...test,
             descriptors:
               Some(
                 switch (test) {
                 | {test_type: Generate, filename, ppx_config, _} =>
                   Ppx(
                     start_ppx(
                       "tests_bucklescript/operations/" ++ filename,
                       ppx_config.options,
                     ),
                   )
                 | {test_type: Compile, filename, ppx_config, _} =>
                   Bsb(
                     start_bsb(
                       ~ppxOptions=ppx_config.options,
                       ~filename,
                       ~pathIn="tests_bucklescript/operations",
                     ),
                   )

                 | {test_type: Error, filename, ppx_config, _} =>
                   Bsb(
                     start_bsb(
                       ~ppxOptions=ppx_config.options,
                       ~filename,
                       ~pathIn="tests_bucklescript/operations/errors",
                     ),
                   )
                 },
               ),
           }
         | test => test
         }
       );
};

let remove_inflight = id => {
  inflight_files :=
    inflight_files^ |> List.filter(({id: this_id, _}) => id != this_id);
};

let get_descriptors = id => {
  switch (List.find(({id: this_id, _}) => this_id == id, inflight_files^)) {
  | {descriptors: Some(descriptors), _} =>
    remove_inflight(id);
    descriptors;
  | {descriptors: None, _} => raise(Not_found)
  };
};

let get_ppx_descriptors =
  fun
  | Ppx(descriptor) => descriptor
  | _ => raise(Not_found);
let get_bsb_descriptors =
  fun
  | Bsb(descriptor) => descriptor
  | _ => raise(Not_found);

let get_type_and_config = tests =>
  switch (tests) {
  | [{ppx_config, test_type, _}, ..._] => (test_type, ppx_config)
  | _ => raise(Not_found)
  };

tests
|> List.iter(tests_by_type => {
     tests_by_type
     |> List.iter(tests_by_config => {
          switch (tests_by_config) {
          | [] => ()
          | _ =>
            let (test_type, ppx_config) =
              get_type_and_config(tests_by_config);
            let typeName =
              switch (test_type) {
              | Generate => "Generate"
              | Compile => "Compile"
              | Error => "Error"
              };

            describe(typeName ++ " " ++ ppx_config.name, ({describe, _}) => {
              tests_by_config
              |> List.iter(({filename, id, _}) => {
                   describe(filename, ({test, _}) => {
                     test("output", ({expect, _}) => {
                       fill_inflight();
                       switch (test_type) {
                       | Generate =>
                         let descriptors =
                           id |> get_descriptors |> get_ppx_descriptors;
                         let (output, error) = continue_ppx(descriptors);
                         expect.string(output).toMatchSnapshot();
                         expect.string(error).toEqual("");

                       | Compile =>
                         let descriptors =
                           id |> get_descriptors |> get_bsb_descriptors;
                         let (output, error) = descriptors |> continue_bsb;
                         expect.string(output).toMatchSnapshot();
                         expect.string(error).toEqual("");

                       | Error =>
                         let descriptors =
                           id |> get_descriptors |> get_bsb_descriptors;
                         let (_, error) = descriptors |> continue_bsb;
                         expect.string(error).toMatchSnapshot();
                       };
                     })
                   })
                 })
            });
          }
        })
   });
