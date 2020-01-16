open Graphql_ppx_base;
open Result_structure;
open Extract_type_definitions;

let generate_name = (name, path) => {
  List.fold_right((item, acc) => item ++ acc, [name, ...path], "");
};

let base_type = name => {
  Ast_helper.Typ.constr(
    {Location.txt: Longident.Lident(name), loc: Location.none},
    [],
  );
};

// generate the type definition, including nullables, arrays etc.
let generate_type =
  fun
  | Res_string(loc) => base_type("string");

// generate all the types:
let generate_types = (path, res) => {
  extract([], res)
  |> List.map(
       fun
       | Object({fields, name, path}) =>
         Ast_helper.Type.mk(
           ~kind=
             Ptype_record(
               fields
               |> List.map(
                    fun
                    | Fragment({module_name}) =>
                      Ast_helper.Type.field(
                        {
                          Location.txt: "fragment_" ++ module_name,
                          loc: Location.none,
                        },
                        Ast_helper.Typ.constr(
                          {
                            Location.txt: Longident.parse(module_name ++ ".t"),
                            loc: Location.none,
                          },
                          [],
                        ),
                      )

                    | Field({name, type_}) =>
                      Ast_helper.Type.field(
                        {Location.txt: name, loc: Location.none},
                        generate_type(type_),
                      ),
                  ),
             ),
           {loc: Location.none, txt: generate_name(name, path)},
         ),
     );
};
