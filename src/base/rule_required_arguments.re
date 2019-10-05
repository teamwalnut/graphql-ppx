module Visitor: Traversal_utils.VisitorSig = {
  open Traversal_utils;
  open Source_pos;
  open Graphql_ast;
  open Schema;

  include AbstractVisitor;

  let enter_field = (_self, ctx, def) => {
    let field_meta = 
      Context.parent_type(ctx)
      |> Option.flat_map(t => Schema.lookup_field(t, def.item.fd_name.item));

    let provided_args = 
      def.item.fd_arguments
      |> Option.map(span => span.item)
      |> Option.get_or_else([])
      |> List.map(arg => (arg |> fst).item); 

    let expected_args =
      field_meta 
      |> Option.map(fm => fm.fm_arguments)
      |> Option.get_or_else([])
      |> List.filter(arg => 
        switch(arg.am_arg_type) {
        | NonNull(_) => true
        | _ => false
      });

    expected_args
    |> List.iter(arg => {
        let provided = provided_args |> List.exists(arg_name => arg_name == arg.am_name);
        if(!provided) {
          let message =
            Printf.sprintf("Argument \"%s\" on field \"%s\" not provided", arg.am_name, def.item.fd_name.item);

          Context.push_error(ctx, def.span, message); 
        }
    });
  };

  type t = unit;
  let make_self = () => ();
};
