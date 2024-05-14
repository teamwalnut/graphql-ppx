open Graphql_ast
open Source_pos
open Schema

exception Cant_find_fragment_type of string spanning

type t =
  | String of string
  | FragmentNameRef of { contents : string spanning; allow_string : bool }
  | FragmentQueryRef of { contents : string spanning; allow_string : bool }

let rec type_ref_name = function
  | Named n -> n
  | NonNull t -> type_ref_name t
  | List t -> type_ref_name t

let rec print_input_value iv =
  match iv with
  | Iv_null -> "null"
  | Iv_int i -> string_of_int i
  | Iv_float f -> Printf.sprintf "%.16g" f
  | Iv_string s -> "\"" ^ String.escaped s ^ "\""
  | Iv_boolean b -> string_of_bool b
  | Iv_enum s -> s
  | Iv_variable v -> "$" ^ v
  | Iv_list list ->
    "["
    ^ (list
      |> List.map (fun { item; _ } -> print_input_value item)
      |> String.concat ", ")
    ^ "]"
  | Iv_object obj ->
    "{"
    ^ (obj
      |> List.map (fun ({ item = key; _ }, { item = value; _ }) ->
           key ^ ": " ^ print_input_value value)
      |> String.concat ", ")
    ^ "}"

let print_argument ({ item = key; _ }, { item = value; _ }) =
  key ^ ": " ^ print_input_value value

let print_arguments args =
  match args with
  | [] -> ""
  | args -> "(" ^ (List.map print_argument args |> String.concat ", ") ^ ")"

let print_directive d =
  "@" ^ d.d_name.item
  ^
  match d.d_arguments with
  | Some { item; _ } -> print_arguments item
  | None -> ""

let is_internal_directive d =
  match d.item.d_name.item with
  | "bsOmitFutureValue" | "ppxOmitFutureValue" | "bsVariant" | "ppxVariant"
  | "bsRecord" | "ppxRecord" | "bsObject" | "ppxObject" | "bsDecoder"
  | "ppxDecoder" | "bsCustom" | "ppxCustom" | "ppxCustomOpt" | "bsAs" | "ppxAs"
  | "argumentDefinitions" | "arguments" | "bsField" | "ppxField" | "ppxConfig"
    ->
    true
  | _ -> false

let print_directives ds =
  " "
  ^ (ds
    |> List.filter (fun d -> not @@ is_internal_directive d)
    |> List.map (fun d -> print_directive d.item)
    |> String.concat " ")
  ^ " "

let print_fragment_spread s =
  [|
    String "...";
    FragmentNameRef
      {
        contents = s.fs_name;
        allow_string =
          s.fs_directives
          |> List.exists (fun x ->
               if x.item.d_name.item = "ppxAllowStringReturnType" then true
               else false);
      };
    String (" " ^ print_directives s.fs_directives);
  |]

let rec print_type ty =
  match ty with
  | Tr_named n -> n.item
  | Tr_list l -> "[" ^ print_type l.item ^ "]"
  | Tr_non_null_list l -> "[" ^ print_type l.item ^ "]!"
  | Tr_non_null_named n -> n.item ^ "!"

let rec print_selection_set schema ty selection_set =
  match selection_set with
  | [] -> [||]
  | selection ->
    let add_typename =
      match ty with Interface _ | Union _ -> true | _ -> false
    in
    let maybe_typename =
      if add_typename then Some (String "__typename\n") else None
    in
    let selection =
      selection
      |> List.map (fun s ->
           Array.append (print_selection schema ty s) [| String "\n" |])
      |> Array.concat
    in
    Array.concat
      [
        (match maybe_typename with
        | Some typename -> [| String "{\n"; typename |]
        | None -> [| String "{\n" |]);
        selection;
        [| String "}\n" |];
      ]

and print_selection schema ty selection =
  match selection with
  | Field { item; _ } -> print_field schema ty item
  | FragmentSpread { item; _ } -> print_fragment_spread item
  | InlineFragment { item; _ } -> print_inline_fragment schema ty item

and print_field schema ty f =
  let ty_fields =
    match ty with
    | Interface { im_fields; _ } -> im_fields
    | Object { om_fields; _ } -> om_fields
    | _ -> []
  in
  let field_type_name =
    (List.find (fun fm -> fm.fm_name = f.fd_name.item) ty_fields).fm_field_type
    |> type_ref_name
  in
  let field_ty =
    field_type_name |> lookup_type schema
    |> Option.unsafe_unwrap
         ~reason:("Cannot find field type: " ^ field_type_name)
  in
  Array.append
    ([
       (match f.fd_alias with
       | Some { item; _ } -> Some (String (item ^ ": "))
       | None -> None);
       Some (String f.fd_name.item);
       (match f.fd_arguments with
       | Some { item; _ } -> Some (String (print_arguments item))
       | None -> None);
       Some (String (print_directives f.fd_directives));
     ]
    |> List.fold_left
         (fun acc -> function Some r -> r :: acc | None -> acc)
         []
    |> List.rev |> Array.of_list)
    (match f.fd_selection_set with
    | Some { item; _ } -> print_selection_set schema field_ty item
    | None -> [||])

and print_inline_fragment schema ty f =
  let inner_ty =
    match f.if_type_condition with
    | Some { item; _ } ->
      lookup_type schema item
      |> Option.unsafe_unwrap
           ~reason:("Can't find inline fragment type: " ^ item)
    | None -> ty
  in
  Array.append
    [|
      String "...";
      String
        (match f.if_type_condition with
        | Some { item; _ } -> "on " ^ item ^ " "
        | None -> " ");
      String (print_directives f.if_directives);
    |]
    (print_selection_set schema inner_ty f.if_selection_set.item)

let print_variable_definition (name, def) =
  Printf.sprintf "$%s: %s%s" name.item
    (print_type def.vd_type.item)
    (match def.vd_default_value with
    | Some { item; _ } -> " = " ^ print_input_value item
    | None -> "")

let print_variable_definitions defs =
  "(" ^ (List.map print_variable_definition defs |> String.concat ", ") ^ ")"

let print_operation schema op =
  let ty_name =
    match op.o_type with
    | Query -> schema.meta.sm_query_type
    | Mutation ->
      Option.unsafe_unwrap ~reason:"Can't find mutation type"
        schema.meta.sm_mutation_type
    | Subscription ->
      Option.unsafe_unwrap ~reason:"Can't find subscription type"
        schema.meta.sm_subscription_type
  in
  Array.append
    ([
       Some
         (String
            (match op.o_type with
            | Query -> "query "
            | Mutation -> "mutation "
            | Subscription -> "subscription "));
       (match op.o_name with
       | Some { item; _ } -> Some (String item)
       | None -> None);
       (match op.o_variable_definitions with
       | Some { item; _ } -> Some (String (print_variable_definitions item))
       | None -> None);
       Some (String (print_directives op.o_directives));
     ]
    |> List.fold_left
         (fun acc -> function Some r -> r :: acc | None -> acc)
         []
    |> List.rev |> Array.of_list)
    (print_selection_set schema
       (lookup_type schema ty_name
       |> Option.unsafe_unwrap ~reason:("Can't find type: " ^ ty_name))
       op.o_selection_set.item)

let print_fragment schema f =
  Array.append
    [|
      String
        ("fragment " ^ f.fg_name.item ^ " on " ^ f.fg_type_condition.item ^ " ");
      String (print_directives f.fg_directives);
    |]
    (print_selection_set schema
       (match lookup_type schema f.fg_type_condition.item with
       | Some fragment_type -> fragment_type
       | None -> raise (Cant_find_fragment_type f.fg_type_condition))
       f.fg_selection_set.item)

let print_definition schema def =
  match def with
  | Operation { item = operation; _ } -> print_operation schema operation
  | Fragment { item = fragment; _ } -> print_fragment schema fragment

let find_fragment_refs parts =
  parts
  |> Array.fold_left
       (fun acc -> function
         | String _ -> acc
         | FragmentNameRef r ->
           Hashtbl.replace acc r.contents.item (r.contents, r.allow_string);
           acc
         | FragmentQueryRef _ -> acc)
       (Hashtbl.create ~random:false 10)
  |> Hashtbl.to_seq_values |> List.of_seq

let compress_parts (parts : t array) =
  parts |> Array.to_list
  |> List.fold_left
       (fun acc curr ->
         match (acc, curr) with
         | String s1 :: rest, String s2 -> String (s1 ^ s2) :: rest
         | acc, curr -> curr :: acc)
       []
  |> List.rev |> Array.of_list

let print_document schema defs =
  let parts = defs |> List.map (print_definition schema) |> Array.concat in
  let fragment_refs = find_fragment_refs parts in
  Array.concat
    [
      parts;
      fragment_refs |> Array.of_list
      |> Array.map (fun ref ->
           FragmentQueryRef { contents = fst ref; allow_string = snd ref });
    ]
  |> compress_parts
