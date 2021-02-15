let rec merge_pairs =
        (
          pairs1: list((string, Yojson.Basic.t)),
          pairs2: list((string, Yojson.Basic.t)),
        ) => {
  let unique_keys =
    List.append(
      List.map(((k, _)) => k, pairs1),
      List.map(((k, _)) => k, pairs2),
    )
    |> List.fold_left(
         (unique_keys, key) =>
           if (List.exists(k => k == key, unique_keys)) {
             [key, ...unique_keys];
           } else {
             unique_keys;
           },
         [],
       );

  unique_keys
  |> List.map(key => {
       (
         key,
         switch (
           List.find_opt(((assoc_key, _)) => key == assoc_key, pairs1)
           |> Option.map(pair => snd(pair)),
           List.find_opt(((assoc_key, _)) => key == assoc_key, pairs2)
           |> Option.map(pair => snd(pair)),
         ) {
         | (Some(`Assoc(pairs1)), Some(`Assoc(pairs2))) =>
           `Assoc(merge_pairs(pairs1, pairs2))
         | (_, Some(`Assoc(pairs2))) => `Assoc(pairs2)
         | (_, Some(any)) => any
         | (Some(any), _) => any
         | (None, None) => assert(false)
         },
       )
     });
}

and deepMerge = (json1: Yojson.Basic.t, json2: Yojson.Basic.t) => {
  switch (json1, json2) {
  | (`Assoc(pairs1), `Assoc(pairs2)) =>
    `Assoc(merge_pairs(pairs1, pairs2))
  | (_, `Bool(value)) => `Bool(value)
  | (_, `Float(value)) => `Float(value)
  | (_, `String(value)) => `String(value)
  | (_, `List(values)) => `List(values)
  | (_, `Int(value)) => `Int(value)
  | (_, `Null) => `Null
  | (_, _) => assert(false)
  };
};
