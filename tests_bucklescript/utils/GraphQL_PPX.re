// will be inlined by bucklescript
let%private clone: Js.Dict.t('a) => Js.Dict.t('a) =
  a => Obj.magic(Js.Obj.assign(Obj.magic(Js.Obj.empty()), Obj.magic(a)));

// merging two json objects deeply
let rec deepMerge = (json1: Js.Json.t, json2: Js.Json.t) => {
  switch (
    (
      Obj.magic(json1) == Js.null,
      Js_array2.isArray(json1),
      Js.typeof(json1) == "object",
    ),
    (
      Obj.magic(json2) == Js.null,
      Js_array2.isArray(json2),
      Js.typeof(json2) == "object",
    ),
  ) {
  // merge two arrays
  | ((_, true, _), (_, true, _)) => (
      Obj.magic(
        Js.Array.mapi(
          (el1, idx) => {
            let el2 = Js.Array.unsafe_get(Obj.magic(json2), idx);
            // it cannot be undefined, because two arrays should always be the
            // same length in graphql responses
            Js.typeof(el2) == "object" ? deepMerge(el1, el2) : el2;
          },
          Obj.magic(json1),
        ),
      ): Js.Json.t
    )
  // two objects that are not null and not arrays
  | ((false, false, true), (false, false, true)) =>
    let obj1 = clone(Obj.magic(json1));
    let obj2 = Obj.magic(json2);
    Js.Dict.keys(obj2)
    |> Js.Array.forEach(key => {
         let existingVal: Js.Json.t = Js.Dict.unsafeGet(obj1, key);
         let newVal: Js.Json.t = Js.Dict.unsafeGet(obj2, key);
         Js.Dict.set(
           obj1,
           key,
           Js.typeof(existingVal) != "object"
             ? newVal : Obj.magic(deepMerge(existingVal, newVal)),
         );
       });
    Obj.magic(obj1);
  // object that becomes null
  | ((_, _, _), (_, _, _)) => json2
  };
};

module type Operation = {
  let query: string;

  module Raw: {
    type t;
    type t_variables;
  };
  type t;

  let parse: Raw.t => t;
  let serialize: t => Raw.t;
};

module type Query = {
  let query: string;

  module Raw: {
    type t;
    type t_variables;
  };
  type t;

  let parse: Raw.t => t;
  let serialize: t => Raw.t;
};

module type Mutation = {include Operation;};

module type Subscription = {include Operation;};

module type Fragment = {
  let query: string;
  module Raw: {type t;};
  type t;

  let parse: Raw.t => t;
  let serialize: t => Raw.t;
};
