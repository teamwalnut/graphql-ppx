module type GraphQLQuery = {
  module Raw: {
    type t;
    type t_variables;
  };
  type t;
  type t_variables;

  let query: string;
  let parse: Raw.t => t;
  let serialize: t => Raw.t;
};

module Parent = {
  module ExtendQuery = (M: GraphQLQuery) => {
    let use = () => ();
  };
};

[%graphql
  {|
    query Bla {
      lists {
        nullableOfNullable
        nullableOfNonNullable
        nonNullableOfNullable
        nonNullableOfNonNullable
      }
    }
  |};
  {extend: "Parent.ExtendQuery"}
];

[%graphql
  {|
    query Bla2 {
      lists {
        nullableOfNullable
        nullableOfNonNullable
        nonNullableOfNullable
        nonNullableOfNonNullable
      }
    }
  |};
  {extend: "Parent.ExtendQuery", inline: true}
];
