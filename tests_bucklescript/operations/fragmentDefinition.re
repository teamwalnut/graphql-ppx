module GraphQL_PPX = {
  // mock
  let deepMerge = (json1, _) => {
    json1;
  };
};

[%graphql
  {|
  fragment ListFragment on Lists {
    nullableOfNullable
    nullableOfNonNullable
  }

  # remove as soon as ListFragment is available via Fragments.ListFragment
  fragment Another on Lists {
    nullableOfNonNullable
  }

  fragment FragmentWithArgs on Lists @argumentDefinitions(arg1: {type: "String"}) {
    listWithArg(arg1: $arg1)
  }
|}
];

module InlineListFragment = {
  [%graphql
    {|
    fragment InlineListFragment on Lists {
      nullableOfNullable
      nullableOfNonNullable
    }
  |};
    {inline: true}
  ];
};

[%graphql
  {|
  query MyQuery($arg1: String) {
    l1: lists {
      ...ListFragment
    }

    l2: lists {
      ...ListFragment @ppxField(name: "frag1")
      ...ListFragment @ppxField(name: "frag2")
    }

    l3: lists {
      nullableOfNullable
      ...ListFragment @bsField(name: "frag1")
      ...ListFragment @bsField(name: "frag2")
    }

    l4: lists {
      nullableOfNullable
      ...InlineListFragment
    }

    l5: lists {
      ...FragmentWithArgs @arguments(arg1: $arg1)
    }
  }
|}
];

module MyQuery2 = [%graphql
  {|
  query {
    lists {
      ...ListFragment
    }
  }
  |}
];
