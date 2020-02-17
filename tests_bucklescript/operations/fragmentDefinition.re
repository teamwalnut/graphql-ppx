// TODO: we're flattening module when there is only one fragment. This seems misleading
module Fragments = [%graphql
  {|
  fragment ListFragment on Lists {
    nullableOfNullable
    nullableOfNonNullable
  }

  # remove as soon as ListFragment is available via Fragments.ListFragment
  fragment Another on Lists {
    nullableOfNonNullable
  }
|}
];

module MyQuery = [%graphql
  {|
  query {
    l1: lists {
      ...Fragments.ListFragment
    }

    l2: lists {
      ...Fragments.ListFragment @bsField(name: "frag1")
      ...Fragments.ListFragment @bsField(name: "frag2")
    }
  }
|}
];
