module MyQuery = [%graphql
  {|
    query($arg: NonrecursiveInput!) {
      nonrecursiveInput(arg: $arg)
    }
  |}
];

module MyQuery2 = [%graphql
  {|
  query($arg: NonrecursiveInput!, $arg2: NonrecursiveInput!) {
    scalarsInput(arg: $arg)
    more: scalarsInput(arg: $arg2)
  }
|}
];
