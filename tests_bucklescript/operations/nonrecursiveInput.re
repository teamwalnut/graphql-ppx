module MyQuery = [%graphql
  {|
    query ($arg: NonrecursiveInput!) {
      nonrecursiveInput(arg: $arg)
    }
  |}
];
