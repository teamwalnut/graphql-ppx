module MyQuery = [%graphql
  {|
  query ($arg: RecursiveInput!) {
    recursiveInput(arg: $arg)
  }
|}
];
