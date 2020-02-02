module MyQuery = [%graphql
  {|
  query ($arg: SampleField!) {
    enumInput(arg: $arg)
  }
|}
];
