module MyQuery: {type t;} = [%graphql
  {|
  {
    variousScalars  {
      string
    }
  }
|}
];

module MyQuery2: {
  type t_variousScalars;
  type t = {variousScalars: t_variousScalars};
} = [%graphql
  {|
  {
    variousScalars  {
      string
    }
  }
|};
  {records: true}
];
