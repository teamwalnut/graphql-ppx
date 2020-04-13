module RecordsQuery = [%graphql
  {|
  {
    lists {
      nullableOfNullable
      nullableOfNonNullable
      nonNullableOfNullable
      nonNullableOfNonNullable
    }
  }
|};
  {records: true}
];

module ObjectsQuery = [%graphql
  {|
  {
    lists {
      nullableOfNullable
      nullableOfNonNullable
      nonNullableOfNullable
      nonNullableOfNonNullable
    }
  }
|};
  {objects: true}
];
