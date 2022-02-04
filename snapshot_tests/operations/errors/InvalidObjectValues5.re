/**
 * Custom scalars should not throw
 */
module MyQuery = [%graphql
  {|
    query {
        invalidObjectValues(
            nonRec: {
                nonNullableField: "!"
                nullableArray: [""]
                field: ""
                enum: FIRST
                custom: { some: "value" }
            }
            rec: {
                otherField: ""
                inner: {
                    otherField: ""
                }
                enum: []
            }
        )
    }
|}
];
