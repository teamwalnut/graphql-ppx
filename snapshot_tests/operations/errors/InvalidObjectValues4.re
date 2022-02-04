/**
 * Invalid list type
 */
module MyQuery = [%graphql
  {|
    query {
        invalidObjectValues(
            nonRec: {
                nonNullableField: "!"
                nullableArray: [2]
                field: ""
                enum: FIRST
            }
            rec: {
                otherField: ""
                inner: {
                    otherField: ""
                }
                enum: FIRST
            }
        )
    }
|}
];
