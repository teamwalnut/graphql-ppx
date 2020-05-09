/**
 * Missing field
 */
module MyQuery = [%graphql
  {|
    query {
        invalidObjectValues(
            nonRec: {
                nullableArray: [""]
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
