/**
 * Normal
 */
module Normal = [%graphql
  {|
    mutation {
        mutationWithError {
            errors {
                message
                field
            }
        }
    }
|}
];
/**
 * By config
 */
module ByConfig = [%graphql
  {|
    mutation {
        mutationWithError {
            errors {
                message
                field
            }
        }
    }
|};
  {future_added_value: false}
];
/**
 * By directive
 */
module ByDirective = [%graphql
  {|
    mutation {
        mutationWithError {
            errors {
                message
                field @ppxOmitFutureValue
            }
        }
    }
|}
];
