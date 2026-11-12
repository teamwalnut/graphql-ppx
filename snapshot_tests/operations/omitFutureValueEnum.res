/**
 * Normal
 */
module Normal = %graphql(`
    mutation {
        mutationWithError {
            errors {
                message
                field
            }
        }
    }
`)
/**
 * By config
 */
module ByConfig = %graphql(
  `
    mutation {
        mutationWithError {
            errors {
                message
                field
            }
        }
    }
`
  {futureAddedValue: false}
)
/**
 * By directive
 */
module ByDirective = %graphql(`
    mutation {
        mutationWithError {
            errors {
                message
                field @ppxOmitFutureValue
            }
        }
    }
`)
