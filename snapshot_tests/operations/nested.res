type record = {
  f1: string,
  f2: string,
}

module MyQuery = %graphql(`
  {
    first: nestedObject {
      inner {
        inner {
          field
        }
      }
    }

    second: nestedObject {
      inner {
        inner @bsRecord {
          f1: field
          f2: field
        }
      }
    }

    # reserved word
    let: nestedObject {
      inner {
        inner {
          field
        }
      }
    }
  }
`)

module Test: {
  module MyQueryWithSig: %graphql(`
    {
      nestedObject {
        inner {
          inner {
            field
          }
        }
      }
    }
  `)
} = {
  module MyQueryWithSig = %graphql(`
    {
      nestedObject {
        inner {
          inner {
            field
          }
        }
      }
    }
  `)
}

// not sure why we would need this...
// might remove this functionality later
module MyQueryWithSigDirect: %graphql(`
  {
    nestedObject {
      inner {
        inner {
          field
        }
      }
    }
  }
  `) = %graphql(`
  {
    nestedObject {
      inner {
        inner {
          field
        }
      }
    }
  }
`)
