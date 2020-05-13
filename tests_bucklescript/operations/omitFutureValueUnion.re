/**
 * Normal
 */
module Normal = [%graphql
  {|
    {
    dogOrHuman {
      ...on Dog {
        name
        barkVolume
      }

      ...on Human {
        name
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
    {
    dogOrHuman {
      ...on Dog {
        name
        barkVolume
      }

      ...on Human {
        name
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
    {
    dogOrHuman @ppxOmitFutureValue {
      ...on Dog {
        name
        barkVolume
      }

      ...on Human {
        name
      }
    }
  }
|}
];
