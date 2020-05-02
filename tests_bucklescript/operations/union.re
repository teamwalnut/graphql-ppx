// module MyQuery = [%graphql
//   {|
//   {
//     dogOrHuman {
//       ...on Dog {
//         name
//         barkVolume
//       }
//       ...on Human {
//         name
//       }
//     }
//   }
// |}
// ];
// module MyQueryNoError = [%graphql
//   {|
//   {
//     dogOrHuman {
//       # this is valid graphql and should pass
//       __typename
//       ...on Dog {
//         name
//         barkVolume
//       }
//       ...on Human {
//         name
//       }
//     }
//   }
// |}
// ];
