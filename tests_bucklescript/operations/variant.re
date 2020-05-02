// module MyQuery = [%graphql
//   {|
//   mutation {
//     mutationForVariant @bsVariant {
//       baseType
//       baseTypeList
//       dog {
//         name
//         barkVolume
//       }
//       human {
//         name
//       }
//       dogOrHuman {
//         ...on Dog {
//           name
//           barkVolume
//         }
//         ...on Human {
//           name
//         }
//       }
//     }
//   }
// |}
// ];
