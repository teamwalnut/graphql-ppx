%graphql(`

    fragment test on Post @argumentDefinitions(name: {type: "String!"}){
 reposts(arg:{name:$name}) {
     id
 }
}
`)
