// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';


var Raw = { };

var query = "subscription   {\nsimpleSubscription  {\n__typename\n...on Dog   {\nname  \n}\n\n...on Human   {\nname  \n}\n\n}\n\n}\n";

function parse(value) {
  var value$1 = value.simpleSubscription;
  var typename = value$1["__typename"];
  var tmp;
  switch (typename) {
    case "Dog" :
        tmp = /* `Dog */[
          3406428,
          {
            name: value$1.name
          }
        ];
        break;
    case "Human" :
        tmp = /* `Human */[
          -1031617139,
          {
            name: value$1.name
          }
        ];
        break;
    default:
      tmp = /* `FutureAddedValue */[
        -31101740,
        value$1
      ];
  }
  return {
          simpleSubscription: tmp
        };
}

function serialize(value) {
  var value$1 = value.simpleSubscription;
  var variant = value$1[0];
  var tmp;
  if (variant !== -31101740) {
    if (variant >= 3406428) {
      var value$2 = value$1[1].name;
      tmp = {
        __typename: "Dog",
        name: value$2
      };
    } else {
      var value$3 = value$1[1].name;
      tmp = {
        __typename: "Human",
        name: value$3
      };
    }
  } else {
    tmp = value$1[1];
  }
  return {
          simpleSubscription: tmp
        };
}

var definition = /* tuple */[
  parse,
  query,
  serialize
];

var MyQuery = {
  Raw: Raw,
  query: query,
  parse: parse,
  serialize: serialize,
  definition: definition
};

exports.MyQuery = MyQuery;
/* No side effect */