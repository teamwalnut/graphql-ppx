---
title: Custom Fields
---

If you've got a custom scalar, or just want to convert e.g. an integer to a
string to properly fit a record type (see above), you can use the `@ppxCustom`
directive to insert a custom function in the decoder:

```reason
module StringHeight = {
  let parse = (height) => string_of_float(height);
  let serialize = (height) => float_of_string(height);
  type t = string;
}


module HeroQuery = [%graphql {|
{
  hero {
    name
    height @ppxCustom(module: "StringHeight")
    mass
  }
}
|}];
```

In this example, `height` will be converted from a float to a string in the
result. Using the `module` argument, you can specify any decoder module with the
functions `parse`, `serialize` and type `t`.

If you have custom scalars that you'd always would like to conver to a specific
type. You can do that in the `custom-fields` part of the `bsconfig.json`
configuration.
