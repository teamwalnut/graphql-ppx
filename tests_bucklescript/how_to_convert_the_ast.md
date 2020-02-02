# How to convert the AST

install the latest ReasonML version and build it, because the currently published version of reason-cli is not compatible with ocaml 4.6.0.

```
cat test4.re | ../../reason/_esy/default/build/default/src/refmt/refmt_impl.exe --parse re --print binary | ../_build/default/src/bucklescript_bin/bin.exe -records /dev/stdin /dev/stdout | ../../reason/_esy/default/build/default/src/refmt/refmt_impl.exe --parse binary --print re --interface false
```
