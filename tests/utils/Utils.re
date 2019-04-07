let whitespaceAgnostic = str => {
  let re = [%bs.re {|/\s/g|}];
  Js.String.replaceByRe(re, "", str);
};
