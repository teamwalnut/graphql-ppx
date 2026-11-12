let reset = "\027[0m"
let bold = "\027[1m"
let cursive = "\027[3m"
let underline = "\027[4m"
let black = "\027[0;30m"
let red = "\027[0;31m"
let green = "\027[0;32m"
let yellow = "\027[0;33m"
let blue = "\027[0;34m"
let purple = "\027[0;35m"
let cyan = "\027[0;36m"
let white = "\027[0;37m"

module HighIntensity = struct
  let black = "\027[0;90m"
  let red = "\027[0;91m"
  let green = "\027[0;92m"
  let yellow = "\027[0;93m"
  let blue = "\027[0;94m"
  let purple = "\027[0;95m"
  let cyan = "\027[0;96m"
  let white = "\027[0;97m"
end

module Dimmed = struct
  let black = "\027[2;30m"
  let red = "\027[2;31m"
  let green = "\027[2;32m"
  let yellow = "\027[2;33m"
  let blue = "\027[2;34m"
  let purple = "\027[2;35m"
  let cyan = "\027[2;36m"
  let white = "\027[2;37m"
end
