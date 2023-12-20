exception Invalid_input

let rec read_input terminator =
  let str = read_line () in
  if str = terminator then [] else str :: read_input terminator
