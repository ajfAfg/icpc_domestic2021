(* NOTE: Assume a ≥ b *)
let rec gcd a b = if b = 0 then a else gcd b (Int.rem a b)

let solve () =
  Util.read_input "0 0 0 0"
  |> List.iter (fun line ->
         let ans =
           match line |> String.split_on_char ' ' |> List.map int_of_string with
           | x :: xs when List.length xs = 3 ->
               List.fold_left
                 (fun acc y -> if acc <= y then gcd y acc else gcd acc y)
                 x xs
           | _ -> raise Util.Invalid_input
         in
         Printf.printf "%d\n" ans)
