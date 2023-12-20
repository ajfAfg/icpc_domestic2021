(*********************
 * Dataset
 *********************)
module Dataset = struct
  type t = {
    n : int; (* 3 ≤ n ≤ 50 *)
    bs : int list; (* List.length bs ≤ n ∧ (∀ b ∈ bs. 1 ≤ b ≤ 50) *)
  }

  let rec parse_input = function
    | [] -> []
    | n_str :: line :: lines ->
        {
          n = int_of_string n_str;
          bs = line |> String.split_on_char ' ' |> List.map @@ int_of_string;
        }
        :: parse_input lines
    | _ -> raise Util.Invalid_input
end

(*********************
 * Main
 *********************)
let calc_ans ({ n = _; bs } : Dataset.t) =
  let bs_sum = List.fold_left ( + ) 0 bs in

  let rec dp memo = function
    | [] -> memo
    | b :: bs ->
        let memo' =
          Array.map
            (fun bitset ->
              Big_int.or_big_int bitset @@ Big_int.shift_left_big_int bitset b)
            memo
        in
        for i = b to bs_sum do
          memo'.(i) <- Big_int.or_big_int memo'.(i) memo.(i - b)
        done;
        dp memo' bs
  in
  let memo =
    dp
      (Array.append [| Big_int.unit_big_int |]
      @@ Array.init bs_sum (fun _ -> Big_int.zero_big_int))
      bs
  in

  let ans = ref 0 in
  for x = 0 to bs_sum do
    for y = 0 to bs_sum do
      if
        Big_int.shift_right_big_int memo.(x) y
        |> Big_int.and_big_int Big_int.unit_big_int
        |> Big_int.eq_big_int Big_int.unit_big_int
      then ans := x |> min y |> min (bs_sum - x - y) |> max !ans
    done
  done;
  !ans

let solve () =
  Util.read_input "0" |> Dataset.parse_input
  |> List.iter (fun dataset -> Printf.printf "%d\n" @@ calc_ans dataset)
