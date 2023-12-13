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
    | _ -> raise Util.InvalidInput
end

(*********************
 * Main
 *********************)
(* NOTE: DP by memonization causes stack overflow. *)
let calc_ans ({ n; bs } : Dataset.t) =
  let bs =
    let dummy = 0 in
    Array.of_list (dummy :: bs)
  in
  let size = Array.fold_left ( + ) 0 bs in
  let dp =
    Array.init (n + 1) @@ fun _ ->
    Array.init (size + 1) @@ fun x ->
    Array.init (size + 1) @@ fun y -> x = y && y = 0
  in
  for k = 1 to n do
    for x = 0 to size do
      for y = 0 to size do
        dp.(k).(x).(y) <-
          dp.(k - 1).(x).(y)
          || (x >= bs.(k) && dp.(k - 1).(x - bs.(k)).(y))
          || (y >= bs.(k) && dp.(k - 1).(x).(y - bs.(k)))
      done
    done
  done;
  let ans = ref 0 in
  for x = 0 to size do
    for y = 0 to size do
      if dp.(n).(x).(y) then ans := max !ans @@ min x y |> min (size - x - y)
      else ()
    done
  done;
  !ans

let solve () =
  Util.read_input "0" |> Dataset.parse_input
  |> List.iter (fun dataset -> Printf.printf "%d\n" @@ calc_ans dataset)
