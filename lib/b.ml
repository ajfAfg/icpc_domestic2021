(*********************
 * List
 *********************)
module List = struct
  include List

  let partition n (* 0 ≤ n *) xs =
    let xs, ys =
      xs
      |> List.mapi (fun i x -> (i, x))
      |> List.partition (fun (i, _) -> i < n)
    in
    let remove_index = List.map (fun (_, x) -> x) in
    (remove_index xs, remove_index ys)
end

(*********************
 * Dataset
 *********************)
module Dataset = struct
  type t = {
    w : int; (* 2 ≤ w ≤ 100 *)
    h : int; (* 2 ≤ h ≤ 100 *)
    xyn_list : (int * int * int) list;
        (* List.length xyn_list = w + h - 1 ∧
           (∀ (x,y,n) ∈ xyn_list. 1 ≤ x ≤ w ∧ 1 ≤ y ≤ h ∧ −100 ≤ n ≤ 100) *)
  }

  let parse_input lines =
    let parse_xyn = function
      | [ x; y; n ] -> (x, y, n)
      | _ -> raise Util.Invalid_input
    in
    let rec parse_dataset = function
      | [] -> []
      | line :: lines -> (
          match line with
          | [ w; h ] ->
              let lines1, lines2 = List.partition (w + h - 1) lines in
              { w; h; xyn_list = List.map parse_xyn lines1 }
              :: parse_dataset lines2
          | _ -> raise Util.Invalid_input)
    in
    lines
    |> List.map @@ String.split_on_char ' '
    |> List.map @@ List.map int_of_string
    |> parse_dataset
end

(*********************
 * Graph
 *********************)
module G = struct
  module G = Graph.Persistent.Graph.Concrete (struct
    type t = int

    let compare = compare
    let hash = Hashtbl.hash
    let equal = ( = )
  end)

  include G
  include Graph.Traverse.Dfs (G)
  include Graph.Components.Make (G)

  let is_connected graph = scc_list graph |> List.length |> ( = ) 1
  let is_tree graph = is_connected graph && (not @@ has_cycle graph)
end

(*********************
 * Main
 *********************)
let graph_of_dataset ({ w; h; xyn_list } : Dataset.t) =
  (* NOTE:
     To distinguish between x and y,
     each range is changed to 1 ≤ x ≤ w, 1 + w ≤ y ≤ h + w. *)
  let graph =
    [ List.init w (( + ) 1); List.init h (( + ) w) ]
    |> List.flatten
    |> List.fold_left (fun graph x -> G.add_vertex graph x) G.empty
  in
  List.fold_left
    (fun graph (x, y, _n) -> G.add_edge graph x (y + w))
    graph xyn_list

let solve () =
  Util.read_input "0 0" |> Dataset.parse_input
  |> List.iter (fun dataset ->
         let ans =
           if dataset |> graph_of_dataset |> G.is_tree then "YES" else "NO"
         in
         print_endline ans)
