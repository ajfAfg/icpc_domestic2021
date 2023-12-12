(*********************
 * List
 *********************)
let partition n (* 0 ≤ n *) xs =
  let xs, ys =
    xs |> List.mapi (fun i x -> (i, x)) |> List.partition (fun (i, _) -> i < n)
  in
  let remove_index = List.map (fun (_, x) -> x) in
  (remove_index xs, remove_index ys)

(*********************
 * Dataset
 *********************)
type dataset = {
  w : int; (* 2 ≤ w ≤ 100 *)
  h : int; (* 2 ≤ h ≤ 100 *)
  xyn_list : (int * int * int) list;
      (* List.length xyn_list = w + h - 1 ∧
         (∀ (x,y,n) ∈ xyn_list. 1 ≤ x ≤ w ∧ 1 ≤ y ≤ h ∧ −100 ≤ n ≤ 100) *)
}

let parse_input lines =
  let parse_xyn = function
    | [ x; y; n ] -> (x, y, n)
    | _ -> raise Util.InvalidInput
  in
  let rec parse_dataset = function
    | [] -> []
    | line :: lines -> (
        match line with
        | [ w; h ] ->
            let lines1, lines2 = partition (w + h - 1) lines in
            { w; h; xyn_list = List.map parse_xyn lines1 }
            :: parse_dataset lines2
        | _ -> raise Util.InvalidInput)
  in
  lines
  |> List.map @@ String.split_on_char ' '
  |> List.map @@ List.map int_of_string
  |> parse_dataset

(*********************
 * Graph
 *********************)
module Vertex = struct
  type t = int

  let compare = compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

module G = Graph.Persistent.Graph.Concrete (Vertex)

let has_path graph v u =
  let rec mem_path' graph v u seen =
    if v = u then true
    else if seen.(v) then false
    else (
      seen.(v) <- true;
      G.succ graph v
      |> List.map (fun v' -> mem_path' graph v' u seen)
      |> List.exists Fun.id)
  in
  mem_path' graph v u (Array.make (G.nb_vertex graph + 1) false)

let is_connected graph =
  G.fold_vertex
    (* NOTE: Assume 1 is always contained in `graph`. *)
      (fun vertex acc -> acc && has_path graph 1 vertex)
    graph true

(* NOTE:
   c.f. [グラフのサイクル検出 (閉路検出) by DFS](https://drken1215.hatenablog.com/entry/2023/05/20/200517) *)
let has_cycle graph =
  let rec is_cycle' graph now prev seen finished =
    seen.(now) <- true;
    let ans =
      G.fold_succ
        (fun next acc ->
          if next = prev || finished.(next) then acc
          else if seen.(next) && not finished.(next) then true
          else acc || is_cycle' graph next now seen finished)
        graph now false
    in
    finished.(now) <- true;
    ans
  in
  let make_history () = Array.make (G.nb_vertex graph + 1) false in
  (* NOTE: Assume 1 is always contained in `graph`. *)
  is_cycle' graph 1 ~-1 (make_history ()) (make_history ())

let is_tree graph = is_connected graph && (not @@ has_cycle graph)

(*********************
 * Main
 *********************)
let graph_of_dataset { w; h; xyn_list } =
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
  Util.read_input "0 0" |> parse_input
  |> List.iter (fun dataset ->
         let ans =
           if dataset |> graph_of_dataset |> is_tree then "YES" else "NO"
         in
         print_endline ans)
