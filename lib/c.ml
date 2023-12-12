(*********************
 * Syntax
 *********************)
type op = Plus | Minus
type exp = ILit of int | Op of op * exp list

let integer_of_char c =
  let i = int_of_char c - int_of_char '0' in
  if 0 <= i && i <= 9 then i else raise @@ Invalid_argument "integer_of_char"

let integer_of_char_opt c =
  try Some (integer_of_char c) with Invalid_argument _ -> None

let parse_exp dataset =
  let rec parse_root chars =
    let chars, exp1 = parse_non_root chars in
    let chars, op1 = parse_op chars in
    let chars, exp2 = parse_non_root chars in
    let chars, op2 = parse_op chars in
    let chars, exp3 = parse_non_root chars in
    if op1 = op2 then (chars, Op (op1, [ exp1; exp2; exp3 ]))
    else raise Util.InvalidInput
  and parse_non_root = function
    | [] -> raise Util.InvalidInput
    | c :: chars ->
        if Option.is_some @@ integer_of_char_opt c then
          (chars, ILit (integer_of_char c))
        else if c = '(' then
          let chars, exp1 = parse_non_root chars in
          let chars, op = parse_op chars in
          let chars, exp2 = parse_non_root chars in
          match chars with
          | [] -> raise Util.InvalidInput
          | c :: chars ->
              if c = ')' then (chars, Op (op, [ exp1; exp2 ]))
              else raise Util.InvalidInput
        else raise Util.InvalidInput
  and parse_op = function
    | [] -> raise Util.InvalidInput
    | c :: chars ->
        if c = '+' then (chars, Plus)
        else if c = '-' then (chars, Minus)
        else raise Util.InvalidInput
  in
  let rest, exp = dataset |> String.to_seq |> List.of_seq |> parse_root in
  if rest = [] then exp else raise Util.InvalidInput

(*********************
 * Id
 *********************)
module Id = struct
  type t = int

  let compare = compare

  let create =
    let id_ref = ref 0 in
    let create' () =
      let id = !id_ref in
      incr id_ref;
      id
    in
    create'
end

(*********************
 * Graph
 *********************)
module Vertex = struct
  type t = Id.t

  let compare = compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

module G = Graph.Persistent.Graph.Concrete (Vertex)

(*********************
 * Map
 *********************)
module IdMap = Map.Make (Id)

(*********************
 * Main
 *********************)
let rec graph_labels_of_exp' graph labels id = function
  | ILit i ->
      let graph = G.add_vertex graph id in
      let labels = IdMap.add id (Either.Left i) labels in
      (graph, labels)
  | Op (op, exps) ->
      let id_exp_list = List.map (fun exp -> (Id.create (), exp)) exps in
      let graph, labels =
        List.fold_left
          (fun (graph', labels') (id, exp) ->
            graph_labels_of_exp' graph' labels' id exp)
          (graph, labels) id_exp_list
      in
      let graph = G.add_vertex graph id in
      let graph =
        List.fold_left (fun graph' id' -> G.add_edge graph' id id') graph
        @@ List.map fst id_exp_list
      in
      let labels = IdMap.add id (Either.Right op) labels in
      (graph, labels)

let graph_labels_of_exp =
  graph_labels_of_exp' G.empty IdMap.empty @@ Id.create ()

let rec traverse memo parent_opt graph labels id =
  match parent_opt with
  | None -> traverse' memo parent_opt graph labels id
  | Some parent ->
      let key = (parent, id) in
      let v =
        match Hashtbl.find_opt memo key with
        | None -> traverse' memo parent_opt graph labels id
        | Some v -> v
      in
      Hashtbl.add memo key v;
      v

and traverse' memo parent_opt graph labels id =
  let children =
    match parent_opt with
    | None -> G.succ graph id
    | Some parent -> G.succ graph id |> List.filter (( <> ) parent)
  in
  match IdMap.find id labels with
  | Either.Left i -> (i, i)
  | Right Plus ->
      children
      |> List.map (fun child -> traverse memo (Some id) graph labels child)
      |> List.fold_left
           (fun (max1, min1) (max2, min2) -> (max1 + max2, min1 + min2))
           (0, 0)
  | Right Minus ->
      let max_min_list =
        children
        |> List.map (fun child -> traverse memo (Some id) graph labels child)
      in
      let max =
        max_min_list
        |> List.map (fun (max, min) ->
               max_min_list |> List.map snd |> List.fold_left ( - ) max
               |> ( + ) min)
        |> List.fold_left max Int.min_int
      in
      let min =
        max_min_list
        |> List.map (fun (max, min) ->
               max_min_list |> List.map fst |> List.fold_left ( - ) min
               |> ( + ) max)
        |> List.fold_left min Int.max_int
      in
      (max, min)

let solve () =
  Util.read_input "-1"
  |> List.iter (fun dataset ->
         let graph, labels = dataset |> parse_exp |> graph_labels_of_exp in
         let memo = Hashtbl.create @@ (2 * IdMap.cardinal labels) in
         G.fold_vertex
           (fun id max ->
             match IdMap.find id labels with
             | Either.Left _ -> max
             | Right _ ->
                 let max_xy x y = if x < y then y else x in
                 max_xy max @@ fst @@ traverse memo None graph labels id)
           graph Int.min_int
         |> Printf.printf "%d\n")
