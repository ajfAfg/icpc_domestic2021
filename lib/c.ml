(*********************
 * Syntax
 *********************)
module Syntax = struct
  type op = Plus | Minus
  type exp = ILit of int | Op of op * exp list
end

(*********************
 * Parser
 *********************)
module Parser : sig
  val parse_exp : string -> Syntax.exp
end = struct
  open Syntax

  let integer_of_char c =
    let i = int_of_char c - int_of_char '0' in
    if 0 <= i && i <= 9 then i else raise @@ Invalid_argument "integer_of_char"

  let integer_of_char_opt c =
    try Some (integer_of_char c) with Invalid_argument _ -> None

  let rec parse_root chars =
    let chars, exp1 = parse_non_root chars in
    let chars, op1 = parse_op chars in
    let chars, exp2 = parse_non_root chars in
    let chars, op2 = parse_op chars in
    let chars, exp3 = parse_non_root chars in
    if op1 = op2 then (chars, Op (op1, [ exp1; exp2; exp3 ]))
    else raise Util.Invalid_input

  and parse_non_root = function
    | [] -> raise Util.Invalid_input
    | c :: chars ->
        if Option.is_some @@ integer_of_char_opt c then
          (chars, ILit (integer_of_char c))
        else if c = '(' then
          let chars, exp1 = parse_non_root chars in
          let chars, op = parse_op chars in
          let chars, exp2 = parse_non_root chars in
          match chars with
          | [] -> raise Util.Invalid_input
          | c :: chars ->
              if c = ')' then (chars, Op (op, [ exp1; exp2 ]))
              else raise Util.Invalid_input
        else raise Util.Invalid_input

  and parse_op = function
    | [] -> raise Util.Invalid_input
    | c :: chars ->
        if c = '+' then (chars, Plus)
        else if c = '-' then (chars, Minus)
        else raise Util.Invalid_input

  let parse_exp dataset =
    let rest, exp = dataset |> String.to_seq |> List.of_seq |> parse_root in
    if rest = [] then exp else raise Util.Invalid_input
end

(*********************
 * Id
 *********************)
module Id : sig
  type t

  val compare : 'a -> 'a -> int
  val create : unit -> t
end = struct
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
module G = Graph.Persistent.Graph.Concrete (struct
  type t = Id.t * (int, Syntax.op) Either.t

  let compare = compare
  let hash = Hashtbl.hash
  let equal = ( = )
end)

(*********************
 * Main
 *********************)
type tree = T of (Id.t * (int, Syntax.op) Either.t) * tree list

let rec tree_of_exp = function
  | Syntax.ILit i -> T ((Id.create (), Either.Left i), [])
  | Op (op, exps) ->
      T ((Id.create (), Either.Right op), List.map tree_of_exp exps)

let graph_of_tree =
  let rec graph_of_tree' graph = function
    | T (vertex, trees) ->
        let graph = G.add_vertex graph vertex in
        let graph =
          List.fold_left
            (fun graph' tree -> graph_of_tree' graph' tree)
            graph trees
        in
        List.fold_left
          (fun graph' (T (vertex', _)) -> G.add_edge graph' vertex vertex')
          graph trees
  in
  graph_of_tree' G.empty

let rec traverse memo parent_opt graph vertex =
  match parent_opt with
  | None -> traverse' memo parent_opt graph vertex
  | Some parent ->
      let key = (parent, vertex) in
      let v =
        match Hashtbl.find_opt memo key with
        | None -> traverse' memo parent_opt graph vertex
        | Some v -> v
      in
      Hashtbl.add memo key v;
      v

and traverse' memo parent_opt graph vertex =
  let children =
    match parent_opt with
    | None -> G.succ graph vertex
    | Some parent -> G.succ graph vertex |> List.filter (( <> ) parent)
  in
  match vertex with
  | _, Either.Left i -> (i, i)
  | _, Right Syntax.Plus ->
      children
      |> List.map (fun child -> traverse memo (Some vertex) graph child)
      |> List.fold_left
           (fun (max1, min1) (max2, min2) -> (max1 + max2, min1 + min2))
           (0, 0)
  | _, Right Minus ->
      let max_min_list =
        children
        |> List.map (fun child -> traverse memo (Some vertex) graph child)
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
         let graph =
           dataset |> Parser.parse_exp |> tree_of_exp |> graph_of_tree
         in
         let memo = Hashtbl.create @@ (2 * G.nb_vertex graph) in
         G.fold_vertex
           (fun vertex max' ->
             match vertex with
             | _, Either.Left _ -> max'
             | _, Right _ -> max max' @@ fst @@ traverse memo None graph vertex)
           graph Int.min_int
         |> Printf.printf "%d\n")
