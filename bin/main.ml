open Icpc_domestic2021

let () =
  (* NOTE: `Sys.argv` must contain a command name at the first. *)
  match Sys.argv.(1) with "a" -> A.solve () | "b" -> B.solve () | _ -> ()
