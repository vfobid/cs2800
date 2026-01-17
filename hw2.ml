hw2.ml
(* If you would like, please write here how many hours you spent on the assignment: 
  XXXX
*)

(* Q1 *)
let rec concat (xs : string list) : string = failwith "TODO"

(* Q2 *)
let rec alternating (xs : int list) : bool = failwith "TODO"

(* Q3 *)

type int_tree = 
  | Leaf
  | Node of (int_tree * int * int_tree)

let rec gen_int_tree (depth : int) (bound : int) : int_tree = failwith "TODO"

let rec string_of_int_tree (t : int_tree) : string = failwith "TODO"

(* Q4 *)

let rec map_int_tree (f : int -> int) (t : int_tree) : int_tree = failwith "TODO"

(* Q5 *)
let rec int_tree_max (t : int_tree) : int option = failwith "TODO"

(* Q6 *)

let rec sorted (t : int_tree) : bool = failwith "TODO"
