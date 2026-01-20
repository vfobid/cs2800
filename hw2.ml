hw2.ml
(* If you would like, please write here how many hours you spent on the assignment: 
  XXXX
*)

(* Q1 *)
let rec concat (xs : string list) : string =
  match xs with
  | [] -> ""
  | h :: t -> h ^ (concat t)


(* Q2 *)
let rec alternating (xs : int list) : bool =
  match xs with
  | [] -> true
  | [x] -> (x mod 2 = 0)
  | x1 :: x2 :: t ->
      if (x1 mod 2 = 0 && x2 mod 2 <> 0) || (x1 mod 2 <> 0 && x2 mod 2 = 0)
      then alternating (x2 :: t)
      else false

(* Q3 *)

type int_tree = 
  | Leaf
  | Node of (int_tree * int * int_tree)

let rec gen_int_tree (depth : int) (bound : int) : int_tree = 
  match depth with
  | d when d <= 0 -> Leaf
  | _ -> 
      let left_subtree = gen_int_tree (depth - 1) bound in
      let right_subtree = gen_int_tree (depth - 1) bound in
      let value = Random.int bound in
      Node (left_subtree, value, right_subtree)

let rec string_of_int_tree (t : int_tree) : string = 
  match t with
  | Leaf -> "Leaf"
  | Node (left, v, right) -> 
      "Node(" ^ (string_of_int_tree left) ^ ", " ^ string_of_int v ^ ", " ^ (string_of_int_tree right) ^ ")"

(* Q4 *)

let rec map_int_tree (f : int -> int) (t : int_tree) : int_tree = 
  match t with
  | Leaf -> Leaf
  | Node (left, v, right) -> 
      let new_left = map_int_tree f left in
      let new_right = map_int_tree f right in
      Node (new_left, f v, new_right)

(* Q5 *)
let rec int_tree_max (t : int_tree) : int option =
  match t with
  |Leaf -> None
  |Node (left, v, right) ->
    let left_max = int_tree_max left in
    let right_max = int_tree_max right in
    let current_max = Some v in
    let max_option a b =
      match (a, b) with
      | (None, None) -> None
      | (Some x, None) -> Some x
      | (None, Some y) -> Some y
      | (Some x, Some y) -> Some (max x y)
    in
    max_option current_max (max_option left_max right_max)

(* Q6 *)
let rec int_tree_min (t : int_tree) : int option =
  match t with
  |Leaf -> None
  |Node (left, v, right) ->
    let left_min = int_tree_min left in
    let right_min = int_tree_min right in
    let current_min = Some v in
    let min_option a b =
      match (a, b) with
      | (None, None) -> None
      | (Some x, None) -> Some x
      | (None, Some y) -> Some y
      | (Some x, Some y) -> Some (min x y)
    in
    min_option current_min (min_option left_min right_min)

let rec sorted (t : int_tree) : bool =
  match t with
  | Leaf -> true
  | Node (left, v, right) ->
    if (match int_tree_max left with
        | None -> true
        | Some max_left -> max_left <= v) &&
       (match int_tree_min right with
        | None -> true
        | Some min_right -> min_right >= v)
    then sorted left && sorted right
    else false
