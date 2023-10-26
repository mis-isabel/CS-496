(* Quiz 1 - 1 Feb 2023 

   Student name 1: Isabel Sutedjo
   Student name 2: Sean Payba
   Pledge: I pledge my honor that I have abided by the Stevens Honor System.
*)

(* Sample Directed Graph *)

let ex = [(1, 2); (2, 3); (3, 1); (3, 4)]


(*
  1 <------ 3
  |      //||
  |     /   | 
  |    /    | 
 \/  /     \/
  2        4
*)
       
(* 
Eg. outgoing ex 3 => [1,4] 
*)
let rec outgoing_nodes g n =
  match g with 
  | [] -> []
  | (fst, snd)::t when (fst = n) -> snd :: outgoing_nodes t n
  | (fst, snd)::t -> outgoing_nodes t n

(* 
   The list of nodes of the tree without duplicates. The order of the   nodes in the list is irrelevant.
   eg. nodes ex => [1,2,3,4] 
*)
let rec nodes g =
  let rec helper g final = 
    match g with
    | [] -> final
    | (fst, snd)::t ->
      if (not (List.mem fst final))
      then helper g (final @ [fst])
      else helper t final
    | (fst, snd)::t ->
      if (not (List.mem fst final))
      then helper g (final @ [snd])
      else helper t final
  in
  helper g []

(** [degree g]  returns the degree of [g]. The degree of a graph is 
    the maximum number of outgoing edges that any node has. 
*)
(*let rec degree g =
  let rec helper g = 
    match g with
    | [] -> []
    | (fst, snd):: t ->
*)
  

(* 
   Remove a node from the graph
   Eg. remove ex 2 =>  [(3, 1); (3, 4)] 
*)
let rec remove g n =
  match g with
  | [] -> []
  | (fst, snd):: t ->
    if (fst = n || snd = n)
    then remove t n
    else (fst, snd) :: remove t n
  
(* Reachable nodes from a source node. (Extra-credit)
   Eg. reachale ex 3 => [1,4,2,3] 
   *)

let reachable g n =
  failwith "implement"

