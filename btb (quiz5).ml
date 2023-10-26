(*

   Quiz 5 - Mutable Data Structures in OCaml
   31 Mar 2023
   Names: Sean Payba and Isabel Sutedjo
   Pledge: I pledge my honor that I have abided by the Stevens Honor System.
*)

type 'a node = {
  mutable data: 'a;
  mutable left: 'a node option;
  mutable right: 'a node option}


type 'a bt = {
  mutable root: 'a node option;
  mutable size: int}

(* Sample binary tree:

      7
     / \
    3   77
       /
      33
*)

let t1:int bt =
  { root = Some { data=7;
                  left = Some {data=3; left=None; right=None};
                  right = Some {data=77;
                                left=Some {data=33; left=None; right=None};
                                right=None} };
    size = 4}


(** [mem e t] determines whether [e] belongs to [t].
    Eg. 
    # mem 77 t1;;
    - : bool = true
    # mem 78 t1;;
    - : bool = false
*)
let rec help : 'a -> 'a node option -> bool =
  fun e t ->
    match t with 
    | None -> false
      | Some ndp  -> 
        if(ndp.data != e)
          then ((help e ndp.left) || (help e ndp.right))
      else true


let mem : 'a -> 'a bt -> bool =
  fun e t ->
  match t.root with 
    | None -> false
    | Some ndp  -> 
      if(ndp.data != e)
        then ((help e ndp.left) || (help e ndp.right))
    else true

(** [maxt t] returns the largest element in tree [t]. 
    It fails if [t] is empty.
    Eg. 
    # maxt t1;;
    - : int = 77
*)

let maxt : 'a bt -> 'a =
  fun t ->
  match t.root with
  | None -> failwith "[t] is empty."
  | Some node -> 
  let rec help curr maxVal =
      let new_max = max curr.data maxVal in
      let max_left = match curr.left with
        | None -> new_max
        | Some left_node -> help left_node new_max in
      match curr.right with
      | None -> max_left
      | Some right_node -> help right_node max_left in
    help node min_int
  
(** [rem_max no] removes the maximum element from a BINARY SEARCH    tree rooted at [no]. It fails if [no] is None.
    NOTE1: this function applies to a NODE not a TREE. 
    NOTE2: this function returns a NODE.
    NOTE3: this function does not rely on the previous one.
    Eg. 
    # t1.root <- rem_max_node t1.root;;
    - : unit = ()
    # t1;;
    - : int bt =
    {root = Some
             {data = 7; left = Some {data = 3; left = None; right = None};
              right = Some {data = 33; left = None; right = None}};
     size = 3}
*)
let rec rem_max : 'a node option -> 'a node option =
  fun no -> 
  match no with
  | None -> failwith "[no] is None."
  | Some node ->
    match node.right with
    | None -> node.left
    | Some right_node ->
      node.right <- rem_max (Some right_node);
      Some node
  