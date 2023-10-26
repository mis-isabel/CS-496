(* 

   Stub for HW2 
   Please
   1. Rename to gt.ml
   2. Place your name here:

    Name: Isabel Sutedjo
    Pledge: I pledge my honor that I have abided by the Stevens Honor System.
*)

type 'a gt = Node of 'a*('a gt) list

let mk_leaf (n:'a) : 'a gt =
  Node(n,[])
    
let t : int gt =
 Node (33,
       [Node (12,[]);
        Node (77, 
              [Node (37, 
                     [Node (14, [])]); 
               Node (48, []); 
               Node (103, [])])
       ])

let max x y = if x > y then x else y

let rec unbrack (l:'x list list) : 'x list =
  match l with
  | [] -> []
  | h::t -> h @ (unbrack t)

let rec height t =
  match t with 
  | Node (_, []) -> 1
  | Node (_, child) -> 
    let deep = 
      List.fold_left (fun l r -> max l r) 1 (List.map height child) 
    in 1 + deep
   
let rec size t =
  match t with 
  | Node (_, []) -> 1
  | Node (_, child) -> 
    let add = 
      List.fold_left (+) 0 (List.map size child) 
    in 1 + add

let rec paths_to_leaves t =
   match t with 
   | Node (h, []) -> [[]]
   | Node (h, l) ->
    unbrack(List.mapi (fun i x -> List.map(fun j -> i::j) x) (List.map (paths_to_leaves) l))

let rec is_leaf_perfect t =
  match t with 
  | Node (h, []) -> true
  | Node (h, x::y) ->
  (List.exists (fun p -> if (p) then true else false) (List.map (fun j -> if (height(x) != j) then false else true) (List.map height y)))

let rec preorder (Node(d,ch)) =
  match Node(d, ch) with
  | Node (h, []) -> [h]
  | Node (h,l) -> [h] @ unbrack(List.map (preorder) l)
                       
let rec mirror (Node(d,ch)) =
  match Node(d, ch) with
  | Node (h, []) -> Node(h,[])
  | Node (h, child) -> Node(h,List.rev(List.map mirror child))

let rec map f (Node(d,ch)) =
  match Node(d, ch) with
  | Node (h, []) -> Node(f h, [])
  | Node (h, child) -> Node(f h, List.map (fun x-> map f x) child)

let rec fold : ('a -> 'b list -> 'bm ) -> 'a gt -> 'b =
  fun f (Node(d,ch)) ->
  match Node(d, ch) with
  | Node (h, []) -> f h []
  | Node (h, child) -> f h (List.map (fun x-> fold f x) child)
  
let sum t =
  fold (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t

let mem t e = 
  fold (fun i rs -> i=e || List.exists (fun i -> i) rs) t

let mirror' t  = 
  fold (fun i rs -> Node(i,(List.rev(rs)))) t

let rec degHelp k l = 
  match l with 
  | [] -> k
  | h::t -> if k < (List.length h) then degHelp (List.length h) t else degHelp k t

let degree t  = 
  degHelp 0 (paths_to_leaves t)
  
  
