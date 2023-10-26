open Ds
open Parser_plaf.Ast
open Parser_plaf.Parser
    
(*
Isabel Sutedjo and Sean Payba
I pledge my honor that I have abided by the Stevens Honor System.   
*)

(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> int result =
  fun e ->
  match e with
  | Int n      -> return n
  | Add(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (n+m)   
  | Sub(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (n-m)   
  | Mul(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (n*m)   
  | Div(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    if m=0
    then error "Division by zero"
    else return (n/m)
  | Abs(e) ->
    eval_expr e >>= fun n ->
    return (abs n)
  | Avg([]) ->
    eval_expr [] >>= fun n ->
      return (avg n)
  | Avg(es) ->
    match es with 
    | [] -> error "Error \"avg: empty sequence\""
    | h::t -> 
      eval_expr es >>= fun n -> 
      return (List.fold_left (+) 0 es) / (List.length es)
and
  eval_exprs : expr list -> (int list) result  =
  fun es ->
  match es with
  | [] -> return []
  | h::t -> 
    eval_expr h >>= fun i ->
    eval_expr t >>= fun j ->
      return(i::j)

      
(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : int result =
  e |> parse |> eval_prog



