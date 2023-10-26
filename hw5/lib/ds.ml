(* This file defines expressed values and environments *)
open ReM
open Parser_plaf.Ast

type 'a tree = Empty | Node of 'a * 'a tree * 'a tree
                                 
type exp_val =
  | NumVal of int
  | BoolVal of bool
  | UnitVal
  | PairVal of exp_val*exp_val
  | ProcVal of string*expr*env
  | RefVal of int
  | RecordVal of (string*exp_val) list
  | ListVal of exp_val list
  | TreeVal of exp_val tree
  | TupleVal of exp_val list
and
  env =
  | EmptyEnv
  | ExtendEnv of string*exp_val*env
  | ExtendEnvRec of string*string*expr*env

(* Environment Abstracted Result *)

type 'a ea_result = ('a,env) a_result

let run (c:'a ea_result) : 'a result =
  c EmptyEnv


(* let sequence (cs: ('a ea_result) list) : ('a list) ea_result  =
 *   let mcons p q = p >>= fun x -> q >>= fun y -> return (x::y)
 *   in List.fold_right mcons cs (return [])  *)

(* let mapM (f:'a -> 'b ea_result) (vs:'a list) : ('b list) ea_result = sequence (List.map f vs) *)

let lookup_env : env ea_result =
  fun env ->
  Ok env

(* Operations on environments *)
let empty_env : unit -> env ea_result =
  fun () ->
  return EmptyEnv

let extend_env : string -> exp_val -> env ea_result =
  fun id v ->
  fun env ->
  Ok (ExtendEnv(id,v,env))

let extend_env_rec : string -> string -> expr -> env ea_result =
  fun id par body ->
  fun env  ->
  Ok (ExtendEnvRec(id,par,body,env))

let rec extend_env_list : string list -> exp_val list -> env ea_result = fun ids evs ->
  match ids,evs  with
  | [],[] -> lookup_env
  | id::ids, _ when List.mem id ids -> error "duplicate identifiers"
  | id::ids, ev::evs -> extend_env id ev >>+ extend_env_list ids evs
  | _,_ -> error "extend_env_list: Arguments do not match parameters!"
             
let rec apply_env : string -> exp_val ea_result =
  fun id ->
  fun env ->
  match env with
  | EmptyEnv -> Error (id^" not found!")
  | ExtendEnv(v,ev,tail) ->
    if id=v
    then Ok ev
    else apply_env id tail
  | ExtendEnvRec(v,par,body,tail) ->
    if id=v
    then Ok (ProcVal (par,body,env))
    else apply_env id tail


(* operations on expressed values *)


let int_of_numVal : exp_val -> int ea_result =  function
  |  NumVal n -> return n
  | _ -> error "Expected a number!"

let bool_of_boolVal : exp_val -> bool ea_result =  function
  |  BoolVal b -> return b
  | _ -> error "Expected a boolean!"

let pair_of_pairVal : exp_val -> (exp_val*exp_val) ea_result = function
  | PairVal(v1,v2) -> return (v1,v2)
  | _ -> error "Expected a pair!"

let clos_of_procVal : exp_val -> (string*expr*env) ea_result =
  fun ev ->
  match ev with
  | ProcVal(id,body,en) -> return (id,body,en)
  | _ -> error "Expected a closure!"

let int_of_refVal =  function
  |  RefVal n -> return n
  | _ -> error "Expected a reference!"

let fields_of_recordVal: exp_val -> ((string*exp_val) list) ea_result = function
  | RecordVal(fs) -> return fs
  | _ -> error "Expected a record!"

let list_of_listVal =  function
  | ListVal l -> return l
  | _ ->  error "Expected a list!"

let tree_of_treeVal =  function
  | TreeVal t -> return t
  | _ -> error "Expected a tree!"

let tuple_of_tupleVal : exp_val -> (exp_val list) ea_result = function
  | TupleVal(evs) -> return evs
  | _ -> error "Expected a tuple!"

let is_listVal = function
  | ListVal(_) ->  true
  | _ ->  false

let is_treeVal = function
  | TreeVal(_) ->  true
  | _ ->  false

let rec string_of_list_of_strings = function
  | [] -> ""
  | [id] -> id
  | id::ids -> id ^ "," ^ string_of_list_of_strings ids

let rec has_dupl l =
  match l with
  | [] -> false
  | h::t ->
    List.mem h t || has_dupl t

(* Pretty printing *)
           
let rec string_of_expval = function
  | NumVal n -> "NumVal " ^ string_of_int n
  | BoolVal b -> "BoolVal " ^ string_of_bool b
  | UnitVal -> "UnitVal "
  | ProcVal (id,body,env) -> "ProcVal ("^ id ^","^string_of_expr
                               body^","^ string_of_env' env^")"
  | PairVal(v1,v2) -> "PairVal("^string_of_expval
                        v1^","^string_of_expval v2^")"
  | RefVal i -> "RefVal ("^string_of_int i^")"
  | RecordVal(fs) -> "RecordVal ("^ (List.fold_left (fun s (id,ev) ->
      s^","^id^"="^string_of_expval ev) "" fs) ^")"
  | _ -> failwith "string_of_expval: not implemented"
and
  string_of_env'  = function
  | EmptyEnv -> ""
  | ExtendEnv(id,v,env) -> string_of_env' env^"\n("^id^","^string_of_expval v^")"
  | ExtendEnvRec(id,param,body,env) -> string_of_env' env^"\n("^id^","^param^","^string_of_expr body^")"

let string_of_env : string ea_result =
  fun env ->
  Ok ("Environment:\n"^ string_of_env' env)
