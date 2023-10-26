(*Isabel Sutedjo and Sean Payba
I pledge my honor that I have abided by the Stevens Honor System.*)
(* This file defines expressed values and environments *)
open Parser_plaf.Ast


(* expressed values and environments are defined mutually recursively *)

type exp_val =
  | NumVal of int
  | BoolVal of bool
  | PairVal of exp_val*exp_val
  | ProcVal of string*expr*env
  | RecordVal of (string*exp_val) list
  | ListVal of exp_val list
and
  env =
  | EmptyEnv
  | ExtendEnv of string*exp_val*env
  | ExtendEnvRec of rdecs*env 

(* Expressed Result *)
                 
type 'a result = Ok of 'a | Error of string

type 'a ea_result = env -> 'a result
  
let return (v:'a) : 'a ea_result =
  fun _env ->
  Ok v

let error (s:string) : 'a ea_result =
  fun _env ->
  Error s

let (>>=) (c:'a ea_result) (f: 'a -> 'b ea_result) : 'b ea_result =
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok v -> f v env

let (>>+) (c:env ea_result) (d:'a ea_result): 'a ea_result =
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok newenv -> d newenv


let run (c:'a ea_result) : 'a result =
  c EmptyEnv

let sequence (cs: ('a ea_result) list) : ('a list) ea_result  =
  let mcons p q = p >>= fun x -> q >>= fun y -> return (x::y)
  in List.fold_right mcons cs (return []) 

let mapM (f:'a -> 'b ea_result) (vs:'a list) : ('b list) ea_result = sequence (List.map f vs)


(* Operations on environments *)
let empty_env : unit -> env ea_result =
  fun () ->
    return EmptyEnv

let extend_env : string -> exp_val -> env ea_result =
  fun id v env ->
    Ok (ExtendEnv(id,v,env))

let extend_env_rec : rdecs -> env ea_result =
  fun list env  ->
    Ok (ExtendEnvRec(list, env))

let rec apply_env : string -> exp_val ea_result =
  fun id env ->
  match env with
  | EmptyEnv -> Error (id^" not found!")
  | ExtendEnv(v,ev,tail) ->
    if id=v
    then Ok ev
    else apply_env id tail
  | ExtendEnvRec(rdecs,tail) ->
    rechelper id rdecs tail env
  and
    rechelper =
      fun id decs tail env ->
      match decs with
      | [] -> apply_env id tail
      | (recname, par, _, _, body)::t ->
        if id=recname
        then Ok(ProcVal(par, body, env))
        else rechelper id t tail env

let lookup_env : env ea_result =
  fun env ->
  Ok env

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

let fields_of_recordVal : exp_val -> ((string*exp_val) list) ea_result = function
  | RecordVal(fs) -> return fs
  | _ -> error "Expected a record!"

let list_of_listVal : exp_val -> (exp_val list) ea_result = function
  | ListVal(vs) -> return (vs)
  | _ -> error "Expected a list!"
           
let clos_of_procVal : exp_val -> (string*expr*env) ea_result =
  fun ev ->
  match ev with
  | ProcVal(id,body,en) -> return (id,body,en)
  | _ -> error "Expected a closure!"

let rec string_of_expval' offset = function
  | NumVal n -> "NumVal " ^ string_of_int n
  | BoolVal b -> "BoolVal " ^ string_of_bool b
  | ProcVal (id,body,env) -> "ProcVal("^ id ^","^string_of_expr
                               body^","^ string_of_env' (offset+2) env^")"
  | PairVal(v1,v2) -> "PairVal("^string_of_expval' offset
                        v1^","^string_of_expval' offset v2^")"
  | RecordVal(fs) -> "RecordVal("^List.fold_left (fun s (id,ev) ->
      s ^","^id ^"="^string_of_expval' offset
                        ev) "" fs ^")"
  | ListVal(evs) -> "ListVal("^String.concat "," (List.map
                                                       (string_of_expval' offset)
                                                       evs) ^ ")"
and
  string_of_env' offset = function
  | EmptyEnv -> "[]"
  | env -> "[\n"^string_of_env'' offset env^String.make offset ' '^"]"
and
  string_of_env'' offset = function
  | EmptyEnv -> ""
  | ExtendEnv(id,v,env) -> string_of_env'' offset env^String.make offset ' ' ^" "^id^":="^string_of_expval' offset v^"\n"
  | ExtendEnvRec(decs,env) -> string_of_env'' offset env^"
  ("^String.concat "," (List.map string_of_rdecl decs)^")\n"


let string_of_env : string ea_result =
  fun env ->
  Ok ("Environment:\n"^ string_of_env' 0 env)
