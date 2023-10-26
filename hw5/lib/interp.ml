open Ds
open ReM
open Parser_plaf.Ast
open Parser_plaf.Parser
       
let g_store = Store.empty_store 20 (NumVal 0)

let rec eval_expr : expr -> exp_val ea_result = fun e ->
  match e with
  | Int(n) -> return @@ NumVal n
  | Var(id) -> apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1+n2)
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1-n2)
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1*n2)
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return @@ NumVal (n1/n2)
  | Let(v,def,body) ->
    eval_expr def >>= 
    extend_env v >>+
    eval_expr body 
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return @@ BoolVal (n = 0)
  | Pair(e1,e2) ->
    eval_expr e1 >>= fun ev1 ->
    eval_expr e2 >>= fun ev2 ->
    return @@ PairVal(ev1,ev2)
  | Fst(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun p ->
    return @@ fst p 
  | Snd(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun p ->
    return @@ snd p
  | Proc(id,_,e)   ->
    lookup_env >>= fun en ->
    return (ProcVal(id,e,en))
  | App(e1,e2)  -> 
    eval_expr e1 >>= 
    clos_of_procVal >>= fun (id,e,en) ->
    eval_expr e2 >>= fun ev ->
    return en >>+
    extend_env id ev >>+
    eval_expr e
  | Letrec([(id,par,_targ,_ty,e)],target) ->
    extend_env_rec id par e >>+
    eval_expr target 
  | BeginEnd(es) ->
    List.fold_left (fun c e -> c >>= fun _ -> eval_expr e) (return UnitVal) es
  | NewRef(e) ->
    eval_expr e >>= fun ev ->
    return @@ RefVal (Store.new_ref g_store ev)
  | DeRef(e) ->
    eval_expr e >>=
    int_of_refVal >>= 
    Store.deref g_store 
  | SetRef(e1,e2) ->
    eval_expr e1 >>=
    int_of_refVal >>= fun v1 ->
    eval_expr e2 >>= fun v2 ->
    Store.set_ref g_store v1 v2 >>= fun _ ->
    return UnitVal
        | Cons(e1, e2) ->
    eval_expr e1 >>= fun v1 ->
    eval_expr e2 >>=
    list_of_listVal >>= fun l1 ->
    return @@ ListVal (v1 :: l1)
  | Hd(e1)      ->
    eval_expr e1 >>=
    list_of_listVal >>= fun l1 ->
    if l1=[]
    then error "Hd: Empty list"
    else return @@ List.hd l1
  | Tl(e1)      ->
    eval_expr e1 >>=
    list_of_listVal >>= fun l1 ->
    if l1=[]
    then error "Tl: Empty list"
    else return @@ ListVal (List.tl l1)
  | Record(fs) ->
    let (ids,fes) = List.split fs
    in let (_flags,es) = List.split fes
    in
    if has_dupl ids
    then error "Record: duplicate fields"
    else 
    sequence (List.map eval_expr es) >>= fun evs ->
    return (RecordVal(List.combine ids evs))
  | Proj(e,id) ->
    eval_expr e >>=
    fields_of_recordVal >>= fun fs ->
    (match List.assoc_opt id fs with
    | None -> error "Proj: field does not exist"
    | Some ev -> return ev)
  | IsEmpty(e1)     ->
    eval_expr e1 >>= fun v1 ->
    if is_listVal v1 || is_treeVal v1
    then
      (if is_listVal v1
       then list_of_listVal v1 >>= fun l1 ->
         return @@ BoolVal (l1 = [])
       else tree_of_treeVal v1 >>= fun t1 ->
         return @@ BoolVal (t1 = Empty)
      )
    else error "Expected a tree or a list!"
  | EmptyList(_t)    -> return @@ ListVal []
  | EmptyTree(_t) -> return @@ TreeVal Empty
  | Node(e1,lte,rte) ->
    eval_expr e1 >>= fun data ->
    eval_expr lte >>=
    tree_of_treeVal >>= fun lt ->
    eval_expr rte >>=
    tree_of_treeVal >>= fun rt ->
    return @@ TreeVal (Node(data,lt,rt))
  | CaseT(target,emptycase,id1,id2,id3,nodecase) ->
    eval_expr target >>=
    tree_of_treeVal >>= fun v1 ->
    (match v1 with
        | Empty -> eval_expr emptycase
        | Node(v,ltree,rtree) ->
          extend_env id1 v >>+
          extend_env id2 (TreeVal ltree) >>+
          extend_env id3 (TreeVal rtree) >>+
          eval_expr nodecase)
  | Tuple(es) ->
    sequence (List.map eval_expr es) >>= fun ts ->
    return (TupleVal ts)
  | Untuple(ids,e1,e2) ->
    eval_expr e1 >>=
    tuple_of_tupleVal >>= fun ts ->
    extend_env_list ids ts >>+
    eval_expr e2
  | Debug(_e) ->
    string_of_env >>= fun str_env ->
    let str_store = Store.string_of_store string_of_expval g_store 
    in (print_endline (str_env^"\n"^str_store);
        error "Reached breakpoint")
  | _ -> failwith ("Not implemented: "^string_of_expr e)

let eval_prog (AProg(_,e)) =
  eval_expr e   

(* Interpret an expression *)
let interp (s:string) : exp_val result =
  let c = s |> parse |> eval_prog
  in run c

