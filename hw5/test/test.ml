open OUnit2
open Parser_plaf.Ast
open Checked.ReM
open Checked.Checker
    
  
let tests = [
    (* ref *)
  "newref1" >:: (fun _ -> assert_equal
                    (Ok (RefType (RefType IntType)))
                    (chk "newref(newref(3))"));
  "newref2" >:: (fun _ -> assert_equal
                    (Ok (RefType (FuncType (IntType, IntType))))
                    (chk "newref(proc (x:int) { x })"));
  "setref1" >:: (fun _ -> assert_equal
                    (Ok UnitType)
                    (chk "let x = newref(5) in setref(x, 6)"));
  "deref1" >:: (fun _ -> assert_equal
                    (Ok IntType)
                    (chk "let x = newref(5) in deref(x)"));
  "deref2" >:: (fun _ -> assert_equal
                   (*                    ~printer:string_of_texpr *)
                    (Ok (RefType IntType))
                    (chk "deref(newref(newref(3)))")); 
    (* pair *)
    "pair1 " >::
        (fun _ -> assert_equal
            (*            ~printer:string_of_result *)
            (Ok (PairType (IntType, IntType)))
            (chk "pair(3, 4)"));
    
    "pair2 " >::
        (fun _ -> assert_equal
            (Ok (PairType ((PairType (IntType, IntType)), IntType)))
            (chk "pair(pair(3, 4), 5)"));
    
    "pair3 " >::
        (fun _ -> assert_equal
            (Ok (PairType (BoolType, IntType)))
            (chk "pair(zero?(0), 3)"));
    
    "pair_proc " >::
        (fun _ -> assert_equal
            (Ok (PairType (FuncType (IntType, IntType), IntType)))
            (chk "pair(proc (x:int) { x - 1 }, 4)"));
    
    "unpair1 "  >::
        (fun _ -> assert_equal
            (Ok IntType)
            (chk "unpair (a, b) = pair(1, 2) in a + b"));
    
    "unpair_proc " >::
        (fun _ -> assert_equal
            (Ok (FuncType ((PairType (IntType, BoolType)), (PairType (BoolType, IntType)))))
            (chk "proc (z:int*bool) { unpair(x, y) = z in pair(y, x) }"));
    
    "pair_bool_int " >::
        (fun _ -> assert_equal
            (Ok (PairType (BoolType, IntType)))
        (chk "let f = proc (z:int*bool) { unpair (x,y)=z in pair(y,x) } in (f pair(1, zero?(0)))"));
    
    (* list *)
    "emptylist" >::
        (fun _ -> assert_equal
            (Ok (ListType (IntType)))
            (chk "emptylist(int)"));
    
    "single_ele_int " >::
        (fun _ -> assert_equal
            (Ok (ListType (IntType)))
            (chk "cons(1, emptylist(int))"));
    
    "single_ele_bool " >::
        (fun _ -> assert_equal
            (Ok (ListType (BoolType)))
            (chk "cons(zero?(1), emptylist(bool))"));
    
    "list_of_list_let_int " >::
        (fun _ -> assert_equal
            (Ok (ListType (ListType (IntType))))
            (chk "let x = 4 in cons(cons(x - 1, emptylist(int)), emptylist(list(int)))"));
    
    "is_emptylist" >::
        (fun _ -> assert_equal
            (Ok BoolType)
            (chk "empty?(emptylist(list(int)))"));
    
    "hd_list_int " >::
        (fun _ -> assert_equal
            (Ok IntType)
            (chk "hd(cons(1, emptylist(int)))"));
    
    "tl_list_int " >::
        (fun _ -> assert_equal
            (Ok (ListType (IntType)))
            (chk "tl(cons(1, emptylist(int)))"));
    
    (* tree *)
    "empty_tree " >::
        (fun _ -> assert_equal
            (Ok (TreeType (IntType)))
            (chk "emptytree(int)"));
    
    "empty_tree " >::
        (fun _ -> assert_equal
            (Ok BoolType)
            (chk "empty?(emptytree(int))"));
    
    "non_empty_tree " >::
        (fun _ -> assert_equal
            (Ok BoolType)
            (chk "empty?(node(1, node(2, emptytree(int), emptytree(int)), emptytree(int)))"));
    
    "get_data_tree " >::
        (fun _ -> assert_equal
            (Ok IntType)
            (chk
               "caseT node(1, node(2, emptytree(int), emptytree(int)),
        emptytree(int)) of { emptytree() -> 0, node(d,l,r) -> d }"));
    
    "get_lst " >::
        (fun _ -> assert_equal
            (Ok (TreeType (IntType)))
            (chk "caseT node(1, node(2, emptytree(int), emptytree(int)),
        emptytree(int)) of { emptytree() -> emptytree(int), node(d,l,r) -> l }"));
    
    "get_rst " >::
        (fun _ -> assert_equal
            (Ok (TreeType (IntType)))
            (chk "caseT node(1, node(2, emptytree(int), emptytree(int)),
        emptytree(int)) of { emptytree() -> emptytree(int), node(d,l,r) -> r }"));

]

let _ = run_test_tt_main ("suite" >::: tests)
