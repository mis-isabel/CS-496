
(** Basic functions on finite automata *)
(* ******************************************** *)
(**
   Code stub for assignment 1
   Isabel Sutedjo
   I pledge my honor that I have abided by the Stevens Honor System.
*)

type symbol = char
type input = char list

type state = string

(* transition function *)
type tf = (state * symbol * state) list

(* initial state * transition function * end state *)
type fa = { states: state list; start:state; tf: tf; final: state list}


(* ******************************************** *)
(* Examples of automata *)
(* ******************************************** *)

let a = {states = ["q0";"q1";"q2"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
         final = ["q2"]}

let a2 = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1")
               ; ("q1",'c',"q2");  ("q3",'a',"q4")];
          final = ["q2"]
         }

let a3 = {states = ["q0";"q1";"q2";"q3"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1")
               ; ("q1",'c',"q2");  ("q0",'a',"q3")];
          final = ["q2";"q3"]}
let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]



(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec getSym tf = 
  match tf with
  | [] -> []
  | (st, sy, fin)::t -> sy :: getSym t

(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)

(*apply_transition_function : tf -> state -> symbol -> state option The expression apply_transition_function f st sym applies the transition function f to
the symbol sym assuming that the current state is st. *)
let rec apply_transition_function tf state symbol =
  match tf with
    | [] -> None 
    | (st, sy, fin)::t ->
      if (sy = symbol && st = state) 
      then Some fin 
      else apply_transition_function t state symbol 

(*accept : fa -> input -> bool
Determine whether a word is accepted by a finite automaton.*)
 let rec accept fa input =  
  let rec accHelp fa input currSt = 
    match input with
    | [] when List.mem currSt fa.final -> true
    | [] -> false
    | h::t ->
      let nxtSt = apply_transition_function fa.tf currSt h in
      match nxtSt with
      | None -> false
      | Some nxtSt -> accHelp fa t nxtSt
  in 
  accHelp fa input fa.start

(*next : tf -> state -> symbol -> state list
This functions returns the list of all the states that are successors of some given state
with some give symbol.*)
 let rec next tf state symbol = 
  match tf with
  | [] -> []
  | (st, sy, fin)::t ->
    if (sy = symbol && st = state)
    then [fin] @ next t state symbol
    else next t state symbol

(*deterministic : fa -> bool
This function checks whether the given automaton is non-deterministic or not. A
non-deterministic FA is one in which there is a state that has two or more successor
states with the same symbol.*)

let rec deterministic fa =
  let rec detHelp st sym = 
    if sym = []
    then true
    else if ((List.length) (next fa.tf ((List.hd) fa.states) ((List.hd) sym))) > 1
    then false 
    else detHelp st ((List.tl) sym)
  in let rec detHelp2 st sym = 
    if st = []
    then true
    else if (detHelp st sym)
    then detHelp2 ((List.tl) st) sym
    else false
  in
  if (detHelp2 fa.states (getSym fa.tf))
  then true
  else false

(*valid : fa -> bool
Implement valid that checks for validity. A FA is said to be valid if
(a) The list of states has no duplicates;
(b) The start state belongs to set of states;
(c) The final states belong to set of states; and
(d) It is deterministic.*)

let rec valHelp fi st = 
    match fi with 
    | [] -> true
    | h::t ->
      if ((List.mem) ((List.hd) fi) st)
      then valHelp ((List.tl) fi) st
      else false 

let valid fa =
  if (((List.mem) fa.start fa.states) && (valHelp fa.final fa.states) && (deterministic fa))
  then true
  else false 

(*reachable : fa -> state list
Reports list of states that are reachable from the start state. Note that the start state
is stored in the start field of the FA. *)
let rec dif lst1 lst2 = 
  match lst1 with
  | [] -> []
  | h::t ->
    if (List.mem h lst2)
    then dif t lst2
    else h:: dif t lst2

let rec reachable fa =   
  let rec reachHelp vst curr =
    match curr with
    | [] -> vst
    | h::t -> (reachHelp (h::vst) t @ ((dif (next fa.tf fa.start ((List.hd) (getSym fa.tf))) vst)))
    in reachHelp [] [fa.start]

(*non_empty : fa -> bool
Determines whether a FA accepts at least one word. Hint: make sure that at least one
final state is reachable from the start state.*)
(* let rec non_empty fa =  *)

(*remove_dead_states : fa -> fa
Removes all dead (i.e. unreachable) states from a valid FA. This includes removing
them from the set of states, removing the transitions in δ that involve dead states and
also removing them from the set of final states.*)
(* let rec remove_dead_states fa = *)

