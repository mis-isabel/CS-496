(*
       Quiz 2 - Fruit Basket Processors
  
       8 Feb 2023

       Name 1: Isabel Sutedjo
       Name 2: Sean Payba
       Pledge: I pledge my honor that I have abided by the Stevens Honor System. 
*)

type 'a result = Ok of 'a | Error
type fruit = A | O | K
type 'a basket = Empty | Add of 'a*'a basket

(* Sample fruit baskets *)
let fb1 : fruit basket = Add(A,Add(A,Add(O,Add(A,Add(K,Add(K,Empty))))))
let fb2 : fruit basket = Add(A,Add(A,Add(A,Add(A,Empty))))

(* 
   A fruit basket processor is any expression whose type is:

      fruit basket -> t result

   for any type t. Some examples of types that have this form are: 
   Eg. fruit basket -> int result
   Eg. fruit basket -> bool result
   Eg. fruit basket -> (fruit basket) result

   A fruit basket processor analyzes a fruit basket and can:
   1. Either, fail (Error)
   2. Or, succeed (Ok v, where v the result of processing it)
*)

(* 
   Implement the following fruit basket processors.
   NOTE: You are free to add the "rec" keyword just after the "let", if needed.
 *)
    
(** [no_of_oranges fb] fruit basket processor that returns the number of oranges in the fruit basket [fb].
    Eg. no_of_oranges fb1 => Ok 1
*)

let rec helpOrange : fruit basket -> int = 
  fun fb ->
  match fb with
  | (Empty) -> 0
  | (Add(h, t)) -> 
    if (h != O) 
    then helpOrange(t)
    else 1 + helpOrange(t)


let no_of_oranges : fruit basket -> int result =
  fun fb ->
  Ok (helpOrange (fb))

(** [has_apples fb] fruit basket processor that determines whether there are apples in [fb] 
    Eg. has_apples fb1 => Ok true *)

let rec helpApple : fruit basket -> bool = 
  fun fb ->
  match fb with
  | (Empty) -> false
  | (Add(h, t)) -> 
    if (h != O) 
    then helpApple(t)
    else true

let has_apples : fruit basket -> bool result =
  fun fb ->
  Ok (helpApple (fb)) 
    
(** [apples_to_oranges_ratio fb] fruit basket processor that returns
    the quotient between the number of apples and the number of
    oranges in [fb].
    It should return [Error] if there are no oranges. 
    Eg. apples_to_oranges_ratio fb1 => Ok 3
        apples_to_oranges_ratio fb2 => Error
*)

let rec countApple : fruit basket -> int = 
  fun fb -> 
    match fb with 
    |(Empty) -> 0
    | (Add(h, t)) -> 
      if (h != A) 
      then countApple(t)
      else 1 + countApple(t)

let apples_to_oranges_ratio : fruit basket -> int result =
  fun fb ->
  if (helpOrange (fb) = 0)
  then Error
  else Ok (countApple (fb) / helpOrange (fb))

(** [apples_to_kiwis_ratio fb] fruit basket processor that returns
    the quotient between the number of apples and the number of
    kiwis in [fb].
    It should return [Error] if there are no kiwis. 
    Eg. apples_to_kiwis_ratio fb1 => Ok 1
        apples_to_kiwis_ratio fb2 => Error
*)
let rec helpKiwi : fruit basket -> int = 
  fun fb -> 
    match fb with
  | (Empty) -> 0
  | (Add(h, t)) -> 
    if (h != K) 
    then helpKiwi(t)
    else 1 + helpKiwi(t)

let apples_to_kiwis_ratio : fruit basket -> int result =
  fun fb ->
    if (helpKiwi (fb) = 0)
    then Error
    else Ok (countApple (fb) / helpKiwi (fb))
  

(** [ratio_sum fb] fruit basket processor that returns the sum of the
    apples-to-oranges ratio and the apples-to-kiwis ration in [fb].
    IMPORTANT: YOU MUST USE [apples_to_oranges_ratio] AND
    [apples_to_kiwis_ratio] FROM ABOVE.
    Eg. ratio_sum fb1 => Ok 4
        ratio_sum fb2 => Error
*)
let ratio_sum : fruit basket -> int result =
  fun fb ->
  if ((helpKiwi (fb) = 0) || (helpOrange (fb) = 0)) 
  then Error
  else Ok ((countApple (fb) / helpOrange (fb)) + (countApple (fb) / helpKiwi (fb)))



