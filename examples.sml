(* Concatenating two lists *)
[1, 2, 3] @ [4, 5, 6];

(* Prepending an element to a list *)
0 :: [1, 2, 3];

(* Types cannot be mixed together in a list*)
(* [0,'a'] *)

(* However, you can mix them with pairs/tuples*)
(1, "hi");
(2.5, 1, "100");

(* Explicit typing of parameters and function. *)
fun add (x:int) (y:int):int = x + y;

(* A demonstration of currying *)
val add5 = add 5;
(* You can see when you run this file that
 * add5 will act as a function that takes in
 * one parameter, when the reality is
 * it is a "curried" version of add with 5
 * already fed in as a parameter.
 *)
add5 6;

(* This is the quickSort lecture from the slides *)
fun quickSort op< [] = []
| quickSort op< [x] = [x]
| quickSort op< (a::bs) =
  let fun
    deposit (x, (left, right)) = if x < a
      then (x::left, right)
      else (left, x::right)
    val
      (left, right) = foldr deposit ([], [a]) bs
    in
   quickSort op< left @ quickSort op< right
end

(* fibonacci function implemented without tail-recursion
 * or pattern-matching
 *)
fun fibonacci n =
  if n < 1 then 0 
  else if n < 3 then 1
         else fibonacci (n-1) + fibonacci (n-2)
(* fibonacci implemented without tail-recursion
 * but with pattern-matching
 *)
fun fib 0 = 0
  |   fib 1 = 1
  |   fib 2 = 1
  |   fib n = (fib (n-1)) + (fib (n-2))

(* fibonacci implemented with tail-recursion *)
fun fibTR n =
  let fun fibTRH 0 a b = a
         |  fibTRH n a b = fibTRH (n-1) b (a+b)
       in
    fibTRH n 0 1
  end
;

(* function for determining if integer is even *)
fun isEven x = (x mod 2 = 0);

(* Use of previously defined predicate for list filtering *)
List.filter isEven [1,2,3,4,5,6];

(* Adding all elements of a list without tail recursion *)
fun addLst [] = 0
  |   addLst (hd::tl) = hd + (addLst tl);

(* Adding all elements of a list with tail recursion *)
fun addLstTR lst =
  let fun addH acc [] = acc
       |    addH acc (hd::tl) = addH (acc + hd) tl
     in addH 0 lst
     end;

(* Adding all elements of a list using a foldl*)
fun addLstFold lst = List.foldl (op+) 0 lst;

(* Demonstrating how to use List.map *)
fun doubleLst lst = List.map (fn x => x * 2) lst;


(* fun checke [] y  = (y = 0)| checke (hd::tl) y = checke (tl) (y-hd); *)

fun checke2 (hd::tl)(y:int):bool = (hd = y);


