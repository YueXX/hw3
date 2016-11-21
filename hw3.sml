
(* 1. Implement a function listsum *)
fun listsum [](y:int):bool = (y=0)
  |	listsum (hd::tl)(y:int):bool = listsum (tl)(y-hd);


(* 2. Implement a function isten *)
fun isten xs = listsum xs 10;

(* 3. Implement length function using foldl *)
fun length lst = List.foldl (fn (x,acc) => 1+acc) 0 lst;

(* 4. Implement reverse function using foldl *)
fun reverse list = foldl op:: [] list;

(* 5. Implement function zip *)


exception Mismatch
fun zip ([],    [])    = []
  | zip (x::xs, y::ys) = (x,y) :: zip (xs,ys)
  | zip _              = raise Mismatch;


(* 6. Implement function zip *)

fun unzip [] = ([], [])
  | unzip ((x,y)::xys)  =
      let val (xs,ys) = unzip xys in (x::xs,y::ys) end;

(* 7. Implement scan_left function *)

fun plus x y = x +y;
fun scan_left plus x (hd::tl) = x::(scan_left plus (plus x hd) tl)
	| scan_left plus x [] = [x];


(* 8. Using scan_left, implement a function factorial *)

fun countup n =
let fun countup' 0 l = l
		   | countup' i l = countup' (i - 1) (i::l);
in
  countup' n []
  end
 ;

fun multiply x y = x *y;
fun fact_list n = tl (scan_left multiply 1 (countup n));


(* Test above functions *)
listsum [1,2,3] 5;
isten [2,3,5];
length [1,2,3,4];
reverse [1,2,6,4];
zip ([1,2,3],[3,4,6]);
unzip [(1,3),(2,4),(3,6)];
scan_left plus 0 [10,27,3];
fact_list 8;



