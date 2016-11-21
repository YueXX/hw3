fun append1([], ys)= ys
	| append1(x::xs,ys)=x:: append1(xs,ys);

append1 ([1 ,2 ,3] , [8 ,9]);

tl [2,3,4];

fn x => if x >= 0.0 then x else ~x;

val incremented = map (fn x => x + 1) [1]

fun sq1(x:real) = x*x ;

fun sq2(x) = (x:real)*x ;


fun sumlist []      = 0 
  |   sumlist (hd::tl) = hd + (sumlist tl);

addLstFold [1,2,3,4]

fun listrev s = List.foldl (op ::) [] s;
fun length l = List.foldl (fn (_,a) => a+1) 0 l

fun rev l = foldl List.:: [] l

fun zip [] [] = []
  | zip (h::t) (k::l) = [h,k]::(zip t l);
  | zip [] (h::t) =  raise Mismatch
  | zip (h::t) [] = raise Mismatch


fun scan_left plus x [] = [x]
	| scan_left plus x [hd::tl] =  

fun scan_left plus x [] = [x]
	| scan_left plus x [hd::tl] =  


exception Mismatch
fun zip  nil l = raise Mismatch
|   zip  l   nil  = raise Mismatch
|   zip (a::la) (b::lb)  = (a,b)::(zip la lb) ;
	


