datatype 'a inflist = NIL
| CONS of 'a*(unit->'a inflist);


fun HD (CONS(a,b)) = a
| HD NIL = raise Subscript;

fun TL (CONS(a,b)) = b()
| TL NIL = raise Subscript;

fun NULL NIL = true
| NULL _ = false;


fun FILTER f l = if NULL l
then NIL
else if f (HD l)
then CONS(HD l, fn () =>
(FILTER f (TL l)))
else FILTER f (TL l);

fun TAKE(xs, 0) = []
| TAKE(NIL, n) = raise Subscript
| TAKE(CONS(x,xf), n) = x::TAKE(xf(), n-1);

fun even(n:int):bool=n mod 2 = 0;
fun odd(n:int):bool=n mod  2 <>0;


fun fib a b = CONS(a,fn()=>fib b (a+b));
val fibs=fib 0 1;

val evenFibs=FILTER even fibs;
val oddFibs=FILTER odd fibs;

fun printGenList f l=if length l =0 then()
else (f (hd l); printGenList f (tl l));

(*------------------------------------------------------------------------------*)

(*val printList=printGenList (fn x:int=>Int.toString(x));*)
val printList=printGenList (fn x:int=>(print (Int.toString(x)^" ");Int.toString(x)));



(*val printPairList=printGenList( fn (x1,x2)=>(Int.toString(x1);Int.toString(x2)) );*)
val printPairList=printGenList( fn (x1,x2)=>(print ( "("^Int.toString(x1)^","^Int.toString(x2)^")"^" ");Int.toString(x1);Int.toString(x2)) );


(*-------------------------------------------------------------------------------*)
fun ZIP (l1,l2)=CONS( (HD l1, HD l2), fn()=>ZIP(TL l1, TL l2)  );
(*------------print-------------*)
printList (TAKE (fibs,20));
printList (TAKE (evenFibs,10));
printList (TAKE (oddFibs,10));
printPairList (TAKE(ZIP(evenFibs,oddFibs),10));