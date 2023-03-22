(*lnko két különböző algoritmussal*)

fun gcd(m, n)= 
  if m = 0 then n
  else gcd(n mod m, m);

gcd(12, 32);
gcd(48, 36);


fun lnko(n, m)=
  if n = m then n
  else if n < m then lnko(m, n)
  else lnko(n-m, m);

lnko(12, 32);
lnko(48, 36);

(*Rekurzív hatványozás [típuskövetkeztetéssel]*)

fun power(x, k):real =
  if k = 1 then x
  else if k mod 2 = 0 then power(x*x, k div 2)
  else x*power(x*x, k div 2);

power(2.0, 5);
(*power(2, 5); Ez hibás*)

(*Fibonacci számok [típuskövetkeztetéssel]*)
fun nextfib(prev, curr: int)=
  (curr, prev+curr);

nextfib(0,1);
nextfib(1,1);
nextfib(1,2);
nextfib(2,3);

fun fibpair(n)=
  if n = 1 then (0,1)
  else nextfib(fibpair (n-1));

fibpair(12);

fun snd(_, y) = y;

fun fib1(n) = snd(fibpair(n));

fib1(12);

fun itfib(n, prev, curr)=
  if n= 1 then curr
  else itfib (n-1, curr, prev+curr);

fun fib2(n) = itfib(n, 0, 1);

fib2(12);

(*Négyzetgyök egészrészének kiszámítása*)
fun increase(k, n) = 
  if (k+1)*(k+1) > n then k
  else k+1;

increase(1, 10);
increase(2, 10);
increase(3, 10);

fun introot(n) =
  if n = 0 then 0
  else increase(2*introot(n div 4), n);

introot(10);
introot(72);

(*Newton-Raphson módszer*)

fun root(a, x, accu)=
  let val nextx = (a/x + x) / 2.0
in if abs(x - nextx) < accu*x then nextx
  else root(a, nextx, accu)
end;

fun sqroot a = root(a, 1.0, 1.0E~10);

sqroot 2.0;
it*it;

sqroot 3.0;
it*it;

fun newroot(a) =
  let val accu = 1.0E~10
  fun findroot x = 
  let val nextx = (a/x+x)/2.0
  in if abs(x-nextx) < accu*x
    then nextx else findroot
    (nextx) end
in
  findroot 1.0 end;

newroot 2.0;

(*var x:=0; y:=0; z:=0;
F: x:=x+1; goto G;
G: if y<z then goto F else y:=x+y; goto H);
H: if z>0 then F(x, y, z-x) else (x,y,z)*)

fun F(x,y,z) = G(x+1, y, z)
and G(x,y,z) = if y<z then F(x, y, z)
else H(x, x+y, z)
and H(x, y, z) = if z>0 then F(x, y, z-x) else (x,y,z);


(*Faktoriális*)
fun helper(0, r:int) = r
  | helper (n:int, r:int) = helper (n-1, n*r);

fun csoda(n:int) = helper(n, 1);

csoda 10;
csoda 4;
csoda 5;

(*Páros, páratlan*)

fun even 0 = true
  | even n = odd(n-1)
  and odd 0 = false
  | odd n = even (n-1);

even(10);
even(11);

(*Listahossz*)
fun lengthl nil = 0
  | lengthl (_::t) = 1 + lengthl t;

fun naive_rev nil = nil
  | naive_rev(h::t) = naive_rev t @ [h];

naive_rev[1,2,3,4,5];

local
   fun helper (nil, a) = a
    | helper (h::t, a) = helper (t, h::a)
in 
  fun rev' l = helper (l, nil)
end;

