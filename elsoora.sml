fun fact 0 =1
  |fact n = n*fact(n-1);

fact 10;

fun plus (n, 0) = n
  |plus (n, m) = 1 + plus (n, m-1);
(*feltesszük, hogy adott a rákövetkező függvény, azaz n |-> n+1 *)

plus (12, 15);

fun times (n, 0) = 0
  |times (n,m) = plus (n, times (n, m-1));

times(7, 8);


fun exp (n, 0) = 1
  |exp (n,m) = times (n, exp (n, m-1));

exp(2, 3);


fun gcd (n, 0) = n
  |gcd(n,m) = gcd(m, n mod m);

(* az n es m legnagyobb kozos osztója az m es az n mel való osztasi maradékának legnagyobb közös osztója (euklideszi*)

gcd(24, 72);

(* hazi feladat írjuk meg ugyenezt egy felteteles utasitas formajaban *)

fun lnko (n, m) = if n= m then n else
if n > m then lnko (n-m, m)
     else lnko (m,n);

lnko(25, 60);

exception noRealSolution;

fun solve(a, b, c) =
  let val d = b*b-4.0*a*c in
  if d<0.0 then raise noRealSolution else
  ((~b+Math.sqrt d)/(2.0*a), (~b-Math.sqrt d)/(2.0*a))
end; 

solve (1.0, ~1.0, ~2.0);

type qnum = int*int;
exception QDIV;

fun mkQ(_,0) = raise QDIV
  |mkQ quo = quo: qnum;

infix 6 ++ --;
infix 7 ** //;
infix 4 ==;


fun (a,b) ++ (c,d) = (a*d + b*c, b*d)
fun (a,b) -- (c,d) = (a*d - b*c, b*d)
fun (a,b) ** (c,d) = (a*c, b*d)
fun (a,b) // (c,d) = (a,b) ** mkQ(d,c)
fun (a,b) == (c,d) = (a*d = b*c);


fun toString (p,q) =
let val sign = if p*q<0 then "~" else ""
  val ap = abs p
  val aq = abs q
  val d = gcd (ap, aq)
in if d<>q then sign^(Int.toString (ap div d))^"/"^(Int.toString (aq div d))
  else sign^(Int.toString (ap div d))
end;

toString (10, 14);

(* ezt  megírni hogy a kiiratasnal racionalis szamok olyan egesz parok legyenek, melyekre a szamlalo és a nevezo legnagyobb kozos osztoja 1*)

fun downto1 0 = []
  |downto1 n = n :: downto1(n-1);

downto1 10;

fun upto(k,n) = if k>n then [] else
k::upto(k+1,n);

upto (5, 10);

fun uptoN n =
let fun auxupto i = if i>n then [] else 
i::auxupto (i+1)
in auxupto 1
end;

uptoN 10;


fun suml [] = 0 
  |suml (x::xs) = x + suml xs;

infix 5 @;

fun [] @ ys = ys
  |(x::xs)@ys = x::(xs@ys);

fun naiveRev [] = []
  |naiveRev (x::xs) = naiveRev xs @ [x];

naiveRev[1,2,3];

infix member;
fun  x member [] = false 
  | x member (y::ys) = x=y orelse x member 
ys;

3 member [1,2,3];

(* fuggveny mely egy adott elem minden elofordulasa eltavolitja a listabol. *)
(* irjunk fuggvenyt mely a többszörös előfordulasokbol egyet tart meg a listaban*)

