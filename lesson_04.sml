(*infix jelölés*)
datatype 'a tree = L | B of 'a tree * 'a tree;

(*prefix jelölés*)
datatype 'a tree = L | N of 'a* 'a tree* 'a tree;

(*fa csomópontjainak száma*)
fun nodes L = 0
  | nodes (N(_, t1, t2))  = 1 + nodes t1 + nodes t2;

val tree1 = N(3, N(5, N(1, L, L), L), N(10, L, L));

nodes tree1;

(*HF:
1. Írjunk fel egy olyan fa adattípust, melyben a levelekben vannak egész szám elemek, a csómopontok "üresek".
2. Írjunk fel egy olyan fa adattípust, melyben a csomópontok és a levelek egyaránt egészek, a levelek lehetnek üres típusúak, azaz egy tetszőleges nullváltozós adatkonstruktor.
3. Adjuk meg a nodes függvényt ezekre a különböző adattípusokra.
4. Írjuk fel a nodes egy tail recursive változatát.
*)

(*fa mélysége*)
fun depth' L = 0
  | depth' (N(_, t1, t2)) = 1 + Int.max(depth' t1, depth' t2);

depth' tree1;

(*fa mélység tail rekurzív változata*)
fun depth t =
  let fun depth0 (L, d) = d
    | depth0 (N(_, t1, t2), d) = Int.max(depth0 (t1, d+1), depth0 (t2, d+1))
in
  depth0(t, 0)
end;

depth tree1;

(*építsünk fel egy n mélységű teljes fát (ahol minden csúcsnak pontosan két gyermeke van)*)
fun fulltree n =
let
  fun ftree (_, 0) = L
    | ftree (k, n) = N(k, ftree(2*k, n-1), ftree(2*k+1, n-1))
in
  ftree(1, n)
end;

val fulltree1 = fulltree 3;

(*HF:
5. Írjunk egy függvényt, mely a fát tükrözi a gyökéren átmenő függőleges tengelyre.
*)

fun preorder t =
