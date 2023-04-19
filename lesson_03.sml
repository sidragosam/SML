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

fun preorder t = case t of
                L => []
  | N(v, t1, t2) => v::(preorder t1 @ preorder t2);

preorder tree1;

(*1. inorder
2. preorder*)

fun preord (L, vs) = vs
  | preord(N(v, t1, t2), vs) = v::preord (t1, preord(t2, vs));

preord (tree1, []);

(*HF:
6. Tail recursive módon írjuk fel az inord és postord függvényeket.
*)


(*HF:
7. Írjuk fel ezeket a függvényekett hd és t1 használata nélkül
*)
fun take (n, l) = if n = 0 then [] else hd l::take(n-1, tl l);

fun drop (n, l) = if n = 0 then l else drop (n-1, tl l);

take(3, [1, 2, 3, 4, 5]);
drop(2, [1, 2, 3, 4, 5]);

(*preorder bejárású, majdnem kiegyensúlyozott fa (leghosszabb és legrövidebb ágai hosszainak különbsége legfeljebb egy)*)
fun preorderfa (x::xs) =
let val k = length xs div 2
in
  N(x, preorderfa(take(k, xs)), preorderfa(drop(k, xs)))
end
  | preorderfa[] = L;

preorderfa [1, 2, 3, 4, 5, 6];
(*HF:
8. Írjunk olyan takedrop függvényt, hogy a takedrop(k, xs) listapárt ad vissza anélkül, hogy megírnánk külön a take vagy a drop függvényeket.
*)
