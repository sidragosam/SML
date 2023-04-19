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

