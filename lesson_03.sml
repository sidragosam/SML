(*fun map f nil = nil
  |map f (x::xr)= (f x):: (map f xr)*)

map (fn x => x+1) [1,2,3];

val minus5 = fn x => x-5;

map minus5 [~2, 1, 0, 1, 2];

map rev [[1,2,3], [4,5,6]];

List.filter (fn x => (x >=0)) [1, ~1, 2, ~2, 0];

fun filter f nill = nill
  | filter p (x::xs) = if (p x) then x:: 
(filter p xs) else (filter p xs);  

filter (fn x => x mod 2 =0) [0,1,2,3,4,5];

filter (fn x => true) [true andalso false];

filter (fn x => true) [true andalso false, 3+2 >= 1];

(*miért értékeli ki a kifejezéseket a fentisorban*)

fun exists f nill = false
  |exists f (x::xr) = (f x) orelse exists f xr;

fun all f nil = true
  | all f (x::xs) = (f x) andalso all f xs;

(* írjuk fel az  all predikátummal azt, hogy egy szám prímszám.*)
(* ugyanez az exist predikatummal*)

fun foldl f s nil = s 
  | foldl f s (x::xs) = foldl f (f(x,s))
xs;

foldl op+ 0 [1,2,3,4,5];

foldl (fn (x,y) => x +1) 0 [1,2,3,4,5];

fun foldr f s nil = s 
  | foldr f s (x::xs) =  f(x, foldr f s xs);

fun revl xs = foldl op:: []xs;

revl [1,2,3,4,5];

fun newl xs = foldr op:: [] xs;

newl [1,2,3,4,5];

(*
1. append operatort írjunk fel a foldr segítségével
2. irjunk egy revapp operatort, mely az első argumentum megfordításával fűzi össze a masodik argumentummal
3. irjunk egy foldl-el a lista elemeit osszeszorzo fuggvenyt.
4. irjunk egy memb: 'a -> ('a list) -> bollean függvényt, mely eldönti, hogy az elem benne van-e a listában.
5. count : 'a -> (a' list) -> int függvény, mely egy adott elem elorodulasai szamat adja vissza
6. adjuk me egy szam decimalis feldonbatast, azaz 7896 -> [7,8,9,6]
*)
(*
7. adjuk meg ennek a fordítottját, inverzfuggvenyet  [7,8,9,6] ->  7896
8. irjuk fel a foldl-t a foldr segítségével.
menete:
a, írjuk fel az appendet a foldr-rel.
b, adjuk meg a rev operatort  append es foldr segítségével
c, adjuk meg a foldl-t a foldr es a rev segítségével
d, adjuk meg a foldlt csak a foldr-rel
*)


fun null nil = true
  |null (x::xr) = false;


fun nth(xs,n) = if n<0 orelse null xs then
raise Subscript
else if n= 0 then hd xs else nth(tl xs, n-1);

nth ([1,2,3,4,5], 3);

(* nth ([1,2,3,4,5], 10); *)

(*
adjuk meg egy last: 'a list -> 'a függvényt, mely a lista utolso elemét adja vissza, egyébként az Empty nevű kivételt generálja
2. keressük meg a foldl segítségével egy lista legnagyobb elemét, azaz a maxlist: 'a list -> 'a
3. take(xs, n): a lista elso eleme, ahol a lista nullaval szamozodik, han<0 vagy |xs | <n akkor subscript kivételt generálja
4. drop(xs, n) a lista , kivéve az első n elemét ahol a lista a nullaval szamozodik , ha n<0 vagy |xs | <n akkor a sbuscript kivételt generálja
*)

List.take([1,2,3],2);
List.drop([1,2,3],2);

explode ("hello bazdmeg");
implode ([#"R",#"A",#"K"]);

(*
1. irjunk egy függvényt mely egy sztring hosszat adja meg
2 irjunk egy sztringeket megfordító függvényt
3. irjunk egy sztringeket osszehasonlito fuggvenyt.
4. isDigit: char -> bool, ellenőrzi, hogy egy adott karakter számjegy-e
5. ToInt string -> int, toInt "123" = 123.
*)
