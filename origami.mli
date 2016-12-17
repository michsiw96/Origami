type point = float * float
(* Point on a plane *)

type kartka = point -> int
(* [kartka p] (kartka = sheet of paper) 
   returns how many layers of origami are in point p *)

val prostokat : point -> point -> kartka
(* [prostokat p1 p2] returns a sheet, representing a rectangle
   with left corner in point [p1] and right corner in point [p2] *)

val kolko : point -> float -> kartka
(* [kolko p r] returns sheet, representing circle with 
   radius [r] and center in point [p] *)

val zloz : point -> point -> kartka -> kartka
(* [zloz p1 p2 k] folds sheet [k] along a line intersecting
   points [p1] and [p2] (different points). Paper is 
   folded from right to left side of a line *)

val skladaj : (point * point) list -> kartka -> kartka
(* [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = 
   zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...)] 
   that means sheet [k] is folded along every line [p1_i p2_i] in this particular order *) 