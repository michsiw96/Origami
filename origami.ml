type point = float * float;;
type kartka = point -> int;;

  
let znak a b x = 
	let x0 = fst(a) and x1 = fst(b) and x2 = fst(x)
	and y0 = snd(a) and y1 = snd(b) and y2 = snd(x) in
		((x1 -. x0) *. (y2 -. y0)) -. ((x2 -. x0) *. (y1 -. y0))
	
let skalar a b x = 
	let x0 = fst(a) and x1 = fst(b) and x2 = fst(x)
	and y0 = snd(a) and y1 = snd(b) and y2 = snd(x) in
		(((x2 -. x0) *. (x1 -. x0)) +. ((y2 -. y0) *. (y1 -. y0))) /.
		(((x0 -. x1) *. (x0 -. x1)) +. ((y0 -. y1) *. (y0 -. y1))) ;;

let rzut a b x = 
	let x0 = fst(a) and x1 = fst(b)
	and y0 = snd(a) and y1 = snd(b) and u = skalar a b x in
		(((x1 -. x0) *. u) +. x0, ((y1 -. y0) *. u) +. y0)

let odbicie a b x = 
	let x0 = fst(x) and x1 = fst(rzut a b x)
	and y0 = snd(x) and y1 = snd(rzut a b x) in
		((2. *. x1) -. x0, (2. *. y1) -. y0)

let prostokat p1 p2 = 
	fun x -> if fst(x) >= fst(p1) && fst(x) <= fst(p2) &&
				snd(x) >= snd(p1) && snd(x) <= snd(p2) then 1
				else 0

let square x = x *. x

let odl p1 p2 = 
	let a = fst(p1) and b = fst(p2) and c = snd(p1) and d = snd(p2) in
		square(a -. b) +. square(c -. d)
	
let kolko p r = 
	fun x -> if (odl x p) <= square r then 1 else 0
	
let zloz p1 p2 k = 
	fun x -> if znak p1 p2 x > 0. then k (odbicie p1 p2 x) + k x
			 else if znak p1 p2 x = 0. then k x
			 else 0

let skladaj l k = 
	List.fold_left (fun a h -> zloz (fst(h)) (snd(h)) a) k l;;

let p = prostokat (-2.,0.) (3., 4.);;
assert (p (2.5, 7.0) = 0);;
assert (p (1.0, 0.0) = 1);;

let b = zloz ((-1.), 0.) (3., 4.) p;;
assert (b (0.5, 3.) = 2);;
assert (b (0.6, 1.9) = 2);;
assert (b ((-0.99), 3.99) = 2);;
assert (b (2., 3.) = 1);;
assert (b ((-2.), 2.) = 1);;
assert (b (2., 2.) = 0);;
assert (b (100., (-100.)) = 0);;

let c = zloz ((-1.), 0.) ((-1.), 2.) b;;
assert (c ((-1.1), 2.) = 3);;
assert (c ((-2.), 1.) = 2);;
assert (c ((-3.), 2.5) = 2);;
assert (c ((-1.), 2.) = 2);;
assert (c ((-2.), 0.98) = 1);;
assert (c ((-5.), 4.) = 1);;
assert (c ((-5.01), 3.99) = 0);;

let d = zloz ((-1.), 4.) ((-1.), 0.) b;;
assert (d ((-0.5), 3.5) = 3);;
assert (d ((-1.), 2.) = 2);;
assert (d ((-2.), 0.) = 0);;

let d = skladaj [(((-1.), 0.),(3.,4.)) ; (((-1.), 0.),((-1.),4.))] p;;
assert (d ((-7.), 2.) = c ((-7.), 2.));;
assert (d ((-2.), 1.) = c ((-2.), 1.));;
assert (d ((-1.5), 3.8) = c ((-1.5), 3.8));;

let k = kolko (2., 3.) 4.;;
let j = zloz (2., (-1.)) (6., 3.) k;;

assert (j (5., 2.03) = 2);;
assert (j (4., 1.) = 1);;
assert (j (2., 3.) = 1);;
assert (j (2., 7.) = 1);;
assert (j (4., 0.999) = 0);;

let m = zloz (2., (-100.)) (2., 109.) j;;
assert (m (0., 1.03) = 3);;
assert (m (0., 2.) = 3);;
assert (m (0., 1.) = 2);;
assert (m ((-1.5), 3.) = 2);;
assert (m (2., 3.) = 1);;

let n = skladaj [((2., (-1.)), (6., 3.)) ; ((2., (-100.)), (2., 109.))] k;;
assert (m (1., (-1.)) = n (1., (-1.)));;
assert (m (0., 2.) = n (0., 2.));;
assert (m (2., 4.) = n (2., 4.));;
