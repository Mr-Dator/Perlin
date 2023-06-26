#load "graphics.cma";;
open Graphics;;
Random.full_init;;

let t = 512;;

let randomUnit () = 
	let x = Random.float 1. in
	let sgn = Random.bool () in
	let y = sqrt (1.-.x**2.) in
	if sgn then x,y else x,-.y;;

let scalar u v =
	let x,y = u in
	let a,b = v in
	a*.x +. b*.y;;

let normalize t = 
	let n = Array.length t in
	let max = ref t.(0).(0) in
	let min = ref t.(0).(0) in
	for i = 0 to n-1 do
		for j = 0 to n-1 do
			let u = t.(i).(j) in
			if u > !max then max := u;
			if u < !min then min := u;
		done;
	done;
	
	for i = 0 to n-1 do
		for j = 0 to n-1 do
			let u = t.(i).(j) in
			t.(i).(j) <- (u -. !min) /. (!max -. !min);
		done;
	done;;

let normalizeBis mat = 
	let n = Array.length mat.(0).(0) in
	let max = ref Float.neg_infinity in
	let min = ref Float.infinity in
	for k = 0 to 1 do
	for l = 0 to 1 do
		for i = 0 to n-1 do
			for j = 0 to n-1 do
				let u = mat.(k).(l).(i).(j) in
				if u > !max then max := u;
				if u < !min then min := u;
			done;
		done;
	done;
	done;
	
	for k = 0 to 1 do
	for l = 0 to 1 do
	for i = 0 to n-1 do
		for j = 0 to n-1 do
			let u = mat.(k).(l).(i).(j) in
			mat.(k).(l).(i).(j) <- (u -. !min) /. (!max -. !min);
		done;
	done;
	done;
	done;;

let getShade x = 
	let c =  int_of_float (x *. 255.) in
	rgb c c c;;

let draw t = 
	let n = Array.length t in 
	for i = 0 to n-1 do
		for j = 0 to n-1 do
			set_color (getShade t.(i).(j));
			plot i j;
		done;
	done;;

let drawBis t offsetX offsetY = 
	let n = Array.length t in 
	for i = 0 to n-1 do
		for j = 0 to n-1 do
			set_color (getShade t.(i).(j));
			plot (i + offsetX) (j + offsetY);
		done;
	done;;

let fade t = t *. t *. t *. (t *. (t *. 6. -. 15.) +. 10.);;

let lerp a b t = a +. t*.(b -. a);;

let lerps t1 t2 =
	let n = Array.length t1 in
	let l = Array.make_matrix n n 0. in
	for i = 0 to n-1 do
		for j =0 to n-1 do
			let a,b = t1.(i).(j), t2.(i).(j) in
			l.(i).(j) <- lerp 0.5 a b;
		done;
	done;
	l;;

let initMatrix n n f =
	let m = Array.make_matrix n n (f 0 0) in
	for i = 0 to n-1 do
		for j = 0 to n-1 do
			m.(i).(j) <- f i j;
		done;
	done;
	m;;

let fusMatrix mat =
	let n = Array.length mat.(0) in
	let t = Array.length mat.(0).(0) in
	let l = n*t in
	initMatrix l l (fun i j -> mat.(i/t).(j/t).(i mod t).(j mod t));;

let scaleMatrix x mat = 
	let n = Array.length mat in
	for i = 0 to n-1 do
		for j = 0 to n-1 do
			mat.(i).(j) <- x *. mat.(i).(j)
		done;
	done;
	;;

let rec pow k n = match n with
	0 -> 1
	|1 -> k
	|_ -> k * pow k (n-1)
	;;

let sumMatrix mat1 mat2 =
	let n = Array.length mat1 in
	let r = Array.make_matrix n n 0. in
	for i = 0 to n-1 do
		for j = 0 to n-1 do
			r.(i).(j) <- mat1.(i).(j) +. mat2.(i).(j);
		done;
	done;
	r;;


let perlinPartiel n u1 u2 u3 u4 =

let screen = Array.make_matrix n n 0. in

let l = float_of_int n in

for i = 0 to n-1 do 
	for j = 0 to n-1 do
		let x,y = float_of_int i, float_of_int j in
		
		let sx = fade (x /. l) in
		let sy = fade (y /. l) in
		
		let x1 = scalar (x,y) u1 in
		let x2 = scalar (x -. l, y) u2 in
		let y1 = lerp x1 x2 sx in
		
		let x3 = scalar (x, y -. l) u3 in
		let x4 = scalar (x -.l, y -. l) u4 in
		let y2 = lerp x3 x4 sx in
		
		screen.(i).(j) <- lerp y1 y2 sy;
		
	done;
done;

screen;;


let perlinOctave octaves persistence =
	
	let p = pow 2 (octaves - 1) in
	let amplitude = ref 1. in
	let unitVectors = initMatrix (p + 1) (p + 1) (fun i j -> randomUnit ()) in (*a la ieme octave on a 2^(i-1) carres par ligne*)
	let octave = ref 1 in
	let result = ref (Array.make_matrix t t 0.) in
	
	let screenOctave = Array.make octaves [||] in
	
	for k = 1 to octaves do
		
		let taille = t / (!octave) in
		let screenMatrix = Array.make_matrix (!octave) (!octave) [||] in
		
		for i = 0 to !octave - 1 do
			for j = 0 to !octave - 1 do
			
				let d = p / (!octave) in
				
				let u1 = unitVectors.(i * d).(j * d) in
				let u2 = unitVectors.((i+1) * d).(j * d) in
				let u3 = unitVectors.(i * d).((j+1) * d) in
				let u4 = unitVectors.((i+1) * d).((j+1) * d) in
				
				screenMatrix.(i).(j) <- perlinPartiel taille u1 u2 u3 u4;
			done;
		done;
		
		let screen = fusMatrix screenMatrix in
		scaleMatrix (!amplitude) screen;
		screenOctave.(k-1) <- screen;
		result := sumMatrix !result screen;
		
		octave := 2 * (!octave);
		amplitude := !amplitude *. persistence;
	done;
	
	normalize !result;
	!result;;


open_graph(" " ^ string_of_int t ^"x" ^ string_of_int t);
draw (perlinOctave 9 0.95);;
