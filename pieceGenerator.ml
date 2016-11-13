open Core.Std;;

let irange a b = List.range (a) (b+1);;

(* remaining = remaining height *)
let all_next remaining (a,b) : (int*int) list =
  List.concat
    (List.map
       ~f:(fun x ->
	 (List.map ~f:(fun y -> (x,y)) (irange b remaining))
       )
       (irange a b));;

(* all_next 2 (0,1);; *)

assert(all_next 0 (0,0) = [0,0]);;
assert(all_next 2 (0,1) = [0,1;0,2;1,1;1,2]);;

all_next 1 (0,0);;

(* irange 0 1;; *)
(* all_next 2 (0,0);;   *)

let all_pieces n =
  let rec aux (a,b) remaining : (int * int) list list =
    let next = all_next remaining (a,b) in
    let all_suffix =
      (* if remaining= 0 then [[]] else *)
      List.map
	~f:(fun (c,d) -> ((c,d),if remaining > d then aux (c,d) (remaining-1) else [[]]))
	next
    in
    let before_concat =
      List.map
	~f:(fun ((c,d),ll) ->
	  List.map
	    ~f:(fun l -> (c,d)::l)
	    ll
	)
	all_suffix
    in
    List.concat before_concat
  in aux (0,0) (n-2);;

let get_height_width (piece : (int*int) list) =
  let width = List.length piece in
  let height = List.fold_left piece ~init:0 ~f:(fun maxi (_,y) -> max (y+1) maxi) in (height,width);;

let piece_to_matrix piece=
  let (height,width) = get_height_width piece in
  let res = Array.make_matrix ~dimx:width ~dimy:height false in
  List.iteri
    piece
    ~f:(fun i (a,b) ->
      for j = a to b do
	res.(i).(j) <- true
      done);
  res;;

type piece = ((int * int) list)*int*int

let piece_to_position_list piece : piece =
  let (height,width) = get_height_width piece in
  let res = List.foldi
    piece
    ~f:(fun i accu (a,b) ->
        accu@(List.map ~f:(fun x -> (i,x)) (List.range a (b+1))))
    ~init:[] in (res,height,width);;


let all4_triples : piece list = List.map ~f:piece_to_position_list (all_pieces 4);;

let get_min_altitude (p : piece) =
  let (p,_,_) = p in
    List.fold_right p ~f:(fun (a,_) m -> min a m) ~init:1000;;

let get_max_altitude (p : piece) = 
  let (p,_,_) = p in
  List.fold_right p ~f:(fun (a,_) m -> max a m) ~init:(-1);;
  
let get_min_xoffset (p : piece) = 
  let (p,_,_) = p in
    List.fold_right p ~f:(fun (_,b) m -> min b m) ~init:1000;;

let get_max_xoffset (p : piece) = 
  let (p,_,_) = p in
    List.fold_right p ~f:(fun (_,b) m -> max b m) ~init:(-1);;

let rotate_pi4 (piece_pos : piece) : piece = let (p,h,w) = piece_pos in
  let temp = (List.map ~f:(fun (x,y) -> (y,w-1-x)) p) in
  let alt = get_min_altitude (temp,w,h) in
  let xoffset = get_min_xoffset (temp,w,h) in
  (List.sort ~cmp:(Pervasives.compare) (List.map ~f:(fun (a,b) -> (a-xoffset,b-alt)) temp),w,h)
;;


(List.nth_exn all4_triples 4);; 
rotate_pi4 (List.nth_exn all4_triples 4);; 
rotate_pi4 @@ rotate_pi4 (List.nth_exn all4_triples 4);; 
rotate_pi4 @@ rotate_pi4 @@ rotate_pi4 (List.nth_exn all4_triples 4);; 

let rec iter n f x = match n with
  | 0 -> x
  | n -> f (iter (n-1) f x);;

let all_rotations (piece_pos : piece) =
  List.fold_right
    ~f:(fun i accu -> let temp = iter i (fun x -> rotate_pi4 x) piece_pos in
         if List.mem accu temp
         then
           accu
         else temp::accu)
    [0;1;2;3]
    ~init:[]
;;

let all_prods l1 l2 =
  List.concat (List.map ~f:(fun x -> (List.map ~f:(fun y -> (x,y)) l2 )) l1);;

let all_positions_translation n (p : piece) =
  let (_,h,w) = p in
  all_prods (List.range 0 (n-w+1)) (List.range 0 (n-h+1));;

let all_positions (n : int) (piece : piece) = let res = (List.map ~f:(fun p -> (p,all_positions_translation n p)) (all_rotations piece)) in
  res
;;

List.map ~f:(fun p -> (p,all_positions 4 p)) all4_triples;;

let generate_all_positions n =
  let all_triples = List.map ~f:piece_to_position_list (all_pieces n) in
  List.map ~f:(fun p -> (p,all_positions n p)) all_triples;;

generate_all_positions 4;;

let sob b = if b then "1" else "0"

let print_bit_matrix (mat : bool array array) =
  let m = Array.length mat in
  let n = Array.fold_right ~f:(fun x (y : int) -> max (Array.length x) y) mat ~init:0 in
  let bnd = (max m n) in
    Printf.printf "\n";
  for j = bnd-1 downto 0  do
    for i = 0 to bnd-1 do
      if (i < m && j < Array.length mat.(i)) then
        Printf.printf "%s" (sob mat.(i).(j)) else
        Printf.printf "0"
    done;
    Printf.printf "\n%!";
  done;
  Printf.printf "\n%!";;

let get_matrix (p : piece) n (offx,offy) =
  (* let m = get_max_altitude p in *)
  (* let n = get_max_altitude p in *)
  let (p,_,_) = p in
  let res = Array.make_matrix ~dimx:n ~dimy:n false in
  List.iter ~f:(fun (i,j) -> res.(offx+i).(offy+j) <- true) p;
  res;;

(* List.map ~f:(fun p -> print_bit_matrix @@ get_matrix p 4) (all4_triples);; *)


(* let n = 4;; *)
(* List.iter ~f:(fun (p,l) -> Printf.printf "piece :"; (print_bit_matrix @@ get_matrix p n); *)
(*                List.iter ~f:(fun (p1,l1) -> *)
(*                    let (p1,h,w) = p1 in  *)
(*                    List.iter ~f:(fun (posx,posy) -> Printf.printf "considering positions (%d,%d)" posx posy;print_bit_matrix @@ get_matrix (List.map ~f:(fun (u,v) -> (u+posy,v+posx)) p1,h,w) n) l1) l) (generate_all_positions 4);; *)


let get_variable (p : piece) (p1 : piece) ((x,y) : int * int) =
  let (p1,_,_) = p1 in
  let (p,_,_) = p in
  List.fold_right ~f:(fun (u,v) b -> Printf.sprintf"%d%d%s" u v b) (p@p1) ~init:(Printf.sprintf"%d%d" x y);;



(* get_variable (List.nth_exn all4_triples 4) (0,0);; *)


(* List.map ~f:piece_to_matrix (all_pieces 4);; *)

(* let sob b = if b then "1" else "0"     *)

(* let print_bit_matrix (mat : bool array array) = *)
(*   let m = Array.length mat in *)
(*   let n = Array.fold_right ~f:(fun x (y : int) -> max (Array.length x) y) mat ~init:0 in *)
(*   let bnd = (max m n) in  *)
(*   for j = bnd-1 downto 0  do *)
(*     for i = 0 to bnd-1 do *)
(*       if (i < m && j < Array.length mat.(i)) then *)
(*         Printf.printf "%s" (sob mat.(i).(j)) else *)
(*         Printf.printf "0" *)
(*     done; *)
(*     Printf.printf "\n"; *)
(*   done; *)
(*   Printf.printf "\n";; *)

(* let get_pos_list mat = *)
(*   Array.foldi ~f:(fun i (x : (int*int) list) vect -> *)
(*       Array.foldi vect ~f:(fun j y b -> if b then (i,j)::y else y) ~init:x) mat ~init:[];; *)

(* let all4 = (all_pieces 4) *)
(* let all_mats4 = (List.map ~f:piece_to_matrix all4);; *)


(* get_pos_list (piece_to_matrix (List.nth_exn all4 3));; *)


(* (\* List.iter ~f:(print_bit_matrix) (List.map ~f:piece_to_matrix (all_pieces 4));; *\) *)


(* (\* all_prods [1;2] [4;5];; *\) *)


(* let rotate_pi4 piece_pos h w = List.map ~f:(fun (x,y) -> (y,w-1-x)) piece_pos *)


(* (\* let rotate_pi4 piece = Array.map ~f:(fun x -> Array.map ~f:(fun (a,b) -> (b,a)) x) piece;; *\) *)

(* let all_rotations piece_pos h w = (List.map ~f:(fun i -> iter i (fun x -> rotate_pi4 x h w) piece_pos) [0;1;2;3]);; *)

(* all_rotations (get_pos_list (piece_to_matrix (List.nth_exn all4 0))) 2 2;; *)



(* all_rotations (piece_to_matrix (List.nth_exn all4 0));; *)

(* let all_positions_translation n piece = let (h,w) = get_height_width piece in *)
(* all_prods (List.range 0 (n-h+1)) (List.range 0 (n-w+1));; *)

(* all_positions_translation 4 (List.nth_exn all4 0);; *)

(* piece_to_matrix (List.nth_exn all4 0);; *)

(* let all_positions n piece = List.concat (List.map (all_positions_translation n) (all_rotations piece)) *)
