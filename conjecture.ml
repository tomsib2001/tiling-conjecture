open Core.Std;;
open Combine;;

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

(* let ap4 = all_pieces 4;; *)
(* assert(List.length (all_pieces 4) = 5);; *)

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

(* List.map ap4 ~f:piece_to_matrix  *)

let make_pieces n = 
  let ap = all_pieces n in
  List.map ap
  ~f:Tiling.Tile.(fun x -> create ~s:(Srotations) ~m:Mone (Tiling.Pattern.create (piece_to_matrix x)));;


make_pieces 4;;

let make_pattern n = 
  let size = (1 lsl (n-2)) in
  Tiling.Pattern.create (Array.make_matrix ~dimx:size ~dimy:size true);;
 

let create_problem n = Tiling.create_problem (make_pattern n) (make_pieces n);;
 
(* Tiling.print_problem Format.std_formatter (create_problem 4);; *)

(* let create_emc n = let pb = (create_problem n) in (pb,Tiling.ToEMC.make pb);; *)

(* let solve n =  *)
(*   let (pb,emc) = create_emc n in *)
(*   (pb,Emc.D.find_solution (Emc.D.create ~primary:emc.Tiling.ToEMC.primary emc.Tiling.ToEMC.matrix));; *)

let get_mat_dims m =
  let r = Array.length m in
  let c = Array.length (m.(0)) in
  (r,c);;

let print_solution_D n = 
  let pb = (create_problem n) in
  let emc = Tiling.ToEMC.make pb in
  let (r,c) = get_mat_dims emc.Tiling.ToEMC.matrix in
  Printf.printf "dimensions: %d %d\n%!" r c;
  let d = (Emc.D.create ~primary:emc.Tiling.ToEMC.primary emc.Tiling.ToEMC.matrix) in
  let solution = Emc.D.find_solution d in
  print_newline();
  Tiling.ToEMC.print_solution_to_svg_file (Printf.sprintf "toto%d.svg" n) pb emc solution ~width:1000 ~height:1000;
  flush stdout;;
  (* let c = Emc.D.count_solutions d in *)
  (* Printf.printf "number of solutions: %d\n%!" c;; *)

(* let sat_solver = (fun ~input ~output -> sprintf "minisat %s %s" input output);; *)

(* let print_solution_Sat n = *)
(*   let pb = (create_problem n) in *)
(*   let emc = Tiling.ToEMC.make pb in *)
(*   let (r,c) = get_mat_dims emc.Tiling.ToEMC.matrix in *)
(*   Printf.printf "dimensions: %d %d\n%!" r c; *)
(*   let solution = Emc.Sat.find_solution sat_solver (Emc.Sat.create ~primary:emc.Tiling.ToEMC.primary emc.Tiling.ToEMC.matrix) in *)
(*   print_newline(); *)
(* Tiling.ToEMC.print_solution_to_svg_file (Printf.sprintf "toto%d.svg" n) pb emc solution ~width:1000 ~height:1000;; *)


print_solution_D (int_of_string (Sys.argv.(1)));;


(* Emc.D.find_solution (create_emc 4);; *)
