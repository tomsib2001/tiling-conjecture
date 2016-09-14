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

let ap4 = all_pieces 4;;
(* assert(List.length (all_pieces 4) = 5);; *)

let two_bars n =
  let horizontal_bar = List.map (List.range 0 (n-1)) ~f:(fun _ -> (0,0)) in
  let vertical_bar = [(0,n-2)] in
  (horizontal_bar,vertical_bar);;

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

let make_pieces n ~remBars:remBars =
  let ap = ref (all_pieces n) in
  Printf.printf "number of pieces: %d%!\n" (List.length !ap);
  if remBars then
    begin
      assert(n>=4);
      Printf.printf "removing two bars from pieces...%!\n";
      let (x,y) = two_bars n in
      (* let before = List.length !ap in *)
      ap := List.filter !ap ~f:(fun i -> i <> x && i <> y);
      (* assert(List.length !ap = before - 2); *)
      Printf.printf "Done removing two bars from pieces.%!\n";
    end;
  List.map !ap
  ~f:Tiling.Tile.(fun x -> create ~s:(Tiling.Spositive) ~m:Mone (Tiling.Pattern.create (piece_to_matrix x)));;


make_pieces 4;;

let make_pattern n ~remBars:remBars =
  let size = (1 lsl (n-2)) in
  let res = (Array.make_matrix ~dimx:size ~dimy:size true) in
  if remBars then begin
    Printf.printf "modifying matrix to remove two bars...%!\n";
    for i = 0 to n-2 do
      res.(0).(i) <- false;
      res.(size-1).(i) <- false;
    done;
    Printf.printf "Done modifying matrix to remove two bars.%!\n";
  end;
  Tiling.Pattern.create res
;;


let create_problem n ~remBars:remBars = Tiling.Problem.create ~name:"Tiling Conjecture" (make_pattern n ~remBars:remBars) (make_pieces n ~remBars:remBars);;

(* Tiling.print_problem Format.std_formatter (create_problem 4);; *)

(* let create_emc n = let pb = (create_problem n) in (pb,Tiling.ToEMC.make pb);; *)

(* let solve n =  *)
(*   let (pb,emc) = create_emc n in *)
(*   (pb,Emc.D.find_solution (Emc.D.create ~primary:emc.Tiling.ToEMC.primary emc.Tiling.ToEMC.matrix));; *)

let get_mat_dims m =
  let r = Array.length m in
  let c = Array.length (m.(0)) in
  (r,c);;

let print_solution_D n ~remBars:remBars =
  let open Tiling.Problem.ToEMC in
  Printf.printf "creating problem...%!\n";
  let pb = (create_problem ~remBars:remBars n) in
  Printf.printf "Done creating problem.%!\n";
  (* Tiling.Problem.print Format.std_formatter pb; *)
  Printf.printf "creating emc...%!\n";
  let emc = make pb in
    Printf.printf "Done creating emc.%!\n";
  (* print_emc Format.std_formatter emc; *)
  Printf.printf "dimensions: %d %d\n%!" (Array.length emc.emc) emc.columns;
  Printf.printf "Launching create_sparse...%!\n";  
  let d =
    Emc.D.create_sparse ~columns:emc.columns ~primary:emc.primary emc.emc in
  Printf.printf "Done doing create_sparse.%!\n";  

  let solution = Emc.D.find_solution d in
  if (solution = []) then Format.printf "Empty solution!%!\n";
  print_newline();
  let filename = (Printf.sprintf "toto%d.svg" n) in
  let oc = Pervasives.open_out filename in
  let decode_solution d s = List.map ~f:(fun r -> d.(r)) s in
  let fmt = Format.formatter_of_out_channel oc in
  Tiling.FourColoring.print_solution_to_svg_four_colors fmt
    ~width:1000 ~height:1000 pb (decode_solution emc.tiles solution);
  Pervasives.close_out oc;
  print_solution_to_svg_file (Printf.sprintf "toto%d_classic.svg" n) pb emc solution ~width:1000 ~height:1000;
  flush stdout;;
  (* let c = Emc.D.count_solutions d in *)
  (* Printf.printf "number of solutions: %d\n%!" c; emc;; *)

(* let count_solution_D n ~remBars:remBars = *)
(*   let open Tiling.Problem.ToEMC in *)
(*   Printf.printf "creating problem...%!\n"; *)
(*   let pb = (create_problem ~remBars:remBars n) in *)
(*   (\* Tiling.Problem.print Format.std_formatter pb; *\) *)
(*   let emc = make pb in *)
(*   (\* print_emc Format.std_formatter emc; *\) *)
(*   Printf.printf "dimensions: %d %d\n%!" (Array.length emc.emc) emc.columns; *)
(*   let d = *)
(*     Emc.D.create_sparse ~columns:emc.columns ~primary:emc.primary emc.emc in *)
(*   Printf.printf "Done creating problem.%!\n"; *)
(*   (\* let solution = Emc.D.find_solution d in *\) *)
(*   (\* print_newline(); *\) *)
(*   (\* print_solution_to_svg_file (Printf.sprintf "toto%d.svg" n) pb emc solution ~width:1000 ~height:1000; *\) *)
(*   (\* flush stdout;; *\) *)
(*   let c = Emc.D.count_solutions d in *)
(*   Printf.printf "number of solutions: %d\n%!" c; emc;; *)


(* let sat_solver = (fun ~input ~output -> sprintf "minisat %s %s" input output);; *)

(* let print_solution_Sat n = *)
(*   let pb = (create_problem n) in *)
(*   let emc = Tiling.ToEMC.make pb in *)
(*   let (r,c) = get_mat_dims emc.Tiling.ToEMC.matrix in *)
(*   Printf.printf "dimensions: %d %d\n%!" r c; *)
(*   let solution = Emc.Sat.find_solution sat_solver (Emc.Sat.create ~primary:emc.Tiling.ToEMC.primary emc.Tiling.ToEMC.matrix) in *)
(*   print_newline(); *)
(* Tiling.ToEMC.print_solution_to_svg_file (Printf.sprintf "toto%d.svg" n) pb emc solution ~width:1000 ~height:1000;; *)

let i = try (int_of_string (Sys.argv.(1))) with
  | _ -> 4;;

print_solution_D i ~remBars:false ;;


(* Emc.D.find_solution (create_emc 4);; *)
