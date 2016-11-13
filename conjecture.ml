open Core.Std;;
open Combine;;
open PieceGenerator;;
(* List.map ap4 ~f:piece_to_matrix  *)

let two_bars n =
  let horizontal_bar = List.map (List.range 0 (n-1)) ~f:(fun _ -> (0,0)) in
  let vertical_bar = [(0,n-2)] in
  (horizontal_bar,vertical_bar);;

let make_pieces n ~remBars:remBars =
  let ap = ref (all_pieces n) in
  Format.printf "number of pieces: %d%!\n" (List.length !ap);
  if remBars then
    begin
      assert(n>=4);
      Format.printf "removing two bars from pieces...%!\n";
      let (x,y) = two_bars n in
      (* let before = List.length !ap in *)
      ap := List.filter !ap ~f:(fun i -> i <> x && i <> y);
      (* assert(List.length !ap = before - 2); *)
      Format.printf "Done removing two bars from pieces.%!\n";
    end;
  List.map !ap
  ~f:Tiling.Tile.(fun x -> create ~s:(Spositive) ~m:Mone (Tiling.Pattern.create (piece_to_matrix x)));;


make_pieces 4;;

let make_pattern n ~remBars:remBars =
  let size = (1 lsl (n-2)) in
  let res = (Array.make_matrix ~dimx:size ~dimy:size true) in
  if remBars then begin
    Format.printf "modifying matrix to remove two bars...%!\n";
    for i = 0 to n-2 do
      res.(0).(i) <- false;
      res.(size-1).(i) <- false;
    done;
    Format.printf "Done modifying matrix to remove two bars.%!\n";
  end;
  Tiling.Pattern.create res
;;


let create_problem n ~remBars:remBars = Tiling.Problem.create (make_pattern n ~remBars:remBars) (make_pieces n ~remBars:remBars);;

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
  Format.printf "creating problem...%!\n";
  let pb = (create_problem ~remBars:remBars n) in
  Format.printf "Done creating problem.%!\n";
  (* Tiling.Problem.print Format.std_formatter pb; *)
  Format.printf "creating emc...%!\n";
  let emc = make pb in
  (* Format.printf "%a\n" print_emc emc; *)
  
    Format.printf "Done creating emc.%!\n";
  (* print_emc Format.std_formatter emc; *)
  Format.printf "dimensions: %d %d\n%!" (Array.length emc.emc) emc.columns;
  Format.printf "Launching create_sparse...%!\n";  
  let d =
    Emc.D.create_sparse ~columns:emc.columns ~primary:emc.primary emc.emc in
  Format.printf "Done doing create_sparse.%!\n";  

  let solution = Emc.D.find_solution d in
  print_newline();
  let filename = (Format.sprintf "toto%d.svg" n) in
  let oc = Pervasives.open_out filename in
  let decode_solution d s = List.map ~f:(fun r -> d.(r)) s in
  let fmt = Format.formatter_of_out_channel oc in
  Tiling.FourColoring.print_solution_to_svg_four_colors fmt
    ~width:1000 ~height:1000 pb (decode_solution emc.tiles solution);
  Pervasives.close_out oc;

  flush stdout;;
  (* let c = Emc.D.count_solutions d in *)
  (* Format.printf "number of solutions: %d\n%!" c; emc;; *)

let count_solution_D n ~remBars:remBars =
  let open Tiling.Problem.ToEMC in
  Format.printf "creating problem...%!\n";
  let pb = (create_problem ~remBars:remBars n) in
  Tiling.Problem.print Format.std_formatter pb;
  let emc = make pb in
  (* Format.printf "%a\n" print_emc emc; *)
  Format.printf "dimensions: %d %d\n@." (Array.length emc.emc) emc.columns;
  assert(false);
  let d =
    Emc.D.create_sparse ~columns:emc.columns ~primary:emc.primary emc.emc in
  Format.printf "Done creating problem.@.\n";
  (* let solution = Emc.D.find_solution d in *)
  (* print_newline(); *)
  (* print_solution_to_svg_file (Format.sprintf "toto%d.svg" n) pb emc solution ~width:1000 ~height:1000; *)
  (* flush stdout;; *)
  let c = Emc.D.count_solutions d in
  Format.printf "number of solutions: %d\n%!" c; emc;;


(* let sat_solver = (fun ~input ~output -> sprintf "minisat %s %s" input output);; *)

(* let print_solution_Sat n = *)
(*   let pb = (create_problem n) in *)
(*   let emc = Tiling.ToEMC.make pb in *)
(*   let (r,c) = get_mat_dims emc.Tiling.ToEMC.matrix in *)
(*   Format.printf "dimensions: %d %d\n%!" r c; *)
(*   let solution = Emc.Sat.find_solution sat_solver (Emc.Sat.create ~primary:emc.Tiling.ToEMC.primary emc.Tiling.ToEMC.matrix) in *)
(*   print_newline(); *)
(* Tiling.ToEMC.print_solution_to_svg_file (Format.sprintf "toto%d.svg" n) pb emc solution ~width:1000 ~height:1000;; *)

let i = try (int_of_string (Sys.argv.(1))) with
  | _ -> 4;;

print_solution_D i ~remBars:false ;;


(* Emc.D.find_solution (create_emc 4);; *)
