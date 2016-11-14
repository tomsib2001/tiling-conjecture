open Core.Std;;
open PieceGenerator;;

let create_ij_equations size_board allp =
  Printf.eprintf "generating equations for each matrix square...\n%!";
  let res = Array.make_matrix ~dimx:size_board ~dimy:size_board [] in
  List.iter ~f:(fun ((p,listpos) : piece * (piece * (int*int) list) list) ->
      List.iter ~f:(fun ((p1,listpos) : (piece * (int*int) list)) ->
          (List.iter
             ~f:(fun ((posx,posy) : int * int) ->
                 let (p2,_,_) = p1 in
                 List.iter ~f:(fun (u,v) ->
                     (* Printf.eprintf "(u,v) : (%d,%d) and (posx,posy) : (%d,%d) with size_board : %d\n" u v posx posy size_board;  *)
                     res.(u+posx).(v+posy) <- (p,p1,posx,posy)::res.(u+posx).(v+posy)) p2)
             listpos)
        )
        listpos)
    allp;
  Printf.eprintf "Done generating equations for each matrix square.\n%!";
  res;;

let write_ij_equations size_board oc allp =
  Printf.eprintf "Writing equations for each matrix square...\n%!";
  let mat = create_ij_equations size_board allp in
  for i = 0 to size_board-1 do
    Printf.eprintf "Writing equations for line %d...\n%!" i;
    for j=0 to size_board-1 do
      Printf.eprintf "Writing equations for (%d,%d)...\n%!" i j;
      let init = " = 1\n" in
      let s = List.fold_right ~f:(fun (p,p1,posx,posy) accu -> let vN = get_variable p p1 (posx,posy) in Printf.sprintf "x%s%s" vN (if accu = init then accu else (" + "^accu))) ~init:init mat.(i).(j) in
      Printf.fprintf oc "%s" s
    done;
    Printf.fprintf oc "\n"
  done;
  Printf.eprintf "Done writing equations for each matrix square...\n%!";
;;

(* let write_ij_equations_gt size_board oc allp = *)
(*   let mat = create_ij_equations size_board allp in *)
(*   for i = 0 to size_board-1 do *)
(*     for j=0 to size_board-1 do *)
(*       let init = " > 0\n" in *)
(*       let s = List.fold_right ~f:(fun (p,p1,posx,posy) accu -> let vN = get_variable p p1 (posx,posy) in Printf.sprintf "x%s%s" vN (if accu = init then accu else (" + "^accu))) ~init:init mat.(i).(j) in *)
(*       Printf.fprintf oc "%s" s *)
(*     done; *)
(*     Printf.fprintf oc "\n" *)
(*   done;; *)

let write_p_equations_le (oc : out_channel) allp =
  let res = List.map ~f:(fun (p,listpos) ->
      List.fold_right ~f:(fun (p1,listpos) lres ->
          let init = " = 1\n" in
          let s1 = 
          List.fold_right ~f:(fun (posx,posy) s -> Printf.sprintf "x%s%s" (get_variable p p1 (posx,posy)) (if s = init then s else (" + "^s))) listpos ~init:init in s1^lres) listpos ~init:"\n"
    ) allp in
  List.iter ~f:(fun s -> Printf.fprintf oc "%s" s) res
;;

(* let write_p_equations_gt (oc : out_channel) allp = *)
(*   let res = List.map ~f:(fun (p,listpos) -> *)
(*       List.fold_right ~f:(fun (p1,listpos) lres -> *)
(*           let init = " >= 0\n" in *)
(*           let s1 =  *)
(*           List.fold_right ~f:(fun (posx,posy) s -> Printf.sprintf "x%s%s" (get_variable p p1 (posx,posy)) (if s = init then s else (" + "^s))) listpos ~init:init in s1^lres) listpos ~init:"\n" *)
(*     ) allp in *)
(*   List.iter ~f:(fun s -> Printf.fprintf oc "%s" s) res *)
(* ;; *)


let write_obj_function (oc : out_channel) allp =
  Printf.eprintf "Writing objective function...\n%!";
  let res = List.map ~f:(fun (p,listpos) ->
      List.fold_right ~f:(fun (p1,listpos) lres ->
          let s1 = 
          List.fold_right ~f:(fun (posx,posy) s -> Printf.sprintf "x%s%s" (get_variable p p1 (posx,posy)) (if s = "" then s else (" + "^s))) listpos ~init:"" in s1^(if lres = "" then lres else " + "^lres)) listpos ~init:""
    ) allp in
  Printf.fprintf oc "Minimize\nobj: ";
  List.iter ~f:(fun s -> Printf.fprintf oc "%s" s) res;
  Printf.fprintf oc  "\n\nSubject To\n";
  Printf.eprintf "Done writing objective function...\n%!";
;;

let flatten allp  =
  Printf.eprintf "Flattening allp...\n%!";
  let res = 
  List.concat @@
  List.concat @@
  List.map
    ~f:(fun (p,listpos) ->
        (List.map
           ~f:(fun (p1,listpos) ->
               List.map ~f:(fun pos -> (p,p1,pos))
                 listpos)
           listpos))
    allp in
  Printf.eprintf "Done flattening allp.\n%!";
  res
;;

let list_all_variables oc allp =
  Printf.eprintf "Listing all variables...\n%!";
  Printf.fprintf oc "Binary\n";
  let l = flatten allp in
  let res = List.iter ~f:(fun (p,p1,pos) -> Printf.fprintf oc "x%s\n" (get_variable p p1 pos)) l in
  Printf.eprintf "Done listing all variables...\n%!";
  res;;
(* let n = int_of_string Sys.argv.(1);; *)
(* let size_board = size n;; *)
(* let allp = generate_all_positions n in *)
(* let res = create_ij_equations size_board allp in *)
(* for i = 0 to size_board-1 do *)
(*   for j = 0 to size_board-1 do *)
(*     Printf.printf "(i,j) = (%d,%d)%!\n" i j; *)
(*     let pl = res.(i).(j) in *)
(*     List.iter ~f:(fun (_,p,posx,posy) -> print_bit_matrix @@ get_matrix p size_board (posx,posy)) pl; *)
(*   done; *)
(* done;; *)
  

let main () =
  let oc = open_out "toto.lp" in
  let n = int_of_string Sys.argv.(1) in
  let size_board = size n in
  Printf.eprintf "Starting with n=%d and size_board=%d...\n%!" n size_board;
  let allp = generate_all_positions n in
  write_obj_function oc allp;
  write_ij_equations size_board oc allp;
  (* write_ij_equations_gt size_board oc allp; *)
  (* write_p_equations_gt oc allp; *)
  write_p_equations_le oc allp;
  list_all_variables oc allp;
  Printf.fprintf oc "End\n";;

main ();;
