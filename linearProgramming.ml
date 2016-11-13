open Core.Std;;
open PieceGenerator;;


let create_ij_equations n allp =
  let res = Array.make_matrix ~dimx:n ~dimy:n [] in
  List.iter ~f:(fun ((p,listpos) : piece * (piece * (int*int) list) list) ->
      List.iter ~f:(fun ((p1,listpos) : (piece * (int*int) list)) ->
          (List.iter
             ~f:(fun ((posx,posy) : int * int) ->
                 let (p2,_,_) = p1 in
                 List.iter ~f:(fun (u,v) -> res.(u+posx).(v+posy) <- (p,p1,posx,posy)::res.(u+posx).(v+posy)) p2)
             listpos)
        )
        listpos)
    allp; res;;

let write_ij_equations n oc allp =
  let mat = create_ij_equations n allp in
  for i = 0 to n-1 do
    for j=0 to n-1 do
      let init = " <= 1\n" in
      let s = List.fold_right ~f:(fun (p,p1,posx,posy) accu -> let vN = get_variable p p1 (posx,posy) in Printf.sprintf "x%s%s" vN (if accu = init then accu else (" + "^accu))) ~init:init mat.(i).(j) in
      Printf.fprintf oc "%s" s
    done;
    Printf.fprintf oc "\n"
  done;;

let write_ij_equations_gt n oc allp =
  let mat = create_ij_equations n allp in
  for i = 0 to n-1 do
    for j=0 to n-1 do
      let init = " > 0\n" in
      let s = List.fold_right ~f:(fun (p,p1,posx,posy) accu -> let vN = get_variable p p1 (posx,posy) in Printf.sprintf "x%s%s" vN (if accu = init then accu else (" + "^accu))) ~init:init mat.(i).(j) in
      Printf.fprintf oc "%s" s
    done;
    Printf.fprintf oc "\n"
  done;;



let write_p_equations_le (oc : out_channel) allp =
  let res = List.map ~f:(fun (p,listpos) ->
      List.fold_right ~f:(fun (p1,listpos) lres ->
          let init = " <= 1\n" in
          let s1 = 
          List.fold_right ~f:(fun (posx,posy) s -> Printf.sprintf "x%s%s" (get_variable p p1 (posx,posy)) (if s = init then s else (" + "^s))) listpos ~init:init in s1^lres) listpos ~init:"\n"
    ) allp in
  List.iter ~f:(fun s -> Printf.fprintf oc "%s" s) res
;;

let write_p_equations_gt (oc : out_channel) allp =
  let res = List.map ~f:(fun (p,listpos) ->
      List.fold_right ~f:(fun (p1,listpos) lres ->
          let init = " >= 0\n" in
          let s1 = 
          List.fold_right ~f:(fun (posx,posy) s -> Printf.sprintf "x%s%s" (get_variable p p1 (posx,posy)) (if s = init then s else (" + "^s))) listpos ~init:init in s1^lres) listpos ~init:"\n"
    ) allp in
  List.iter ~f:(fun s -> Printf.fprintf oc "%s" s) res
;;


let write_obj_function (oc : out_channel) allp =
  let res = List.map ~f:(fun (p,listpos) ->
      List.fold_right ~f:(fun (p1,listpos) lres ->
          let s1 = 
          List.fold_right ~f:(fun (posx,posy) s -> Printf.sprintf "x%s%s" (get_variable p p1 (posx,posy)) (if s = "" then s else (" + "^s))) listpos ~init:"" in s1^(if lres = "" then lres else " + "^lres)) listpos ~init:""
    ) allp in
  Printf.fprintf oc "Maximize\nobj: ";
  List.iter ~f:(fun s -> Printf.fprintf oc "%s" s) res;
  Printf.fprintf oc  "\n\nSubject To\n"
;;

let flatten allp  =
 List.concat @@  List.concat (List.map ~f:(fun (p,listpos) -> (List.map ~f:(fun (p1,listpos) -> List.map ~f:(fun pos -> (p,p1,pos)) listpos) listpos)) allp);;

let list_all_variables oc allp =
  Printf.fprintf oc "Binary\n";
  let l = flatten allp in
  List.iter ~f:(fun (p,p1,pos) -> Printf.fprintf oc "x%s\n" (get_variable p p1 pos)) l 
  
  (* let n = 5;; *)
(* let res = create_ij_equations n in *)
(* for i = 0 to n-1 do *)
(*   for j = 0 to n-1 do *)
(*     Printf.printf "(i,j) = (%d,%d)%!\n" i j; *)
(*     let pl = res.(i).(j) in *)
(*     List.iter ~f:(fun (p,posx,posy) -> print_bit_matrix @@ get_matrix p n (posx,posy)) pl; *)
(*   done; *)
(* done;; *)
  

let main () =
  let oc = open_out "toto.lp" in
  let n = 5 in
  let allp = generate_all_positions n in
  write_obj_function oc allp;
  write_ij_equations n oc allp;
  write_ij_equations_gt n oc allp;
  write_p_equations_gt oc allp;
  write_p_equations_le oc allp;
  list_all_variables oc allp;
  Printf.fprintf oc "End\n";;

main ();;
