open Core.Std
open PieceGenerator

let flatten allp =
  List.concat @@
  List.concat @@
  List.map
    ~f:(fun (p,listpos) ->
        (List.map
           ~f:(fun (p1,listpos) ->
               List.map ~f:(fun pos -> (p,p1,pos))
                 listpos)
           listpos))
    allp;;

let piece_to_pol (p,_,_) =
  let aux (i,j) = Printf.sprintf "x^%d * y^%d + x^%d * y^%d + x^%d * y^%d + x^%d * y^%d" i j (i+1) j i (j+1) (i+1) (j+1) in
  List.map ~f:aux p;;

let pol_to_string (p : string list) =
  let rec aux res = function
    | [] -> res
    | [h] -> h^res^" mod 2"
    | h::t -> aux (Printf.sprintf "%s + %s" res h) t
  in aux "" p;;

let n = int_of_string Sys.argv.(1) in
let allp = all_pieces n in
let size_board = size n in
let allp_rot = List.map ~f:(fun x -> all_rotations (piece_to_position_list x)) allp in
List.iter ~f:(fun x -> Printf.printf "All rotations of the same piece:\n"; List.iter ~f:(fun x -> print_bit_matrix @@ get_matrix x (size_board) (0,0); print_string (pol_to_string (piece_to_pol x)); Printf.printf "\n") x ; Printf.printf "\n") allp_rot
;;

