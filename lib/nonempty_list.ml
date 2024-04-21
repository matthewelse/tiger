open! Core

type 'a t = ( :: ) of 'a * 'a list

let find_map (hd :: tl) ~f =
  match f hd with
  | None -> List.find_map tl ~f
  | Some x -> Some x
;;

let hd (hd :: _) = hd
let cons (hd' :: tl) hd = hd :: hd' :: tl
