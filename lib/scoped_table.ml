open! Core

type ('k, 'v) t = ('k, 'v) Hashtbl.t Nonempty_list.t

let create (type k) (module Key : Hashtbl.Key with type t = k) : _ t =
  [ Hashtbl.create (module Key) ]
;;

let find (t : _ t) ident =
  Nonempty_list.find_map t ~f:(fun table -> Hashtbl.find table ident)
;;

let set t ~key ~data = Hashtbl.set (Nonempty_list.hd t) ~key ~data

let enter_scope (t : _ t) =
  let new_table = Hashtbl.create (Hashtbl.hashable_s (Nonempty_list.hd t)) in
  Nonempty_list.cons t new_table
;;

let leave_scope_exn (t : _ t) : _ t =
  match t with
  | [ _ ] -> failwith "Cannot leave the outermost scope"
  | _ :: x :: xs -> x :: xs
;;
