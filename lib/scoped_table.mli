open! Core

type ('k, 'v) t

val create : (module Hashtbl.Key with type t = 'k) -> ('k, _) t

(** Finds the value of a variable in any applicable scope. *)
val find : ('a, 'b) t -> 'a -> 'b option

(** Sets the value of a variable in the current scope. *)
val set : ('a, 'b) t -> key:'a -> data:'b -> unit

val enter_scope : ('a, 'b) t -> ('a, 'b) t

(** Raises if you try to leave the outermost scope. *)
val leave_scope_exn : ('a, 'b) t -> ('a, 'b) t
