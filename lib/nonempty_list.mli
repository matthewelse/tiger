open! Core

type 'a t = ( :: ) of 'a * 'a list

val find_map : 'a t -> f:('a -> 'b option) -> 'b option
val hd : 'a t -> 'a
val cons : 'a t -> 'a -> 'a t
