type 'a t

val empty : 'a t

val is_empty : 'a t -> bool
val insert : 'a -> 'a t -> 'a t
val remove : 'a -> 'a t -> 'a t
val remove_all : 'a -> 'a t -> 'a t

val filter : ('a -> bool) -> 'a t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val fold_right : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

val union : 'a t -> 'a t -> 'a t
val equals : 'a t -> 'a t -> bool