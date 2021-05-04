type 'a t
val create : int -> 'a -> 'a t
val top : 'a t -> 'a
val pop : 'a t -> 'a
val add : 'a t -> 'a -> int -> unit
