module type PT = sig
  type 'a t

  val map : f:('a -> 'b) -> 'a t -> 'b t
  val map2 : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val iter : f:('a -> unit) -> 'a t -> unit
  val iter2 : f:('a -> 'b -> unit) -> 'a t -> 'b t -> unit
end
