(** input module type to functors that make optimisation modules such as {!module:Owl_opt.D.Adam.Make} *)

module type PT = sig
  (** 
  Example:
  {[
   module Prms = struct
      type 'a t = {a: 'a; b: 'a} [@@deriving prms]
   end
  ]}
 *)

  (** ['a t] is typically defined by the user. *)

  type 'a t

  (** The following functions can be derived using [@@deriving prms] *)

  val map : f:('a -> 'b) -> 'a t -> 'b t
  val map2 : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val iter : f:('a -> unit) -> 'a t -> unit
  val iter2 : f:('a -> 'b -> unit) -> 'a t -> 'b t -> unit
end
