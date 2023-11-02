(* Intentionally left empty until your implementation *)

open Core

module Dist (Item : Map.Key) : sig 
  type t 
  val make_distribution : Item.t list -> n:int -> t
  val sample_random_sequence : Item.t list -> k:int -> t -> Item.t list
  val ngram_find : t ->  Item.t list -> Item.t list option
end