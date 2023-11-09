(* Intentionally left empty until your implementation *)

open Core

module type R = sig
  val int : int -> int
end

module Dist
    (Item : Map.Key)
    (Random : R) : sig
  module Random : R

  type t

  val ngram_find : t -> Item.t list -> Item.t list option
  val to_list : t -> (Item.t list * Item.t list) list
  val make_distribution : Item.t list -> n:int -> t
  val sample_random_sequence : Item.t list -> k:int -> t -> Item.t list
  val sample_random_context : t -> Item.t list

  val most_freq_ngrams :
    Item.t list -> n:int -> k:int -> (Item.t list * int) list
end
with module Random = Random

val sanitize : string -> string
val parse_corpus : string -> string list
