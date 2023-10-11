(* *********************************************************** *)
(* ********************** P A R T I ************************** *)
(* *********************************************************** *)

open Core

(*
  Your implementation goes here. We provide just a little bit of starter code to give you
  the syntax, but you're expected to do the rest without modifying the .mli files.

  See finite_group.mli for the details.
*)

module type S = sig
  type t [@@deriving compare, sexp]

  val id : t
  val op : t -> t -> t
  val inverse : t -> t
  val of_int : int -> t option
  val to_int : t -> int
end

module type Params = sig
  val op : int -> int -> int
  val n : int
end

module Make (P : Params) : S = struct
  type t = int [@@deriving compare, sexp]

  let op (e1 : t) (e2 : t) : t = P.op e1 e2 mod P.n
  let elements = List.init P.n ~f:Fn.id (* Local function to initialize group *)

  (* Fold over elements for each element to find id. First fold value is initialized
     to -1 because -1 is an invalid id. Even though fold adds all elements, expect
     to find only one id since there is only one id in the group by its definition. *)
  let id : t =
    List.fold_left elements ~init:(-1) ~f:(fun id e1 ->
        if
          List.fold_left elements ~init:0 ~f:(fun matching e2 ->
              if op e1 e2 = e2 && op e2 e1 = e2 then matching + 1 else matching)
          = P.n
        then e1
        else id)

  let inverse (e1 : t) : t =
    List.fold elements ~init:(-1) ~f:(fun inv e2 ->
        if op e1 e2 = id && op e2 e1 = id then e2 else inv)

  let of_int (e : int) : t option = if e >= 0 && e < P.n then Some e else None
  let to_int (e : t) : int = e
end

module Z5_params : Params = struct
  let op (e1 : int) (e2 : int) : int = e1 + e2
  let n : int = 5
end

module Z5_add : S = Make (Z5_params)

module Memoize (G : S) : S with type t = G.t = struct
  include G
  module MemMap = Map.Make (G)

  let map : t MemMap.t =
    let rec add_to_map (map : t MemMap.t) (e : int) =
      match G.of_int e with
      | None -> map
      | Some k -> add_to_map (Map.add_exn map ~key:k ~data:(G.inverse k)) (e + 1)
    in
    add_to_map MemMap.empty 0

  let inverse (e : t) : t = Map.find_exn map e
end
