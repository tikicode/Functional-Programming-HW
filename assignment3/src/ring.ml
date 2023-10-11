open Core

module type S = sig
  type t

  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val of_string : string -> t option
  val to_string : t -> string
end

module Make_modular_ring (P : sig
  val n : int
end) : S = struct
  type t = int

  let ( + ) (e1 : t) (e2 : t) : t = (e1 + e2) mod P.n
  let ( * ) (e1 : t) (e2 : t) : t = e1 * e2 mod P.n

  let of_string (str : string) : t option =
    match int_of_string_opt str with
    | None -> None
    | Some x -> if x >= 0 && x < P.n then Some x else None

  let to_string (e : int) : string = string_of_int e
end

module Z4 = Make_modular_ring (struct
  let n = 4
end)
