open Core

module type Data = sig
  include Ring.S

  val next : string -> (string * t) option
end

module type Eval = sig
  type t

  val eval : string -> (t, string) result
end

module Make_data (Ring : Ring.S) : Data with type t = Ring.t = struct
  include Ring

  let check_seperator (c : char) : bool =
    (not (Char.is_digit c))
    && (not (Char.( = ) c '/'))
    && not (Char.( = ) c '-')

  let check_op (c : char) : bool = Char.( = ) c '+' || Char.( = ) c '*'

  let check_max_munch (value : string) (is_valid : bool) : bool =
    match Ring.of_string value with None -> is_valid | Some _ -> false

  let check_is_valid (value : string) : bool =
    match Ring.of_string value with None -> false | Some _ -> true

  let next (str : string) : (string * t) option =
    let rec munch_value (s : string) (value : string) : string * string =
      let is_valid = check_is_valid value in
      if String.length s = 0 || check_seperator s.[0] then (s, value)
      else if check_max_munch (value ^ String.make 1 s.[0]) is_valid then
        (s, value)
      else
        munch_value
          (String.sub s ~pos:1 ~len:(String.length s - 1))
          (value ^ String.make 1 s.[0])
    in
    if String.length str = 0 then None
    else if check_op str.[0] then None
    else
      let rem_s, next_val = munch_value str "" in
      if String.( = ) next_val "" then None
      else
        match Ring.of_string next_val with
        | None -> None
        | Some v -> Some (rem_s, v)
end

module Make_eval (Data : Data) : Eval with type t = Data.t = struct
  type t = Data.t

  let check_op (c : char) : bool = Char.( = ) c '+' || Char.( = ) c '*'

  let compute (val_acc : t list) (op : char) : t list option =
    match val_acc with
    | e1 :: e2 :: tl ->
        if Char.( = ) op '+' then Some (Data.( + ) e1 e2 :: tl)
        else Some (Data.( * ) e1 e2 :: tl)
    | _ -> None

  let eval (str : string) : (t, string) result =
    let rec parse_input (s : string) (val_acc : t list) (list_len : int) :
        (t, string) result =
      if String.length s = 0 then
        if list_len > 1 then Error "unmatched"
        else match val_acc with v :: _ -> Ok v | [] -> Error "unmatched"
      else
        match Data.next s with
        | Some (next_s, value) ->
            parse_input next_s (value :: val_acc) (list_len + 1)
        | None -> (
            let next_s = String.sub s ~pos:1 ~len:(String.length s - 1) in
            match s.[0] with
            | ch when check_op ch -> (
                match compute val_acc ch with
                | Some rem_acc -> parse_input next_s rem_acc (list_len - 1)
                | None -> Error "unmatched")
            | ch when Char.is_whitespace ch ->
                parse_input next_s val_acc list_len
            | _ -> Error "illegal character")
    in
    parse_input str [] 0
end

module Z4_data = Make_data (Ring.Z4)

module Int_data = Make_data (struct
  type t = int

  let ( + ) (e1 : t) (e2 : t) = e1 + e2
  let ( * ) (e1 : t) (e2 : t) = e1 * e2
  let of_string (e : string) = int_of_string_opt e
  let to_string (e : t) = string_of_int e
end)

module Rat_data = Make_data (struct
  type t = int * int

  let rec gcd (e1 : int) (e2 : int) : int =
    if e1 < e2 then gcd e2 e1 else if e2 = 0 then e1 else gcd e2 (e1 % e2)

  let reduce ((n, d) : t) : t =
    let common = if n >= 0 then gcd n d else gcd (-n) d in
    if d = 1 then (n, d) else (Int.( / ) n common, Int.( / ) d common)

  let ( + ) ((e1n, e1d) : t) ((e2n, e2d) : t) : t =
    reduce ((e1n * e2d) + (e2n * e1d), e1d * e2d)

  let ( * ) ((e1n, e1d) : t) ((e2n, e2d) : t) : t = reduce (e1n * e2n, e1d * e2d)

  let of_string (str : string) : t option =
    let splits = String.split ~on:'/' str in
    if List.length splits <> 2 then None
    else
      match String.split ~on:'/' str with
      | n :: d :: _ -> (
          try
            let num = int_of_string n in
            let den = int_of_string d in
            if den > 0 then Some (reduce (num, den)) else None
          with _ -> None)
      | _ -> None

  let to_string ((n, d) : t) : string = string_of_int n ^ "/" ^ string_of_int d
end)

module Z4_eval = Make_eval (Z4_data)
module Int_eval = Make_eval (Int_data)
module Rat_eval = Make_eval (Rat_data)
