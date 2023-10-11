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

  let next (_ : string) : (string * t) option = failwith "unimplemented"
end

module Make_eval (Data : Data) : Eval with type t = Data.t = struct
  type t = Data.t

  let eval (_ : string) : (t, string) result = failwith "unimplemented"
end

module Z4_data = Make_data (Ring.Z4)
module Int_data = Make_data (Ring.Z4)
module Rat_data = Make_data (Ring.Z4)
module Z4_eval = Make_eval (Z4_data)
module Int_eval = Make_eval (Int_data)
module Rat_eval = Make_eval (Rat_data)
