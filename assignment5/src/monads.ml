open Core

[@@@warning "-27"]
[@@@warning "-32"]

module State_int = struct
  module T : Monad.Basic with type 'a t = int -> 'a * int = struct
    type 'a t = int -> 'a * int

    let bind (x : 'a t) ~(f : 'a -> 'b t) : 'b t =
     fun (i : int) ->
      let x', i' = x i in
      (f x') i'

    let return (x : 'a) : 'a t = fun i -> (x, i)
    let map = `Define_using_bind
  end

  include T
  include Monad.Make (T)

  let run (i : 'a t) : 'a * int = i 0
  let set (i : int) : unit t = fun _ -> ((), i)
  let get : int t = fun n -> (n, n)
  let inc : int t = fun n -> (n + 1, n + 1)
end

module Stack_monad : sig
  include Monad.S2

  val run : ('a, 'e) t -> 'a
  val push : 'e -> (unit, 'e) t
  val pop : ('e, 'e) t
  val is_empty : (bool, 'e) t
end = struct
  module Stack = struct
    type 'e t = 'e list

    let push (s : 'e t) (e : 'e) : 'e t = e :: s

    let pop (s : 'e t) : 'e * 'e t =
      match s with [] -> failwith "empty pop" | e :: s' -> (e, s')

    let is_empty (s : 'e t) : bool = match s with [] -> true | _ -> false
  end

  module T : Monad.Basic2 with type ('a, 'e) t = 'e Stack.t -> 'a * 'e Stack.t =
  struct
    type ('a, 'e) t = 'e Stack.t -> 'a * 'e Stack.t

    let bind (x : ('a, 'e) t) ~(f : 'a -> ('b, 'e) t) : ('b, 'e) t =
     fun (s : 'e Stack.t) ->
      let x', s' = x s in
      (f x') s'

    let return (x : 'a) : ('a, 'e) t = fun (s : 'e Stack.t) -> (x, s)
    let map = `Define_using_bind
  end

  include T
  include Monad.Make2 (T)

  let run (x : ('a, 'e) t) : 'a = match x [] with a, _ -> a
  let push (x : 'e) : (unit, 'e) t = fun (s : 'e Stack.t) -> ((), Stack.push s x)
  let pop : ('e, 'e) t = fun (s : 'e Stack.t) -> Stack.pop s
  let is_empty : (bool, 'e) t = fun (s : 'e Stack.t) -> (Stack.is_empty s, s)
end

open Stack_monad
open Stack_monad.Let_syntax

let simple_stack : ('a, char) t =
  let%bind () = push 'a' in
  let%bind () = push 'b' in
  let%bind () = push 'c' in
  let%bind c = pop in
  return Char.(c = 'c')
;;

let r = run simple_stack in
assert r

let are_balanced_mutable s =
  let stack_of_lefts = Stack.create () in
  let match_with s c = Char.(c = Stack.pop_exn s) in
  let parse = function
    | '(' -> Fn.const true @@ Stack.push stack_of_lefts '('
    | ')' -> match_with stack_of_lefts '('
    | _ -> true
  in
  try
    let r = String.fold ~init:true ~f:(fun b c -> b && parse c) s in
    r && Stack.is_empty stack_of_lefts
  with _ -> false

let parse (c : char) : (bool, char) Stack_monad.t =
  match c with
  | '(' -> push '(' >>= fun () -> return true
  | ')' -> pop >>= fun c -> return true
  | _ -> return true

let main_monadic (s : string) : (bool, char) Stack_monad.t =
  let rec check_balanced (s' : char list) =
    match s' with
    | [] -> is_empty >>= fun empty -> return empty
    | c :: st ->
        parse c >>= fun matched ->
        if matched then check_balanced st else return false
  in
  s |> String.to_list |> check_balanced

let are_balanced_monadic (s : string) : bool =
  try run @@ main_monadic s with _ -> false

(*
  ------------
  EXTRA CREDIT
  ------------

  Uncomment the final few lines in `monads.mli` and implement them here, if you choose.  
*)

module Exception_stack : sig
  include Monad.S2

  val run : ('a, 'e) t -> 'a option
  val push : 'e -> (unit, 'e) t
  val pop : ('e, 'e) t
  val is_empty : (bool, 'e) t
end = struct
  module Stack = struct
    type 'e t = 'e list

    let push (s : 'e t) (e : 'e) : 'e t = e :: s

    let pop (s : 'e t) : 'e option * 'e t =
      match s with [] -> (None, s) | e :: s' -> (Some e, s')

    let is_empty (s : 'e t) : bool = match s with [] -> true | _ -> false
  end

  module T :
    Monad.Basic2 with type ('a, 'e) t = 'e Stack.t -> 'a option * 'e Stack.t =
  struct
    type ('a, 'e) t = 'e Stack.t -> 'a option * 'e Stack.t

    let bind (x : ('a, 'e) t) ~(f : 'a -> ('b, 'e) t) : ('b, 'e) t =
     fun (s : 'e Stack.t) ->
      let x', s' = x s in
      match x' with Some a -> f a s' | None -> (None, s')

    let return (x : 'a) : ('a, 'e) t = fun (s : 'e Stack.t) -> (Some x, s)
    let map = `Define_using_bind
  end

  include T
  include Monad.Make2 (T)

  let run (x : ('a, 'e) t) : 'a option = match x [] with a, _ -> a

  let push (x : 'e) : (unit, 'e) t =
   fun (s : 'e Stack.t) -> (Some (), Stack.push s x)

  let pop : ('e, 'e) t = fun (s : 'e Stack.t) -> Stack.pop s

  let is_empty : (bool, 'e) t =
   fun (s : 'e Stack.t) -> (Some (Stack.is_empty s), s)
end

open Exception_stack
open Exception_stack.Let_syntax

let parse2 (c : char) : (bool, char) Exception_stack.t =
  match c with
  | '(' -> push '(' >>= fun () -> return true
  | ')' -> pop >>= fun c -> return Char.(c = '(')
  | _ -> return true

let main_more_monadic (s : string) : (bool, char) Exception_stack.t =
  let rec check_balanced (s' : char list) =
    match s' with
    | [] -> is_empty >>= fun empty -> return empty
    | c :: st ->
        parse2 c >>= fun matched ->
        if matched then check_balanced st else return false
  in
  s |> String.to_list |> check_balanced

let are_balanced_more_monadic (s : string) : bool =
  match run (main_more_monadic s) with Some result -> result | None -> false
