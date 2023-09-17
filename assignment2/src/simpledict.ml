(*

FPSE Assignment 2

Name                  : 
List of Collaborators :

Please make a good faith effort at listing people you discussed any problems with here, as per the course academic integrity policy.

See file simpledict.mli for the specification of Part I of the assignment, and keywordcount.ml for Part II.  Recall from lecture that .mli files are module signatures aka module types and you will need to provide implementations of all the functions listed there in this file. 

Your Part I answers go here, and the Part II application should go in the keywordcount.ml file. No helper functions for Part II should exist in this file beyond what is required for Part I.

Hint: start by reading the code we've put here, and then copy over parts of the .mli file to make dummy headers and fill them with `unimplemented ()`. Note the changes in syntax between .mli and .ml files.

Note that .ml files need to include all `type` declarations in .mli files.

You must leave all `[@@deriving show]` annotations, or your autograder won't work. We use this to pretty-print your results.

*)

let unimplemented () = failwith "unimplemented"

module Tree = struct
  type 'a t =
    | Leaf
    | Branch of
        { item:  'a
        ; left:  'a t
        ; right: 'a t } [@@deriving show]

  (* You are expected to implement the functions in Tree *)

  let size (tree : 'a t) : int =
    unimplemented ()

  let height (tree : 'a t) : int =
    unimplemented ()

  let is_balanced (tree : 'a t) : bool =
    unimplemented ()

  let to_list (tree : 'a t) : 'a list =
    unimplemented ()

  let is_ordered (tree : 'a t) ~(compare: 'a -> 'a -> int) : bool =
    unimplemented ()

end (* module Tree *)

module Dict_item = struct
  type 'a t = { key: string ; value: 'a } [@@deriving show]

  (* We implement this for you.*)
  let compare (x : 'a t) (y : 'a t) : int =
    String.compare x.key y.key
end (* module Dict_item *)


type 'a t = 'a Dict_item.t Tree.t [@@deriving show]

(* 
   We provide this for you to demonstrate that the Tree module functions work on the dict
   since the dict is a Tree.t.
*)
let size = Tree.size

(*
    You will have to implement the rest of the functions in simpledict.mli. We have copied
    over `to_list` for you to show you the syntax, and you will need to copy over the rest.
    See `Tree.is_ordered` for the syntax with named arguments.
*)

let to_list (dict : 'a t) : (string * 'a) list =
  unimplemented ()

(* ... all other simpledict.mli functions ... *)