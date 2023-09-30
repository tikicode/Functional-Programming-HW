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

(* let unimplemented () = failwith "unimplemented" *)

module Tree = struct
  type 'a t = Leaf | Branch of { item : 'a; left : 'a t; right : 'a t }
  [@@deriving show]

  (* You are expected to implement the functions in Tree *)

  let rec size (tree : 'a t) : int =
    match tree with
    | Leaf -> 0
    | Branch { left; right; _ } -> 1 + size left + size right

  let height (tree : 'a t) : int =
    let rec height_helper (t : 'a t) : int =
      match t with
      | Leaf -> 0
      | Branch { left; right; _ } ->
          1 + max (height_helper left) (height_helper right)
    in
    height_helper tree - 1

  let is_balanced (tree : 'a t) : bool =
    match tree with
    | Leaf -> true
    | Branch { left; right; _ } -> height left == height right

  let to_list (tree : 'a t) : 'a list =
    let rec in_order_traversal (t : 'a t) (acc : 'a list) : 'a list =
      match t with
      | Leaf -> acc
      | Branch { item; left; right } ->
          let right_list = in_order_traversal right acc in
          let updated_list = item :: right_list in
          in_order_traversal left updated_list
    in
    in_order_traversal tree []

  let is_ordered (tree : 'a t) ~(compare : 'a -> 'a -> int) : bool =
    let rec ordered_helper (tl : 'a t) (tr : 'a t) (v : 'a) : bool =
      let check_left (tl : 'a t) (vr : 'a) : bool =
        match tl with
        | Leaf -> true
        | Branch { item; left; right } ->
            compare item vr < 0 && ordered_helper left right item
      in
      let check_right (tr : 'a t) (vl : 'a) : bool =
        match tr with
        | Leaf -> true
        | Branch { item; left; right } ->
            compare item vl > 0 && ordered_helper left right item
      in
      check_left tl v && check_right tr v
    in
    match tree with
    | Leaf -> true
    | Branch { item; left; right } -> ordered_helper left right item
end
(* module Tree *)

module Dict_item = struct
  type 'a t = { key : string; value : 'a } [@@deriving show]

  (* We implement this for you.*)
  let compare (x : 'a t) (y : 'a t) : int = String.compare x.key y.key
end
(* module Dict_item *)

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
  let rec in_order_traversal (d : 'a t) (acc : (string * 'a) list) :
      (string * 'a) list =
    match d with
    | Leaf -> acc
    | Branch { item; left; right } ->
        let right_list = in_order_traversal right acc in
        let updated_list = (item.key, item.value) :: right_list in
        in_order_traversal left updated_list
  in
  in_order_traversal dict []

(* ... all other simpledict.mli functions ... *)

let rec lookup (dict : 'a t) ~(key : string) : 'a option =
  match dict with
  | Leaf -> None
  | Branch { item; left; right } ->
      let result = compare key item.key in
      if result = 0 then Some item.value
      else if result < 0 then lookup left ~key
      else lookup right ~key

let lookup_exn (dict : 'a t) ~key : 'a =
  match lookup dict ~key with Some value -> value | None -> raise Not_found

let rec insert (dict : 'a t) ~(key : string) ~(value : 'a) : 'a t =
  match dict with
  | Leaf -> Branch { item = { key; value }; left = Leaf; right = Leaf }
  | Branch { item; left; right } ->
      let result = compare key item.key in
      if result = 0 then Branch { item = { key; value }; left; right }
      else if result < 0 then
        Branch { item; left = insert left ~key ~value; right }
      else Branch { item; left; right = insert right ~key ~value }

let of_list (list : (string * 'a) list) : 'a t =
  let rec insert_kv (dict : 'a t) (l : (string * 'a) list) : 'a t =
    match l with
    | [] -> dict
    | (key, value) :: tl -> insert_kv (insert dict ~key ~value) tl
  in
  insert_kv Leaf list

let of_list_multi (list : (string * 'a) list) : 'a list t =
  let rec insert_list_kv (dict : 'a list t) ((key, value) : string * 'a) :
      'a list t =
    match dict with
    | Leaf ->
        Branch { item = { key; value = [ value ] }; left = Leaf; right = Leaf }
    | Branch { item; left; right } ->
        let result = compare key item.key in
        if result = 0 then
          Branch { item = { key; value = item.value @ [ value ] }; left; right }
        else if result < 0 then
          Branch { item; left = insert_list_kv left (key, value); right }
        else Branch { item; left; right = insert_list_kv right (key, value) }
  in
  let rec insert_list (d : 'a list t) (l : (string * 'a) list) : 'a list t =
    match l with [] -> d | hd :: tl -> insert_list (insert_list_kv d hd) tl
  in
  insert_list Leaf list

let rec map (dict : 'a t) ~(f : string -> 'a -> 'b) : 'b t =
  match dict with
  | Leaf -> Leaf
  | Branch { item; left; right } ->
      Branch
        {
          item = { key = item.key; value = f item.key item.value };
          right = map right ~f;
          left = map left ~f;
        }

let rec map_one (dict : 'a t) ~(key : string) ~(f : string -> 'a -> 'a) : 'a t =
  match dict with
  | Leaf -> Leaf
  | Branch { item; left; right } ->
      let result = compare key item.key in
      if result = 0 then
        Branch { item = { key; value = f item.key item.value }; left; right }
      else if result < 0 then
        Branch { item; left = map_one left ~key ~f; right }
      else Branch { item; left; right = map_one right ~key ~f }

let rec merge (d_one : 'a t) (d_two : 'a t) : 'a t =
  match d_two with
  | Leaf -> d_one
  | Branch { item; left; right } ->
      let dict_with_inserted =
        merge (insert d_one ~key:item.key ~value:item.value) right
      in
      merge dict_with_inserted left

let rec merge_with (d1 : 'a t) (d2 : 'b t)
    ~(merger : 'a option -> 'b option -> 'c) : 'c t =
  match (d1, d2) with
  | Leaf, Leaf -> Leaf
  | Branch { item; right; left }, Leaf ->
      let merge_value = merger (Some item.value) None in
      let merge_left = merge_with left Leaf ~merger in
      let merge_right = merge_with right Leaf ~merger in
      Branch
        {
          item = { key = item.key; value = merge_value };
          left = merge_left;
          right = merge_right;
        }
  | Leaf, Branch { item; right; left } ->
      let merge_value = merger None (Some item.value) in
      let merge_left = merge_with Leaf left ~merger in
      let merge_right = merge_with Leaf right ~merger in
      Branch
        {
          item = { key = item.key; value = merge_value };
          left = merge_left;
          right = merge_right;
        }
  | d_one, d_two ->
      let rec parse_one (d_one : 'a t) (d_two : 'b t) (c : 'c t) : 'c t =
        match d_one with
        | Leaf -> c
        | Branch { item; left; right } ->
            let check_two = lookup d_two ~key:item.key in
            if check_two = None then
              let new_c =
                parse_one left d_two
                  (insert c ~key:item.key
                     ~value:(merger (Some item.value) None))
              in
              parse_one right d_two new_c
            else
              let new_c =
                parse_one left d_two
                  (insert c ~key:item.key
                     ~value:(merger (Some item.value) check_two))
              in
              parse_one right d_two new_c
      in
      let rec parse_two (d_one : 'a t) (d_two : 'b t) (c : 'c t) : 'c t =
        match d_two with
        | Leaf -> c
        | Branch { item; left; right } ->
            let check_one = lookup d_one ~key:item.key in
            if check_one = None then
              let new_c =
                parse_two d_one left
                  (insert c ~key:item.key
                     ~value:(merger None (Some item.value)))
              in
              parse_two d_one right new_c
            else
              let new_c =
                parse_two d_one left
                  (insert c ~key:item.key
                     ~value:(merger check_one (Some item.value)))
              in
              parse_two d_one right new_c
      in
      parse_one d_one d_two (parse_two d_one d_two Leaf)
