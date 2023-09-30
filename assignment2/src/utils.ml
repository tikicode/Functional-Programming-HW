(*
   
See utils.mli. This file contains implementations for your utils.mli functions.

*)
open Sexplib.Std

let traverse_directory (path : string) : string list =
  let rec traverse_helper (path : string) : string list =
    let dir_contents = Sys.readdir path |> Array.to_list in
    let is_ml_mli file_name =
      let match_file_name = Filename.extension file_name in
      match_file_name = ".ml" || match_file_name = ".mli"
    in
    let is_subdirectory dir_item =
      Sys.is_directory (Filename.concat path dir_item)
    in
    let ocaml_files =
      dir_contents
      |> List.filter (fun file -> is_ml_mli file)
      |> List.map (Filename.concat path)
    in
    let sub_dir_files =
      dir_contents
      |> List.filter (fun dir_item -> is_subdirectory dir_item)
      |> List.map (Filename.concat path)
      |> List.map traverse_helper |> List.concat
    in
    ocaml_files @ sub_dir_files
  in
  try
    match Sys.is_directory path with
    | true -> traverse_helper path
    | false -> traverse_helper "."
  with Sys_error _ -> traverse_helper "."

let read_file_as_string (path : string) : string =
  Stdio.In_channel.read_all path

let filter_strings (contents : string) : string =
  let rec filter_helper (chars : char list) (inside_string : bool)
      (acc : char list) =
    match chars with
    | [] -> List.rev acc
    | '"' :: tl when not inside_string -> filter_helper tl true acc
    | '"' :: tl when inside_string -> filter_helper tl false acc
    | _ :: tl when inside_string -> filter_helper tl inside_string acc
    | c :: tl -> filter_helper tl inside_string (c :: acc)
  in
  let char_list = String.to_seq contents |> List.of_seq in
  let filtered_chars = filter_helper char_list false [] in
  String.concat "" (List.map (String.make 1) filtered_chars)

let filter_comments (contents : string) : string =
  let rec filter_helper (chars : char list) (acc : char list) (depth : int)
      (open_seen : bool) =
    match chars with
    | [] -> List.rev acc
    | '(' :: '*' :: tl -> filter_helper tl acc (depth + 1) true
    | '*' :: ')' :: tl when open_seen = true ->
        filter_helper tl acc (depth - 1) (if depth > 0 then true else false)
    | '*' :: ')' :: tl when open_seen = false ->
        filter_helper tl (')' :: '*' :: acc) depth false
    | _ :: tl when depth != 0 && open_seen = true ->
        filter_helper tl acc depth true
    | c :: tl -> filter_helper tl (c :: acc) depth false
  in
  let char_list = String.to_seq contents |> List.of_seq in
  let filtered_chars = filter_helper char_list [] 0 false in
  String.concat "" (List.map (String.make 1) filtered_chars)

let filter_non_characters (contents : string) : string =
  let not_non_character (c : char) : bool =
    match c with
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  let replace_non_characters (c : char) : char =
    if not_non_character c then c else ' '
  in
  String.map replace_non_characters contents

let clean_file (path : string) : string =
  filter_non_characters
    (filter_comments (filter_strings (read_file_as_string path)))

let count_kw_occurance ~(kw_list : string list) ~(path : string) :
    int Simpledict.t =
  let word_list = String.split_on_char ' ' (clean_file path) in
  let count_kw (occurances : int Simpledict.t) (kw : string) : int Simpledict.t
      =
    let count =
      List.fold_left
        (fun count word -> if word = kw then count + 1 else count)
        0 word_list
    in
    Simpledict.insert occurances ~key:kw ~value:count
  in
  List.fold_left count_kw Simpledict.Tree.Leaf kw_list

let sort_kw_list_by_value (kw_counts : int Simpledict.t) : (string * int) list =
  kw_counts |> Simpledict.to_list
  |> List.filter (fun (_, value) -> value > 0)
  |> List.stable_sort (fun (_, val1) (_, val2) -> compare val2 val1)

type z = { keyword : string; count : int } [@@deriving sexp]

let list_to_sexp (kw_counts : (string * int) list) : string =
  kw_counts
  |> List.map (fun (key, value) ->
         { keyword = key; count = value } |> sexp_of_z)
  |> Sexplib.Sexp.List |> Sexplib.Sexp.to_string

(* let keyword_counts_to_sexp counts = ()

   let print_endline line = () *)
