open Core
open OUnit2
module D = Simpledict
module T = D.Tree

open D.Dict_item

let t1 = T.(Branch
  { item = "d"
  ; left = Branch {item = "a"; left = Leaf; right = Leaf}
  ; right = Branch {item = "e"; left = Leaf; right = Leaf}})

let test_tree_size _ =
  assert_equal 0 @@ T.size T.Leaf;
  assert_equal 3 @@ T.size t1

let test_tree_height _ =
  assert_equal 1 @@ T.height t1

let test_tree_is_balanced _ =
  assert_equal true @@ T.is_balanced t1

let test_tree_to_list _ =
  assert_equal [] @@ T.to_list T.Leaf;
  assert_equal ["a";"d";"e"] @@ T.to_list t1

let test_tree_is_ordered _ = () (* fill in *)

let tree_tests = "Tree tests" >::: [
  "Tree.size"        >:: test_tree_size;
  "Tree.height"      >:: test_tree_height;
  "Tree.is_balanced" >:: test_tree_is_balanced;
  "Tree.to_list"     >:: test_tree_to_list;
  "Tree.is_ordered"  >:: test_tree_is_ordered;
]

let d1 = T.(Branch
  { item = {key="d"; value=0}
  ; left = Branch {item = {key="a"; value=1}; left = Leaf; right = Leaf}
  ; right = Branch {item = {key="e"; value=2}; left = Leaf; right = Leaf}})

let test_size _ = () (* fill in *)

let test_to_list _ =
  assert_equal [("a", 1); ("d", 0); ("e", 2)] @@ D.to_list d1

let test_lookup _ = () (* fill in *)

let test_insert _ =
  assert_equal T.(Branch {item={key="5";value=5}; left=Leaf; right=Leaf}) @@ D.insert T.Leaf ~key:"5" ~value:5

let test_map _ = () (* fill in *)

(* also should test of_list, of_list_multi *)

(* test_map_one requires working lookup_exn *)
let test_map_one _ =
  let d = D.map_one d1 ~key:"a" ~f:(fun _ x -> x + 10) in
  assert_equal 11 @@ D.lookup_exn d ~key:"a";
  assert_equal 2 @@ D.lookup_exn d ~key:"e"

let example_dict1 = T.(Branch
  { item = {key = "9"; value = 1}
  ; left = Branch
    { item = {key = "8"; value = 3}
    ; left = Branch { item = {key = "1"; value = 5}; left = Leaf; right = Leaf } 
    ; right = Leaf}
  ; right = Leaf}
)

let example_dict2 = T.(Branch
  { item = {key = "8"; value = 13}
  ; left = Branch
    { item = {key = "1"; value = 2}
    ; left = Leaf
    ; right = Leaf }
   ; right = Branch
    { item = {key = "99"; value = 2}
    ; left = Leaf
    ; right = Leaf }}
)

let merge_fun l r =
  match l, r with 
  | None, None -> failwith "should not get here!"
  | Some _, None -> 0
  | None, Some _ -> 1
  | Some a, Some b -> a * b

(* test_merge_with requires working to_list *)
let test_merge_with _ =
  assert_equal [("1", 10); ("8", 39); ("9", 0); ("99", 1)] D.(merge_with ~merger:merge_fun example_dict1 example_dict2 |> to_list)

let dict_tests = "dict tests" >: test_list [
    "D.size"       >:: test_size;
    "D.list"       >:: test_to_list;
    "D.lookup"     >:: test_lookup;
    "D.insert"     >:: test_insert;
    "D.map"        >:: test_map;
    "D.map_one"    >:: test_map_one;
    "D.merge_with" >:: test_merge_with;
  ]

  (* Add another suite for any of your part II functions needing testing as well.  Make sure to put those functions in utils.ml and headers in utils.mli as only libraries are unit tested; keywordcount.ml is an executable not a library. *)
let series = "Assignment2 Tests" >::: [
    tree_tests;
    dict_tests;
  ]

let () = 
  run_test_tt_main series

