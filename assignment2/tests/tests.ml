open Core
open OUnit2
module D = Simpledict
module T = D.Tree

open D.Dict_item
 
(* ordered tree *)
let t1 = T.(Branch
  { item = "d"
  ; left = Branch {item = "a"; left = Leaf; right = Leaf}
  ; right = Branch {item = "e"; left = Leaf; right = Leaf}})

(* unordered tree *)
let t2 = T.(Branch
  { item = "d"
  ; left = Branch {item = "e"; left = Leaf; right = Leaf}
  ; right = Branch {item = "a"; left = Leaf; right = Leaf}})

(* unbalanced tree *)
let t3 = T.(Branch
{ item = "d"
; left = Branch {item = "e"; left = Branch {item = "a"; left = Leaf; right = Leaf}; right = Leaf}
; right = Leaf})

let test_tree_size _ =
  assert_equal 0 @@ T.size T.Leaf;
  assert_equal 3 @@ T.size t1

let test_tree_height _ =
  assert_equal (-1) @@ T.height T.Leaf;
  assert_equal 1 @@ T.height t1;
  assert_equal 2 @@ T.height t3

let test_tree_is_balanced _ =
  assert_equal true @@ T.is_balanced t1;
  assert_equal false @@ T.is_balanced t3

let test_tree_to_list _ =
  assert_equal [] @@ T.to_list T.Leaf;
  assert_equal ["a";"d";"e"] @@ T.to_list t1;
  assert_equal ["e";"d";"a"] @@ T.to_list t2

let test_tree_is_ordered _ = 
  assert_equal true @@ T.is_ordered t1 ~compare:String.compare;
  assert_equal false @@ T.is_ordered t2 ~compare:String.compare

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

let d2 = T.(Branch
  { item = {key="d"; value=0}
  ; left = Branch {item = {key="a"; value=1}; left = Leaf; right = Branch {item = {key="b"; value=3}; left = Leaf; right = Leaf}}
  ; right = Branch {item = {key="e"; value=2}; left = Leaf; right = Leaf}})

let d3 = T.(Branch
  { item = {key="d"; value="d"}
  ; left = Branch {item = {key="a"; value="a"}; left = Leaf; right = Leaf}
  ; right = Branch {item = {key="e"; value="e"}; left = Leaf; right = Leaf}})

let m1 = T.(Branch
{ item = {key="d"; value=0}
; left = Branch {item = {key="a"; value=1}; left = Leaf; right = Leaf}
; right = Leaf})

let m2 = T.(Branch {item = {key="e"; value=2}; left = Leaf; right = Leaf})

let m3 = T.(Branch {item = {key="a"; value=2}; left = Leaf; right = Leaf})

let test_size _ = 
  assert_equal 0 @@ D.size T.Leaf;
  assert_equal 3 @@ D.size d1

let test_to_list _ =
  assert_equal [("a", 1); ("d", 0); ("e", 2)] @@ D.to_list d1;
  assert_equal [("a", 1); ("b", 3); ("d", 0); ("e", 2)] @@ D.to_list d2

let test_lookup _ =
  assert_equal None @@ D.lookup T.Leaf ~key:"a";
  assert_equal None @@ D.lookup d1 ~key:"z";
  assert_equal (Some (1)) @@ D.lookup d1 ~key:"a"

let test_insert _ =
  assert_equal T.(Branch {item={key="5";value=5}; left=Leaf; right=Leaf}) @@ D.insert T.Leaf ~key:"5" ~value:5

let test_of_list _ = 
  assert_equal T.Leaf @@ D.of_list [];
  assert_equal d1 @@ D.of_list [("d", 0); ("a", 1); ("e", 2)]

let test_of_list_multi _ = 
  assert_equal T.Leaf @@ D.of_list_multi [];
  assert_equal T.(Branch {item={key= "d";value=[10;1;2]}; left=Leaf; right=Leaf}) @@ D.of_list_multi [("d",10);("d",1);("d",2)]

let test_map _ = 
  assert_equal T.Leaf @@ D.map T.Leaf ~f:(fun _ x -> x - 1);
  assert_equal d3  @@ D.map d1 ~f:(fun x _ -> x)

(* test_map_one requires working lookup_exn *)
let test_map_one _ =
  let d = D.map_one d1 ~key:"a" ~f:(fun _ x -> x + 10) in
  assert_equal 11 @@ D.lookup_exn d ~key:"a";
  assert_equal 2 @@ D.lookup_exn d ~key:"e"

let test_merge _ =
  assert_equal T.Leaf @@ D.merge T.Leaf T.Leaf;
  assert_equal d1 @@ D.merge d1 T.Leaf;
  assert_equal d1 @@ D.merge T.Leaf d1;
  assert_equal d1 @@ D.merge m1 m2;
  assert_equal (D.insert d1 ~key:"a" ~value:2) @@ D.merge d1 m3

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
  assert_equal T.Leaf @@ D.merge_with T.Leaf T.Leaf ~merger:merge_fun;
  assert_equal [("1", 0); ("8", 0); ("9", 0)] D.(merge_with ~merger:merge_fun example_dict1 T.Leaf |> to_list);
  assert_equal [("1", 1); ("8", 1); ("99", 1)] D.(merge_with ~merger:merge_fun T.Leaf example_dict2 |> to_list);
  assert_equal [("1", 10); ("8", 39); ("9", 0); ("99", 1)] D.(merge_with ~merger:merge_fun example_dict1 example_dict2 |> to_list)

let dict_tests = "dict tests" >: test_list [
    "D.size"          >:: test_size;
    "D.list"          >:: test_to_list;
    "D.lookup"        >:: test_lookup;
    "D.insert"        >:: test_insert;
    "D.of_list"       >:: test_of_list;
    "D.of_list_multi" >:: test_of_list_multi;
    "D.map"           >:: test_map;
    "D.map_one"       >:: test_map_one;
    "D.merge"         >:: test_merge;
    "D.merge_with"    >:: test_merge_with;
  ]

  (* Add another suite for any of your part II functions needing testing as well.  Make sure to put those functions in utils.ml and headers in utils.mli as only libraries are unit tested; keywordcount.ml is an executable not a library. *)
let series = "Assignment2 Tests" >::: [
    tree_tests;
    dict_tests;
  ]

let () = 
  run_test_tt_main series

  