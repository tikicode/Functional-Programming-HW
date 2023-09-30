open Core
open OUnit2
module D = Simpledict
module T = D.Tree
module U = Utils

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
  assert_equal d1 @@ D.of_list [("d", 0); ("a", 1); ("e", 2)];
  assert_equal m1 @@ D.of_list [("d", 0); ("a", 0); ("a", 1)]

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

(* let cur_dir_ocaml_files = 
["./tests/tests.ml"; "./_build/default/tests/tests.ml";
   "./_build/default/tests/tests.mli"; "./_build/default/src/utils.ml";
   "./_build/default/src/simpledict.pp.ml";
   "./_build/default/src/keywordcount.mli";
   "./_build/default/src/keywordcount.ml";
   "./_build/default/src/simpledict.mli"; "./_build/default/src/simpledict.ml";
   "./_build/default/src/simpledict.pp.mli"; "./_build/default/src/utils.mli";
   "./src/utils.ml"; "./src/keywordcount.ml"; "./src/simpledict.mli";
   "./src/simpledict.ml"; "./src/utils.mli"] *)


(* let test_traverse_directory _ = 
  assert_equal ["./tests.ml"] @@ (U.traverse_directory ".") *)
  (* assert_equal cur_dir_ocaml_files @@ (U.traverse_directory "./tests/tests.ml");
  assert_equal cur_dir_ocaml_files @@ (U.traverse_directory "./boof") *)

let boof = "" (* test keyword *)

let test_filter_strings _ =
  assert_equal boof @@ U.filter_strings "";
  assert_equal "abcdef" @@ U.filter_strings "abc\"abc\"def";
  assert_equal "abcdef" @@ U.filter_strings "abc\" abc \"def";
  assert_equal "abc\ndef" @@ U.filter_strings "abc\" abc \"\ndef";
  assert_equal "abcdef" @@ U.filter_strings "abc\" abc \"def\" abc \""

let test_filter_comments _ = 
  assert_equal boof @@ U.filter_comments "";
  assert_equal "abcdef" @@ U.filter_comments "abc(* comment *)def";
  assert_equal "abcdef" @@ U.filter_comments "abc(* comment (* nest *) *)def";
  assert_equal "abcdef" @@ U.filter_comments "abc(* comment1 *)def(* comment2 *)";
  assert_equal "let (+*) = (+)" @@ U.filter_comments "let (+*) = (+)";
  assert_equal "let (+*) = (+) let (+*) = (+)" @@ U.filter_comments "let (+*) = (+)(* comment1 *)(* comment2 *) let (+*) = (+)"

let test_filter_non_characters _ = 
  assert_equal boof @@ U.filter_non_characters "";
  assert_equal " if x_i   0 then true else false " @@ U.filter_non_characters "(if x_i = 0 then true else false)"

let kw1 = T.(Branch{item ={key = "boof"; value = 4} ; left = T.Leaf; right = T.Leaf})

let test_count_kw_occurance _ =
  assert_equal kw1 @@ U.count_kw_occurance ~kw_list:["boof"] ~path:"./tests.ml"

let kw2 = T.(Branch{item ={key = "test2"; value = 1} ; 
left = T.Branch({item={key = "test1"; value = 2};left=T.Leaf; right=T.Leaf}); 
right = T.Branch({item={key = "test3"; value = 0};left=T.Leaf; right=T.Leaf})})

let kw3 = T.(Branch{item ={key = "test2"; value = 2} ; 
left = T.Branch({item={key = "test1"; value = 2};left=T.Leaf; right=T.Leaf}); 
right = T.Branch({item={key = "test3"; value = 1};left=T.Leaf; right=T.Leaf})})
  
let test_sort_kw_list_by_value _ = 
  assert_equal [] @@ U.sort_kw_list_by_value T.Leaf;
  assert_equal [("test1", 2); ("test2", 1)] @@ U.sort_kw_list_by_value kw2;
  assert_equal [("test1", 2); ("test2", 2); ("test3", 1)] @@ U.sort_kw_list_by_value kw3

  
let test_list_to_sexp _ = 
  assert_equal "()" @@ U.list_to_sexp [];
  assert_equal "(((keyword let)(count 5)))" @@ U.list_to_sexp [("let", 5)];
  assert_equal "(((keyword let)(count 5))((keyword and)(count 1)))" @@ U.list_to_sexp [("let", 5) ; ("and", 1)]

let utils_tests = "utils tests" >: test_list [
  (* "U.traverse_directory" >:: test_traverse_directory; *)
  "U.filter_strings" >:: test_filter_strings;
  "U.filter_comments" >:: test_filter_comments;
  "U.filter_non_characters" >:: test_filter_non_characters;
  "U.count_kw_occurance" >:: test_count_kw_occurance;
  "U.sort_kw_list_by_value" >:: test_sort_kw_list_by_value;
  "U.list_to_sexp" >:: test_list_to_sexp;
]

  (* Add another suite for any of your part II functions needing testing as well.  Make sure to put those functions in utils.ml and headers in utils.mli as only libraries are unit tested; keywordcount.ml is an executable not a library. *)
let series = "Assignment2 Tests" >::: [
    tree_tests;
    dict_tests;
    utils_tests;
  ]

let () = 
  run_test_tt_main series

  