(*
  Put the tests for lib.ml functions here
*)

open Core
open OUnit2
open Lib

module Dist_int = Dist (struct
  type t = int [@@deriving compare, sexp]
end)

let dist_tri_int = Dist_int.make_distribution [ 1; 2; 3; 4; 4; 4; 2; 2; 3; 1 ] ~n:3

let test_dist_tri_int_find (k : int list) : int list =
  match Dist_int.ngram_find dist_tri_int k with
  | Some x -> x
  | None -> assert_failure "failure (you and the test)"

let test_make_distribution_tri_int _ =
  assert_equal [ 3 ] @@ test_dist_tri_int_find [ 1; 2 ];
  assert_equal [ 2; 4 ] @@ test_dist_tri_int_find [ 4; 4 ];
  assert_equal [ 1; 4 ] @@ test_dist_tri_int_find [ 2; 3 ];
  assert_equal [ 2 ] @@ test_dist_tri_int_find [ 4; 2 ];
  assert_equal [ 3 ] @@ test_dist_tri_int_find [ 2; 2 ];
  assert_equal None @@ Dist_int.ngram_find dist_tri_int [ 1; 3 ];
  assert_equal None @@ Dist_int.ngram_find dist_tri_int [ 4; 3 ];
  assert_equal None @@ Dist_int.ngram_find dist_tri_int [ 0 ];
  assert_equal None @@ Dist_int.ngram_find dist_tri_int [ 1; 2; 3 ]

let dist_bi_int = Dist_int.make_distribution [ 1; 2; 3; 4; 4; 4; 2; 2; 3; 1 ] ~n:2

let test_dist_bi_int_find (k : int list) : int list =
  match Dist_int.ngram_find dist_bi_int k with
  | Some x -> x
  | None -> assert_failure "failure (you and the test)"

let test_make_distribution_bi_int _ =
  assert_equal [ 2 ] @@ test_dist_bi_int_find [ 1 ];
  assert_equal [ 1; 4 ] @@ test_dist_bi_int_find [ 3 ];
  assert_equal [ 2; 4; 4 ] @@ test_dist_bi_int_find [ 4 ];
  assert_equal None @@ Dist_int.ngram_find dist_bi_int [ 5 ];
  assert_equal None @@ Dist_int.ngram_find dist_bi_int [ 4; 3 ];
  assert_equal None @@ Dist_int.ngram_find dist_bi_int [ 0 ];
  assert_equal None @@ Dist_int.ngram_find dist_bi_int [ 1; 2; 3 ]

let test_random_tri_int = Dist_int.make_distribution [ 1; 2; 3; 4; 5 ] ~n:3
let test_random_bi_int = Dist_int.make_distribution [ 1; 2; 3; 4; 5 ] ~n:2

let test_sample_random_sequence_int _ =
  assert_equal [ 1; 2; 3; 4; 5 ]
  @@ Dist_int.sample_random_sequence [ 1; 2 ] ~k:5 test_random_tri_int;
  assert_equal [ 1; 2; 3 ]
  @@ Dist_int.sample_random_sequence [ 1; 2; 3 ] ~k:5 test_random_tri_int;
  assert_equal [ 1 ]
  @@ Dist_int.sample_random_sequence [ 1 ] ~k:5 test_random_tri_int;
  assert_equal [] @@ Dist_int.sample_random_sequence [] ~k:5 test_random_tri_int;
  assert_equal [ 1; 2; 3; 4; 5 ]
  @@ Dist_int.sample_random_sequence [ 1 ] ~k:5 test_random_bi_int;
  assert_equal [ 1 ]
  @@ Dist_int.sample_random_sequence [ 1 ] ~k:1 test_random_bi_int;
  assert_equal [ 1 ]
  @@ Dist_int.sample_random_sequence [ 1 ] ~k:0 test_random_bi_int;
  assert_equal [ 1; 2; 3 ]
  @@ Dist_int.sample_random_sequence [ 1; 2; 3 ] ~k:5 test_random_bi_int;
  assert_equal [] @@ Dist_int.sample_random_sequence [] ~k:5 test_random_bi_int


module Dist_string = Dist (struct
  type t = string [@@deriving compare, sexp]
end)

let dist_tri_str = Dist_string.make_distribution [ "1"; "2"; "3"; "4"; "4"; "4"; "2"; "2"; "3"; "1" ] ~n:3

let test_dist_tri_str_find (k : string list) : string list =
  match Dist_string.ngram_find dist_tri_str k with
  | Some x -> x
  | None -> assert_failure "failure (you and the test)"

let test_make_distribution_tri_str _ =
  assert_equal [ "3" ] @@ test_dist_tri_str_find [ "1"; "2" ];
  assert_equal [ "2"; "4" ] @@ test_dist_tri_str_find [ "4"; "4" ];
  assert_equal [ "1"; "4" ] @@ test_dist_tri_str_find [ "2"; "3" ];
  assert_equal [ "2" ] @@ test_dist_tri_str_find [ "4"; "2" ];
  assert_equal [ "3" ] @@ test_dist_tri_str_find [ "2"; "2" ];
  assert_equal None @@ Dist_string.ngram_find dist_tri_str [ "1"; "3" ];
  assert_equal None @@ Dist_string.ngram_find dist_tri_str [ "4"; "3" ];
  assert_equal None @@ Dist_string.ngram_find dist_tri_str [ "0" ];
  assert_equal None @@ Dist_string.ngram_find dist_tri_str [ "1"; "2"; "3" ]

let dist_bi_str= Dist_string.make_distribution [ "1"; "2"; "3"; "4"; "4"; "4"; "2"; "2"; "3"; "1" ] ~n:2

let test_dist_bi_find (k : string list) : string list =
  match Dist_string.ngram_find dist_bi_str k with
  | Some x -> x
  | None -> assert_failure "failure (you and the test)"

let test_make_distribution_bi_str _ =
  assert_equal [ "2" ] @@ test_dist_bi_find [ "1" ];
  assert_equal [ "1"; "4" ] @@ test_dist_bi_find [ "3" ];
  assert_equal [ "2"; "4"; "4" ] @@ test_dist_bi_find [ "4" ];
  assert_equal None @@ Dist_string.ngram_find dist_bi_str [ "5" ];
  assert_equal None @@ Dist_string.ngram_find dist_bi_str [ "4"; "3" ];
  assert_equal None @@ Dist_string.ngram_find dist_bi_str [ "0" ];
  assert_equal None @@ Dist_string.ngram_find dist_bi_str [ "1"; "2"; "3" ]

let test_random_tri_str = Dist_string.make_distribution [ "1"; "2"; "3"; "4"; "5" ] ~n:3
let test_random_bi_str = Dist_string.make_distribution [ "1"; "2"; "3"; "4"; "5" ] ~n:2

let test_sample_random_sequence_str _ =
  assert_equal [ "1"; "2"; "3"; "4"; "5" ]
  @@ Dist_string.sample_random_sequence [ "1"; "2" ] ~k:5 test_random_tri_str;
  assert_equal [ "1"; "2"; "3" ]
  @@ Dist_string.sample_random_sequence [ "1"; "2"; "3" ] ~k:5 test_random_tri_str;
  assert_equal [ "1" ]
  @@ Dist_string.sample_random_sequence [ "1" ] ~k:5 test_random_tri_str;
  assert_equal [] @@ Dist_string.sample_random_sequence [] ~k:5 test_random_tri_str;
  assert_equal [ "1"; "2"; "3"; "4"; "5" ]
  @@ Dist_string.sample_random_sequence [ "1" ] ~k:5 test_random_bi_str;
  assert_equal [ "1"; "2"; "3" ]
  @@ Dist_string.sample_random_sequence [ "1"; "2"; "3" ] ~k:5 test_random_bi_str;
  assert_equal [ "1" ]
  @@ Dist_string.sample_random_sequence [ "1" ] ~k:1 test_random_bi_str;
  assert_equal [ "1" ]
  @@ Dist_string.sample_random_sequence [ "1" ] ~k:0 test_random_bi_str;
  assert_equal [] @@ Dist_string.sample_random_sequence [] ~k:5 test_random_bi_str

let distribution_tests =
  "Dist tests"
  >::: [
         "make_distribution_tri_int" >:: test_make_distribution_tri_int;
         "make_distribution_bi_int" >:: test_make_distribution_bi_int;
         "sample_random_sequence_int" >:: test_sample_random_sequence_int;
         "make_distribution_tri_str" >:: test_make_distribution_tri_str;
         "make_distribution_bi_str" >:: test_make_distribution_bi_str;
         "sample_random_sequence_str" >:: test_sample_random_sequence_str;
       ]

let series = "Assignment4 Tests" >::: [ distribution_tests ]
let () = run_test_tt_main series
