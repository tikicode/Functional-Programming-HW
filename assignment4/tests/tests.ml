(*
  Put the tests for lib.ml functions here
*)

open Core
open OUnit2
open Lib

module Dist_int = Dist (struct
  type t = int [@@deriving compare, sexp]
end)

let dist_tri = Dist_int.make_distribution [ 1; 2; 3; 4; 4; 4; 2; 2; 3; 1 ] ~n:3

let test_dist_tri_find (k : int list) : int list =
  match Dist_int.ngram_find dist_tri k with
  | Some x -> x
  | None -> assert_failure "failure (you and the test)"

let test_make_distribution_tri _ =
  assert_equal [ 3 ] @@ test_dist_tri_find [ 1; 2 ];
  assert_equal [ 2; 4 ] @@ test_dist_tri_find [ 4; 4 ];
  assert_equal [ 1; 4 ] @@ test_dist_tri_find [ 2; 3 ];
  assert_equal [ 2 ] @@ test_dist_tri_find [ 4; 2 ];
  assert_equal [ 3 ] @@ test_dist_tri_find [ 2; 2 ];
  assert_equal None @@ Dist_int.ngram_find dist_tri [ 1; 3 ];
  assert_equal None @@ Dist_int.ngram_find dist_tri [ 4; 3 ];
  assert_equal None @@ Dist_int.ngram_find dist_tri [ 0 ];
  assert_equal None @@ Dist_int.ngram_find dist_tri [ 1; 2; 3 ]

let dist_bi = Dist_int.make_distribution [ 1; 2; 3; 4; 4; 4; 2; 2; 3; 1 ] ~n:2

let test_dist_bi_find (k : int list) : int list =
  match Dist_int.ngram_find dist_bi k with
  | Some x -> x
  | None -> assert_failure "failure (you and the test)"

let test_make_distribution_bi _ =
  assert_equal [ 2 ] @@ test_dist_bi_find [ 1 ];
  assert_equal [ 1; 4 ] @@ test_dist_bi_find [ 3 ];
  assert_equal [ 2; 4; 4 ] @@ test_dist_bi_find [ 4 ];
  assert_equal None @@ Dist_int.ngram_find dist_bi [ 5 ];
  assert_equal None @@ Dist_int.ngram_find dist_bi [ 4; 3 ];
  assert_equal None @@ Dist_int.ngram_find dist_bi [ 0 ];
  assert_equal None @@ Dist_int.ngram_find dist_bi [ 1; 2; 3 ]

let test_random_tri = Dist_int.make_distribution [ 1; 2; 3; 4; 5 ] ~n:3
let test_random_bi = Dist_int.make_distribution [ 1; 2; 3; 4; 5 ] ~n:2

let test_sample_random_sequence _ =
  assert_equal [ 1; 2; 3; 4; 5 ]
  @@ Dist_int.sample_random_sequence [ 1; 2 ] ~k:5 test_random_tri;
  assert_equal [ 1; 2; 3 ]
  @@ Dist_int.sample_random_sequence [ 1; 2; 3 ] ~k:5 test_random_tri;
  assert_equal [ 1 ]
  @@ Dist_int.sample_random_sequence [ 1 ] ~k:5 test_random_tri;
  assert_equal [] @@ Dist_int.sample_random_sequence [] ~k:5 test_random_tri;
  assert_equal [ 1; 2; 3; 4; 5 ]
  @@ Dist_int.sample_random_sequence [ 1 ] ~k:5 test_random_bi;
  assert_equal [ 1; 2; 3 ]
  @@ Dist_int.sample_random_sequence [ 1; 2; 3 ] ~k:5 test_random_bi;
  assert_equal [] @@ Dist_int.sample_random_sequence [] ~k:5 test_random_bi

let distribution_tests =
  "Dist tests"
  >::: [
         "make_distribution_tri" >:: test_make_distribution_tri;
         "make_distribution_bi" >:: test_make_distribution_bi;
         "sample_random_sequence" >:: test_sample_random_sequence;
       ]

let series = "Assignment4 Tests" >::: [ distribution_tests ]
let () = run_test_tt_main series
