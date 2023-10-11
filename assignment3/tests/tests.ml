(*
  Part II: Tests
 
  In this part, you will need to create and run your own tests.  Tests should
  cover both common cases and edge cases.  In previous assignments, we only
  asked for a specified number of additional tests, but in this assignment we
  will be grading based on code coverage.
 
  Aim for complete code coverage on all functions, and we will check 
  by running the bisect tool on your code.  For that reason, you need 
  to add the following line in the dune file for your library:
      
      (preprocess (pps bisect_ppx))
 
  or else your tests will not run in the autograder.

 Additionally, you will need to write a special suite of tests here which
 verifies some invariants.  See the assignment for details.
 
*)

open Core
open OUnit2
open Finite_group
open Ring


module Test_fg = Make(
  struct
    let op (e1 : int) (e2 : int) = e1 + e2 - 1
    let n : int = 6
  end
)

(* Helpers to avoid redundant switch statments *)
let test_fg_of_int (e : int) : Test_fg.t =
  match Test_fg.of_int e with 
  | Some x -> x
  | None -> assert_failure "failure (you and the test)"

let z5_of_int (e : int) : Z5_add.t =
  match Z5_add.of_int e with 
  | Some x -> x
  | None -> assert_failure "failure (you and the test)"

let test_finite_group_of_int _ = 
  assert_equal 1 @@ Test_fg.to_int @@ test_fg_of_int 1;
  assert_equal None @@ Test_fg.of_int 6;
  assert_equal 2 @@ Z5_add.to_int @@ z5_of_int 2;
  assert_equal None @@ Z5_add.of_int 5

let test_finite_group_op _ = 
  assert_equal 4 @@ Test_fg.to_int @@ Test_fg.op (test_fg_of_int 2) (test_fg_of_int 3);
  assert_equal 1 @@ Test_fg.to_int @@ Test_fg.op (test_fg_of_int 5) (test_fg_of_int 3);
  assert_equal 4 @@ Z5_add.to_int @@ Z5_add.op (z5_of_int 1) (z5_of_int 3);
  assert_equal 1 @@ Z5_add.to_int @@ Z5_add.op (z5_of_int 3) (z5_of_int 3)

let test_finite_group_id _ = 
  assert_equal 1 @@ Test_fg.to_int @@ Test_fg.id;
  assert_equal 0 @@ Z5_add.to_int @@ Z5_add.id

let test_finite_group_inverse _ =
  assert_equal 1 @@ Z5_add.to_int @@ Z5_add.inverse (z5_of_int 4);
  assert_equal 2 @@ Z5_add.to_int @@ Z5_add.inverse (z5_of_int 3);
  assert_equal 3 @@ Z5_add.to_int @@ Z5_add.inverse (z5_of_int 2);
  assert_equal 4 @@ Z5_add.to_int @@ Z5_add.inverse (z5_of_int 1);
  assert_equal 0 @@ Z5_add.to_int @@ Z5_add.inverse (z5_of_int 0);
  assert_equal 0 @@ Test_fg.to_int @@ Test_fg.inverse (test_fg_of_int 2);
  assert_equal 1 @@ Test_fg.to_int @@ Test_fg.inverse (test_fg_of_int 1);
  assert_equal 2 @@ Test_fg.to_int @@ Test_fg.inverse (test_fg_of_int 0)

module MemZ5 = Memoize(Z5_add)

let test_memoize_inverse _ = 
  assert_equal 1 @@ Z5_add.to_int @@ MemZ5.inverse (z5_of_int 4);
  assert_equal 2 @@ Z5_add.to_int @@ MemZ5.inverse (z5_of_int 3);
  assert_equal 3 @@ Z5_add.to_int @@ MemZ5.inverse (z5_of_int 2);
  assert_equal 4 @@ Z5_add.to_int @@ MemZ5.inverse (z5_of_int 1);
  assert_equal 0 @@ Z5_add.to_int @@ MemZ5.inverse (z5_of_int 0)

let finite_group_tests = 
  "Finite Group tests"
  >::: [
    "Finite_group.to_int" >:: test_finite_group_of_int;
    "Finite_group.op" >:: test_finite_group_op;
    "Finite_group.id" >:: test_finite_group_id;
    "Finite_group.inverse" >:: test_finite_group_inverse;
    "Memoize.inverse" >:: test_memoize_inverse;
  ]

let z4_of_string (str : string) : Z4.t =
  match Z4.of_string str with 
  | Some x -> x
  | None -> assert_failure "failure (you and the test)"

let test_ring_of_string _ = 
  assert_equal "1" @@ Z4.to_string @@ z4_of_string "1";
  assert_equal None @@ Z4.of_string "@"

let test_ring_add _ = 
  assert_equal "3" @@ Z4.to_string @@ Z4.( + ) (z4_of_string "1") (z4_of_string "2");
  assert_equal "0" @@ Z4.to_string @@ Z4.( + ) (z4_of_string "2") (z4_of_string "2");
  assert_equal "2" @@ Z4.to_string @@ Z4.( + ) (z4_of_string "3") (z4_of_string "3")

let test_ring_multiply _ =
  assert_equal "2" @@ Z4.to_string @@ Z4.( * ) (z4_of_string "1") (z4_of_string "2");
  assert_equal "1" @@ Z4.to_string @@ Z4.( * ) (z4_of_string "3") (z4_of_string "3");
  assert_equal "0" @@ Z4.to_string @@ Z4.( * ) (z4_of_string "0") (z4_of_string "1")

let ring_tests = 
  "Ring tests"
  >::: [
    "Ring.of_string" >:: test_ring_of_string;
    "Ring.( + )" >:: test_ring_add;
    "Ring.( * )" >:: test_ring_multiply;
  ]


let series = "Assignment3 Tests" >::: [finite_group_tests; ring_tests]
let () = run_test_tt_main series