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
open Postfix_calc


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

let rat_of_string (str: string) : string =
  match Rat_data.of_string str with
  | Some v -> Rat_data.to_string v
  | None -> "0/0"

let test_rat_of_string _ =
  assert_equal "1/2" @@ rat_of_string "1/2";
  assert_equal "-1/2" @@ rat_of_string "-1/2";
  assert_equal "1/2" @@ rat_of_string "2/4";

  assert_equal None @@ Rat_data.of_string "1/-2";
  assert_equal None @@ Rat_data.of_string "-1/-2";
  assert_equal None @@ Rat_data.of_string "1/2/";
  assert_equal None @@ Rat_data.of_string "/1/2/";
  assert_equal None @@ Rat_data.of_string "/1/2/3";
  assert_equal None @@ Rat_data.of_string "1/2a"

let z4_next (str : string) : string * string = 
  match Z4_data.next str with
  | Some (s, v) -> (s, Z4_data.to_string v)
  | None -> ("", "")
let test_z4_next _= 
  assert_equal ("", "0") @@ z4_next "0";
  assert_equal ("@", "1") @@ z4_next "1@";
  assert_equal ("+", "1") @@ z4_next "1+";
  assert_equal (" 2", "1") @@ z4_next "1 2";

  assert_equal None @@ Z4_data.next "";
  assert_equal None @@ Z4_data.next "+";
  assert_equal None @@ Z4_data.next "*";
  assert_equal None @@ Z4_data.next "@123";
  assert_equal None @@ Z4_data.next "-1";
  assert_equal None @@ Z4_data.next "7"

let int_next (str : string) : string * string = 
  match Int_data.next str with
  | Some (s, v) -> (s, Int_data.to_string v)
  | None -> ("", "")

let test_int_next _ = 
  assert_equal ("", "0") @@ int_next "0";
  assert_equal ("@", "1") @@ int_next "1@";
  assert_equal ("+", "1") @@ int_next "1+";
  assert_equal (" 234", "1234") @@ int_next "1234 234";
  assert_equal ("-", "-1") @@ int_next "-1-";

  assert_equal None @@ Int_data.next "";
  assert_equal None @@ Int_data.next "+";
  assert_equal None @@ Int_data.next "*";
  assert_equal None @@ Int_data.next "@123";
  assert_equal None @@ Int_data.next "--2"

let rat_next (str : string) : string * string = 
  match Rat_data.next str with
  | Some (s, v) -> (s, Rat_data.to_string v)
  | None -> ("", "")

let test_rat_next _ = 
  assert_equal ("", "0/1") @@ rat_next "0/1";
  assert_equal ("@", "1/2") @@ rat_next "1/2@";
  assert_equal ("+", "1/1") @@ rat_next "1/1+";
  assert_equal (" 123123", "-1/2") @@ rat_next "-1/2 123123";
  assert_equal ("-", "-1/1") @@ rat_next "-1/1-";

  assert_equal None @@ Rat_data.next "";
  assert_equal None @@ Rat_data.next "+";
  assert_equal None @@ Rat_data.next "*";
  assert_equal None @@ Rat_data.next "@123";
  assert_equal None @@ Rat_data.next "/-2";
  assert_equal None @@ Rat_data.next "-x/-2"

let z4_eval (str : string) : string = 
  match Z4_eval.eval str with
  | Ok v -> Z4_data.to_string v
  | Error msg -> msg

let test_z4_eval _ = 
  assert_equal "2" @@ z4_eval "1 1 +";
  assert_equal "3" @@ z4_eval "1 3 3 ++";
  assert_equal "0" @@ z4_eval "2 2 *";
  assert_equal "0" @@ z4_eval "2 2 1 * +";
  assert_equal "2" @@ z4_eval "11+";
  assert_equal "0" @@ z4_eval "2\r\t 2 +\n";

  assert_equal "unmatched" @@ z4_eval "";
  assert_equal "unmatched" @@ z4_eval "1 1";
  assert_equal "unmatched" @@ z4_eval "1 * +";
  assert_equal "unmatched" @@ z4_eval "1 +";
  assert_equal "illegal character" @@ z4_eval "1 1 + @";
  assert_equal "illegal character" @@ z4_eval "1 4 7 ++"

let int_eval (str : string) : string = 
  match Int_eval.eval str with
  | Ok v -> Int_data.to_string v
  | Error msg -> msg

let test_int_eval _ = 
  assert_equal "2" @@ int_eval "1 1 +";
  assert_equal "12" @@ int_eval "1 4 7++";
  assert_equal "4" @@ int_eval "2 2 *";
  assert_equal "16" @@ int_eval "2 2 7 * +";
  assert_equal "-1" @@ int_eval "11-12+";
  assert_equal "4" @@ int_eval "2\r\t 2 +\n";

  assert_equal "unmatched" @@ int_eval "";
  assert_equal "unmatched" @@ int_eval "1 1";
  assert_equal "unmatched" @@ int_eval "1 * +";
  assert_equal "unmatched" @@ int_eval "1 +";
  assert_equal "illegal character" @@ int_eval "1 1 + @";
  assert_equal "illegal character" @@ int_eval "1/2"

let rat_eval (str : string) : string = 
  match Rat_eval.eval str with
  | Ok v -> Rat_data.to_string v
  | Error msg -> msg
let test_rat_eval _ = 
  assert_equal "1/1" @@ rat_eval "1/2 1/2 +";
  assert_equal "17/3" @@ rat_eval "1/1 4/1 2/3 ++";
  assert_equal "5/6" @@ rat_eval "1/2 1/3 +";
  assert_equal "1/1" @@ rat_eval "1/2 2/1 *";
  assert_equal "19/4" @@ rat_eval "3/2 2/1 7/6 + *";
  assert_equal "2/3" @@ rat_eval "2/6 1/3 +";
  assert_equal "4/1" @@ rat_eval "2/1 \r\t 2/1 +\n";

  assert_equal "unmatched" @@ rat_eval "";
  assert_equal "unmatched" @@ rat_eval "1/3 1/2";
  assert_equal "unmatched" @@ rat_eval "1/4 * +";
  assert_equal "unmatched" @@ rat_eval "1/4 *";
  assert_equal "illegal character" @@ rat_eval "1/2 1/1 + @";
  assert_equal "illegal character" @@ rat_eval "1/0 1/1 +";
  assert_equal "illegal character" @@ rat_eval "-1/-3 1/1 + @";
  assert_equal "illegal character" @@ rat_eval "1/-3 1/1 + @";
  assert_equal "illegal character" @@ rat_eval "1/3/2 1/1 + @"

let postfix_calc_tests = 
  "Postfix_calc tests"
  >::: [
    "Postfix_calc.Rat_data.of_string" >:: test_rat_of_string;
    "Postfix_calc.Z4_data.next" >:: test_z4_next;
    "Postfix_calc.Int_data.next" >:: test_int_next;
    "Postfix_calc.Rat_data.next" >:: test_rat_next;
    "Postfix_calc.Z4_eval.eval" >:: test_z4_eval;
    "Postfix_calc.Int_eval.eval" >:: test_int_eval;
    "Postfix_calc.Rat_eval.eval" >:: test_rat_eval;
  ]

(* Postcondition: Rationals are simplified *)
let test_rationals_simplified _ =
  assert_equal ("", "1/2") @@ rat_next "2/4";
  assert_equal ("", "1/2") @@ rat_next "234234/468468";
  assert_equal ("", "-1/2") @@ rat_next "-2/4";
  assert_equal ("", "-1/2") @@ rat_next "-234234/468468";
  assert_equal ("", "-1/3") @@ rat_next "-3/9";
  assert_equal ("", "12/29") @@ rat_next "12/29"

(* Postcondition: Next only reads a 't' off of a string *)
let test_maximal_munch _ =
  assert_equal ("034", "1") @@ z4_next "1034";
  assert_equal ("abcd", "1034") @@ int_next "1034abcd";
  assert_equal (" \n\r\tabcd", "1034") @@ int_next "1034 \n\r\tabcd";
  assert_equal ("/4+", "1/3") @@ rat_next "1/3/4+"

(* Precondition: Negative rationals must only have the  '-' in the numerator *)
let test_negative_rational_precondition _ =
  assert_equal ("", "-1/3") @@ rat_next "-1/3";
  assert_equal None @@ Rat_data.next "-1/-3";
  assert_equal None @@ Rat_data.next "1/-3"

(* Precondition: Correct number of operators *)
let test_input_operator_match _ = 
  assert_equal "1/2" @@ rat_eval "1/4 1/4 +";
  assert_equal "unmatched" @@ rat_eval "1/2 1/2";
  assert_equal "unmatched" @@ rat_eval "1/2 +";
  assert_equal "unmatched" @@ rat_eval "1/2 1/2 + +";
  assert_equal "unmatched" @@ rat_eval "1/2 + +";
  assert_equal "unmatched" @@ rat_eval "";

  assert_equal "2" @@ int_eval "1 1 +";
  assert_equal "unmatched" @@ int_eval "1 1";
  assert_equal "unmatched" @@ int_eval "1 +";
  assert_equal "unmatched" @@ int_eval "1 1 + +";
  assert_equal "unmatched" @@ int_eval "1 + +";
  assert_equal "unmatched" @@ int_eval "";

  assert_equal "2" @@ z4_eval "1 1 +";
  assert_equal "unmatched" @@ z4_eval "1 1";
  assert_equal "unmatched" @@ z4_eval "1 +";
  assert_equal "unmatched" @@ z4_eval "1 1 + +";
  assert_equal "unmatched" @@ z4_eval "1 + +";
  assert_equal "unmatched" @@ z4_eval ""

(* Precondition: Check illegal characters *)
let test_illegal_characters _ =
  assert_equal "illegal character" @@ rat_eval "1/2 2/3 + %*&";
  assert_equal "illegal character" @@ rat_eval "1/0 2/3 + %*&";
  assert_equal "illegal character" @@ int_eval "1 2 + %*&";
  assert_equal "illegal character" @@ z4_eval "1 2 + %*&"

let specifications = 
  "Assignment3 specifications"
  >::: [
    "Spec: Rationals simplified" >:: test_rationals_simplified;
    "Spec: Maximal munch satisfied" >:: test_maximal_munch;
    "Spec: Negative rational precondition" >:: test_negative_rational_precondition;
    "Spec: Input operator match" >:: test_input_operator_match;
    "Spec: Illegal character" >:: test_illegal_characters;
  ]

let series = "Assignment3 Tests" >::: [finite_group_tests; ring_tests; postfix_calc_tests; specifications]
let () = run_test_tt_main series