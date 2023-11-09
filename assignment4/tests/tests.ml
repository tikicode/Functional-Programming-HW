(*
  Put the tests for lib.ml functions here
*)

open Core
open OUnit2
open Lib

(* Deterministic Random Module *)
module Known_random = struct
  let int (_ : int) : int = 0
end

(* Integer Tests *)

module Dist_int = struct
  type t = int [@@deriving compare, sexp]
end

open Dist (Dist_int) (Known_random)

(* Trigram Integer Tests *)
let test_make_distribution_tri_int _ =
  assert_equal [] @@ (make_distribution ~n:3 [] |> to_list);
  assert_equal [ ([ 1; 1 ], [ 1 ]) ]
  @@ (make_distribution ~n:3 [ 1; 1; 1 ] |> to_list);

  assert_equal [ ([ 1; 2 ], [ 3 ]); ([ 2; 3 ], [ 4 ]); ([ 3; 4 ], [ 5 ]) ]
  @@ (make_distribution ~n:3 [ 1; 2; 3; 4; 5 ] |> to_list);

  assert_equal
    [
      ([ 1; 2 ], [ 3 ]);
      ([ 2; 2 ], [ 3 ]);
      ([ 2; 3 ], [ 1; 4 ]);
      ([ 3; 4 ], [ 4 ]);
      ([ 4; 2 ], [ 2 ]);
      ([ 4; 4 ], [ 2; 4 ]);
    ]
  @@ (make_distribution ~n:3 [ 1; 2; 3; 4; 4; 4; 2; 2; 3; 1 ] |> to_list);

  assert_equal
    [
      ([ 1; 3 ], [ 5 ]);
      ([ 1; 4 ], [ 6 ]);
      ([ 1; 6 ], [ 3 ]);
      ([ 2; 1 ], [ 6; 3 ]);
      ([ 2; 4 ], [ 5 ]);
      ([ 3; 1 ], [ 4 ]);
      ([ 3; 2 ], [ 4 ]);
      ([ 3; 5 ], [ 7 ]);
      ([ 4; 5 ], [ 2 ]);
      ([ 4; 6 ], [ 7 ]);
      ([ 5; 2 ], [ 1 ]);
      ([ 5; 7 ], [ 2 ]);
      ([ 6; 3 ], [ 1 ]);
      ([ 7; 2 ], [ 1 ]);
    ]
  @@ (make_distribution ~n:3
        [ 3; 2; 4; 5; 2; 1; 3; 5; 7; 2; 1; 6; 3; 1; 4; 6; 7 ]
     |> to_list)

(* Bigram Integer Tests *)
let test_make_distribution_bi_int _ =
  assert_equal [] @@ (make_distribution ~n:2 [] |> to_list);

  assert_equal [ ([ 1 ], [ 1; 1 ]) ]
  @@ (make_distribution ~n:2 [ 1; 1; 1 ] |> to_list);

  assert_equal
    [ ([ 1 ], [ 2 ]); ([ 2 ], [ 3 ]); ([ 3 ], [ 4 ]); ([ 4 ], [ 5 ]) ]
  @@ (make_distribution ~n:2 [ 1; 2; 3; 4; 5 ] |> to_list);

  assert_equal
    [
      ([ 1 ], [ 2 ]);
      ([ 2 ], [ 3; 2; 3 ]);
      ([ 3 ], [ 1; 4 ]);
      ([ 4 ], [ 2; 4; 4 ]);
    ]
  @@ (make_distribution ~n:2 [ 1; 2; 3; 4; 4; 4; 2; 2; 3; 1 ] |> to_list);

  assert_equal
    [
      ([ 1 ], [ 4; 6; 3 ]);
      ([ 2 ], [ 1; 1; 4 ]);
      ([ 3 ], [ 1; 5; 2 ]);
      ([ 4 ], [ 6; 5 ]);
      ([ 5 ], [ 7; 2 ]);
      ([ 6 ], [ 7; 3 ]);
      ([ 7 ], [ 2 ]);
    ]
  @@ (make_distribution ~n:2
        [ 3; 2; 4; 5; 2; 1; 3; 5; 7; 2; 1; 6; 3; 1; 4; 6; 7 ]
     |> to_list)

(* Other Integer Tests *)
let test_sample_random_sequence_int _ =
  let test_random_tri_int = make_distribution [ 1; 2; 3; 4; 5 ] ~n:3 in
  let test_random_bi_int = make_distribution [ 1; 2; 3; 4; 5 ] ~n:2 in

  assert_equal [ 1; 2; 3; 4; 5 ]
  @@ sample_random_sequence [ 1; 2 ] ~k:5 test_random_tri_int;

  assert_equal [ 1; 2; 3 ]
  @@ sample_random_sequence [ 1; 2; 3 ] ~k:5 test_random_tri_int;

  assert_equal [ 1 ] @@ sample_random_sequence [ 1 ] ~k:5 test_random_tri_int;

  assert_equal [] @@ sample_random_sequence [] ~k:5 test_random_tri_int;

  assert_equal [ 1; 2; 3; 4; 5 ]
  @@ sample_random_sequence [ 1 ] ~k:5 test_random_bi_int;

  assert_equal [ 1 ] @@ sample_random_sequence [ 1 ] ~k:1 test_random_bi_int;

  assert_equal [ 1 ] @@ sample_random_sequence [ 1 ] ~k:0 test_random_bi_int;

  assert_equal [ 1; 2; 3 ]
  @@ sample_random_sequence [ 1; 2; 3 ] ~k:5 test_random_bi_int;

  assert_equal [] @@ sample_random_sequence [] ~k:5 test_random_bi_int

let dist_tri_int = make_distribution [ 1; 2; 3; 4; 4; 4; 2; 2; 3; 1 ] ~n:3
let dist_bi_int = make_distribution [ 1; 2; 3; 4; 4; 4; 2; 2; 3; 1 ] ~n:2

let test_sample_random_context_int _ =
  assert_equal [ 1 ] @@ sample_random_context dist_bi_int;
  assert_equal [ 1; 2 ] @@ sample_random_context dist_tri_int

let test_most_frequent_int _ =
  [ 1; 2; 3; 4 ] |> most_freq_ngrams ~n:2 ~k:1 |> assert_equal [ ([ 1; 2 ], 1) ];

  [ 1; 2; 3; 4 ] |> most_freq_ngrams ~n:2 ~k:2
  |> assert_equal [ ([ 1; 2 ], 1); ([ 2; 3 ], 1) ];

  [ 1; 2; 1; 2; 3; 4 ] |> most_freq_ngrams ~n:2 ~k:2
  |> assert_equal [ ([ 1; 2 ], 2); ([ 2; 1 ], 1) ];

  [ 1; 2; 3; 4 ] |> most_freq_ngrams ~n:3 ~k:1
  |> assert_equal [ ([ 1; 2; 3 ], 1) ];

  [ 1; 2; 3; 1; 2; 3; 4 ] |> most_freq_ngrams ~n:3 ~k:1
  |> assert_equal [ ([ 1; 2; 3 ], 2) ]

(* String Tests *)

open Dist (String) (Known_random)

(* Trigram String Tests *)

let test_make_distribution_tri_str _ =
  assert_equal [] @@ (make_distribution ~n:3 [] |> to_list);
  assert_equal [ ([ "1"; "1" ], [ "1" ]) ]
  @@ (make_distribution ~n:3 [ "1"; "1"; "1" ] |> to_list);

  assert_equal
    [
      ([ "1"; "2" ], [ "3" ]); ([ "2"; "3" ], [ "4" ]); ([ "3"; "4" ], [ "5" ]);
    ]
  @@ (make_distribution ~n:3 [ "1"; "2"; "3"; "4"; "5" ] |> to_list);

  assert_equal
    [
      ([ "1"; "2" ], [ "3" ]);
      ([ "2"; "2" ], [ "3" ]);
      ([ "2"; "3" ], [ "1"; "4" ]);
      ([ "3"; "4" ], [ "4" ]);
      ([ "4"; "2" ], [ "2" ]);
      ([ "4"; "4" ], [ "2"; "4" ]);
    ]
  @@ (make_distribution ~n:3
        [ "1"; "2"; "3"; "4"; "4"; "4"; "2"; "2"; "3"; "1" ]
     |> to_list);

  assert_equal
    [
      ([ "1"; "3" ], [ "5" ]);
      ([ "1"; "4" ], [ "6" ]);
      ([ "1"; "6" ], [ "3" ]);
      ([ "2"; "1" ], [ "6"; "3" ]);
      ([ "2"; "4" ], [ "5" ]);
      ([ "3"; "1" ], [ "4" ]);
      ([ "3"; "2" ], [ "4" ]);
      ([ "3"; "5" ], [ "7" ]);
      ([ "4"; "5" ], [ "2" ]);
      ([ "4"; "6" ], [ "7" ]);
      ([ "5"; "2" ], [ "1" ]);
      ([ "5"; "7" ], [ "2" ]);
      ([ "6"; "3" ], [ "1" ]);
      ([ "7"; "2" ], [ "1" ]);
    ]
  @@ (make_distribution ~n:3
        [
          "3";
          "2";
          "4";
          "5";
          "2";
          "1";
          "3";
          "5";
          "7";
          "2";
          "1";
          "6";
          "3";
          "1";
          "4";
          "6";
          "7";
        ]
     |> to_list)

(* Bigram String Tests *)

let test_make_distribution_bi_str _ =
  assert_equal [] @@ (make_distribution ~n:2 [] |> to_list);

  assert_equal [ ([ "1" ], [ "1"; "1" ]) ]
  @@ (make_distribution ~n:2 [ "1"; "1"; "1" ] |> to_list);

  assert_equal
    [
      ([ "1" ], [ "2" ]);
      ([ "2" ], [ "3" ]);
      ([ "3" ], [ "4" ]);
      ([ "4" ], [ "5" ]);
    ]
  @@ (make_distribution ~n:2 [ "1"; "2"; "3"; "4"; "5" ] |> to_list);

  assert_equal
    [
      ([ "1" ], [ "2" ]);
      ([ "2" ], [ "3"; "2"; "3" ]);
      ([ "3" ], [ "1"; "4" ]);
      ([ "4" ], [ "2"; "4"; "4" ]);
    ]
  @@ (make_distribution ~n:2
        [ "1"; "2"; "3"; "4"; "4"; "4"; "2"; "2"; "3"; "1" ]
     |> to_list);

  assert_equal
    [
      ([ "1" ], [ "4"; "6"; "3" ]);
      ([ "2" ], [ "1"; "1"; "4" ]);
      ([ "3" ], [ "1"; "5"; "2" ]);
      ([ "4" ], [ "6"; "5" ]);
      ([ "5" ], [ "7"; "2" ]);
      ([ "6" ], [ "7"; "3" ]);
      ([ "7" ], [ "2" ]);
    ]
  @@ (make_distribution ~n:2
        [
          "3";
          "2";
          "4";
          "5";
          "2";
          "1";
          "3";
          "5";
          "7";
          "2";
          "1";
          "6";
          "3";
          "1";
          "4";
          "6";
          "7";
        ]
     |> to_list)

let test_random_tri_str = make_distribution [ "1"; "2"; "3"; "4"; "5" ] ~n:3
let test_random_bi_str = make_distribution [ "1"; "2"; "3"; "4"; "5" ] ~n:2

let test_sample_random_sequence_str _ =
  assert_equal [ "1"; "2"; "3"; "4"; "5" ]
  @@ sample_random_sequence [ "1"; "2" ] ~k:5 test_random_tri_str;
  assert_equal [ "1"; "2"; "3" ]
  @@ sample_random_sequence [ "1"; "2"; "3" ] ~k:5 test_random_tri_str;
  assert_equal [ "1" ]
  @@ sample_random_sequence [ "1" ] ~k:5 test_random_tri_str;
  assert_equal [] @@ sample_random_sequence [] ~k:5 test_random_tri_str;
  assert_equal [ "1"; "2"; "3"; "4"; "5" ]
  @@ sample_random_sequence [ "1" ] ~k:5 test_random_bi_str;
  assert_equal [ "1"; "2"; "3" ]
  @@ sample_random_sequence [ "1"; "2"; "3" ] ~k:5 test_random_bi_str;
  assert_equal [ "1" ] @@ sample_random_sequence [ "1" ] ~k:1 test_random_bi_str;
  assert_equal [ "1" ] @@ sample_random_sequence [ "1" ] ~k:0 test_random_bi_str;
  assert_equal [] @@ sample_random_sequence [] ~k:5 test_random_bi_str

(* Other String Tests *)
let dist_tri_str =
  make_distribution [ "1"; "2"; "3"; "4"; "4"; "4"; "2"; "2"; "3"; "1" ] ~n:3

let dist_bi_str =
  make_distribution [ "1"; "2"; "3"; "4"; "4"; "4"; "2"; "2"; "3"; "1" ] ~n:2

let test_sample_random_context_str _ =
  assert_equal [ "1" ] @@ sample_random_context dist_bi_str;
  assert_equal [ "1"; "2" ] @@ sample_random_context dist_tri_str

let test_most_frequent_str _ =
  [ "a"; "b"; "c"; "d" ] |> most_freq_ngrams ~n:2 ~k:1
  |> assert_equal [ ([ "a"; "b" ], 1) ];

  [ "a"; "b"; "c"; "d" ] |> most_freq_ngrams ~n:2 ~k:2
  |> assert_equal [ ([ "a"; "b" ], 1); ([ "b"; "c" ], 1) ];

  [ "a"; "b"; "a"; "b"; "c"; "d" ]
  |> most_freq_ngrams ~n:2 ~k:2
  |> assert_equal [ ([ "a"; "b" ], 2); ([ "b"; "a" ], 1) ];

  [ "a"; "b"; "c"; "d" ] |> most_freq_ngrams ~n:3 ~k:1
  |> assert_equal [ ([ "a"; "b"; "c" ], 1) ];

  [ "a"; "b"; "c"; "a"; "b"; "c"; "d" ]
  |> most_freq_ngrams ~n:3 ~k:1
  |> assert_equal [ ([ "a"; "b"; "c" ], 2) ]

let test_sanitize _ =
  assert_equal "abcd" @@ sanitize "AbCD!@#%!@#$*!@#$()}";
  assert_equal "123" @@ sanitize "123!@#($%&!@#)!_";
  assert_equal "abc123" @@ sanitize "!!!!aBc@#($12)3}{[]\n|}"

let test_parse_corpus _ =
  assert_equal [] @@ parse_corpus "";
  assert_equal [ "i"; "am"; "groot" ] @@ parse_corpus "\n i \t am groot";
  assert_equal [ "i"; "am"; "groot" ]
  @@ parse_corpus "\n i !@#!@#() am \t groot";
  assert_equal [ "i"; "am"; "groot" ]
  @@ parse_corpus "\n I !@#!@#() Am \t grOOt"

let test_quickcheck_sanitize _ =
  let is_sanitized (s : string) : bool =
    let chars = List.init (String.length s) ~f:(fun i -> s.[i]) in
    let rec aux (lst : char list) : bool =
      match lst with
      | [] -> true
      | el :: rest ->
          Char.is_alphanum el && Char.( = ) (Char.lowercase el) el && aux rest
    in
    aux chars
  in

  (* Apply sanitize  over the list then check that *)
  Quickcheck.test (List.quickcheck_generator String.quickcheck_generator)
    ~f:(fun res ->
      let results =
        List.map res ~f:(fun input -> input |> sanitize |> is_sanitized)
      in
      let overall_result =
        List.exists results ~f:(fun el -> Bool.( = ) el false)
      in
      assert_bool "Failed sanitize" (Bool.( <> ) true overall_result))

let distribution_tests =
  "Dist tests"
  >::: [
         "make_distribution_tri_int" >:: test_make_distribution_tri_int;
         "make_distribution_bi_int" >:: test_make_distribution_bi_int;
         "sample_random_sequence_int" >:: test_sample_random_sequence_int;
         "sample_random_context_int" >:: test_sample_random_context_int;
         "test_most_frequent_int" >:: test_most_frequent_int;
         "make_distribution_tri_str" >:: test_make_distribution_tri_str;
         "make_distribution_bi_str" >:: test_make_distribution_bi_str;
         "sample_random_sequence_str" >:: test_sample_random_sequence_str;
         "sample_random_context_str" >:: test_sample_random_context_str;
         "test_most_frequent_str" >:: test_most_frequent_str;
         "test_sanitize" >:: test_sanitize;
         "test_parse_corpus" >:: test_parse_corpus;
         "test_quickcheck_sanitize" >:: test_quickcheck_sanitize;
       ]

let series = "Assignment4 Tests" >::: [ distribution_tests ]
let () = run_test_tt_main series
