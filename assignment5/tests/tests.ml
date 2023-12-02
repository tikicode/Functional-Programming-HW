open Core
open OUnit2
open Monads

let test_are_balanced_monadic _ = 
  assert_equal true @@ are_balanced_monadic "()";
  assert_equal false @@ are_balanced_monadic "("

let test_are_balanced_more_monadic _ =
  assert_equal true @@ are_balanced_more_monadic "()";
  assert_equal false @@ are_balanced_more_monadic "("

let monad_tests =
  "Monad tests"
  >::: [
         "are_balanced_monadic" >:: test_are_balanced_monadic;
         "are_balanced_more_monadic" >:: test_are_balanced_more_monadic;
       ]

let series = "Assignment5 Tests" >::: [ monad_tests ]
let () = run_test_tt_main series
