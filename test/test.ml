
open OUnit
open Big_int

let _ :: _ | [] = 
  run_test_tt_main
    ("Digits">::
     (fun () ->
        let assert_digits = 
          assert_equal ~printer:Digits.to_string
        in
          assert_digits
            []
            (Digits.of_int 0);
          assert_digits
            [1; 2; 3; 4]
            (Digits.of_int 1234);
          assert_digits
            [1; 2; 3; 4]
            (Digits.of_int (-1234));
          assert_digits
            [1; 0; 0; 1; 1; 0; 1; 0; 0; 1; 0]
            (Digits.of_int ~base:2 1234);
          assert_digits
            [4; 13; 2]
            (Digits.of_int ~base:16 1234);
          assert_digits
            [1; 9; 6]
            (DigitsBig_int.of_big_int (big_int_of_int 196));
     ))
