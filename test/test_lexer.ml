open! Core
open! Helpers

let%expect_test "correctly handle negative integers" =
  test_lexer "var x := N-1";
  [%expect {|
    (Var (Ident x) Assign (Ident N) Minus (Int 1)) |}];
  test_lexer "\" O\"";
  [%expect {| ((String " O")) |}];
  test_lexer "let type a = b in 1234 end";
  [%expect {| (Let Type (Ident a) Equal (Ident b) In (Int 1234) End) |}]
;;
