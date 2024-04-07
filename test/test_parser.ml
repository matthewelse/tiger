open! Core
open! Helpers

let%expect_test "asdf" =
  test_parser {| (row[r] := 1) |};
  [%expect
    {|
    (Sequence ((
      Assign (Subscript (Ident row) (Lvalue (Ident r))) (Literal (Int 1))))) |}]
;;

let test_parser s = test_parser [%string "let %{s} in () end"]

let%expect_test _ =
  (* test_parser "type a = b"; *)
  [%expect {||}];
  test_parser "type a = { x : int }";
  [%expect
    {|
    (Let
      (declarations ((Type ((name a) (desc (Record ((x int))))))))
      (exps ((Sequence ())))) |}];
  test_parser "type a = { x : int }   var x := nil";
  [%expect
    {|
    (Let
      (declarations (
        (Type ((name a) (desc (Record ((x int))))))
        (Variable ((ident x) (type_id ()) (expression Nil)))))
      (exps ((Sequence ())))) |}];
  test_parser "function treeLeaves(t : tree) = nil";
  [%expect
    {|
    (Let
      (declarations ((
        Function (
          (ident treeLeaves) (args ((t tree))) (return_type ()) (body Nil)))))
      (exps ((Sequence ())))) |}];
  test_parser "function treeLeaves(t : tree) : asdf = nil";
  [%expect
    {|
    (Let
      (declarations ((
        Function (
          (ident treeLeaves) (args ((t tree))) (return_type (asdf)) (body Nil)))))
      (exps ((Sequence ())))) |}];
  test_parser "var x := 1 + 2 * 3";
  [%expect
    {|
    (Let
      (declarations ((
        Variable (
          (ident x)
          (type_id ())
          (expression (
            Binary Plus
            (Literal (Int 1))
            (Binary Times
              (Literal (Int 2))
              (Literal (Int 3)))))))))
      (exps ((Sequence ())))) |}];
  test_parser "var x := (a = b) = c";
  [%expect
    {|
    (Let
      (declarations ((
        Variable (
          (ident x)
          (type_id ())
          (expression (
            Binary Equal
            (Sequence ((
              Binary Equal
              (Lvalue (Ident a))
              (Lvalue (Ident b)))))
            (Lvalue (Ident c))))))))
      (exps ((Sequence ())))) |}];
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    test_parser "var x := (a = b = c");
  [%expect
    {|
      ("Parser error while processing token"
        (error    "after 'b' and before '='")
        (position :1:20)) |}];
  test_parser "var x := c * d / e";
  [%expect
    {|
    (Let
      (declarations ((
        Variable (
          (ident x)
          (type_id ())
          (expression (
            Binary Divide
            (Binary Times
              (Lvalue (Ident c))
              (Lvalue (Ident d)))
            (Lvalue (Ident e))))))))
      (exps ((Sequence ())))) |}];
  test_parser "var x := a + b - c * d / e";
  [%expect
    {|
    (Let
      (declarations ((
        Variable (
          (ident x)
          (type_id ())
          (expression (
            Binary Minus
            (Binary Plus
              (Lvalue (Ident a))
              (Lvalue (Ident b)))
            (Binary Divide
              (Binary Times
                (Lvalue (Ident c))
                (Lvalue (Ident d)))
              (Lvalue (Ident e)))))))))
      (exps ((Sequence ())))) |}];
  test_parser "var x := a + b - c * d / e = f & g | h";
  [%expect
    {|
    (Let
      (declarations ((
        Variable (
          (ident x)
          (type_id ())
          (expression (
            Binary Or
            (Binary And
              (Binary Equal
                (Binary Minus
                  (Binary Plus
                    (Lvalue (Ident a))
                    (Lvalue (Ident b)))
                  (Binary Divide
                    (Binary Times
                      (Lvalue (Ident c))
                      (Lvalue (Ident d)))
                    (Lvalue (Ident e))))
                (Lvalue (Ident f)))
              (Lvalue (Ident g)))
            (Lvalue (Ident h))))))))
      (exps ((Sequence ())))) |}];
  test_parser "var x := record { asdf = 1234, defg = 999 }";
  [%expect
    {|
    (Let
      (declarations ((
        Variable (
          (ident x)
          (type_id ())
          (expression (
            Record record (
              (asdf (Literal (Int 1234)))
              (defg (Literal (Int 999))))))))))
      (exps ((Sequence ())))) |}];
  test_parser "var x := record { asdf = 1234 }";
  [%expect
    {|
    (Let
      (declarations ((
        Variable (
          (ident x)
          (type_id ())
          (expression (Record record ((asdf (Literal (Int 1234))))))))))
      (exps ((Sequence ())))) |}];
  test_parser "var diag1 := intArray [N+N-1] of 0";
  [%expect
    {|
    (Let
      (declarations ((
        Variable (
          (ident diag1)
          (type_id ())
          (expression (
            Array
            (element_type intArray)
            (size (
              Binary Minus
              (Binary Plus
                (Lvalue (Ident N))
                (Lvalue (Ident N)))
              (Literal (Int 1))))
            (init (Literal (Int 0)))))))))
      (exps ((Sequence ())))) |}];
  test_parser "var diag1 := intArray [-1] of 0";
  [%expect
    {|
    (Let
      (declarations ((
        Variable (
          (ident diag1)
          (type_id ())
          (expression (
            Array
            (element_type intArray)
            (size (Negative (Literal (Int 1))))
            (init (Literal (Int 0)))))))))
      (exps ((Sequence ())))) |}];
  test_parser "var x := print(\"asdf\")";
  [%expect
    {|
    (Let
      (declarations ((
        Variable (
          (ident x)
          (type_id ())
          (expression (Call (func print) (args ((Literal (String asdf))))))))))
      (exps ((Sequence ())))) |}];
  test_parser "var x := col[i]";
  [%expect
    {|
    (Let
      (declarations ((
        Variable (
          (ident x)
          (type_id ())
          (expression (Lvalue (Subscript (Ident col) (Lvalue (Ident i)))))))))
      (exps ((Sequence ())))) |}];
  test_parser "var x := col[i]=j";
  [%expect
    {|
    (Let
      (declarations ((
        Variable (
          (ident x)
          (type_id ())
          (expression (
            Binary Equal
            (Lvalue (Subscript (Ident col) (Lvalue (Ident i))))
            (Lvalue (Ident j))))))))
      (exps ((Sequence ())))) |}];
  test_parser "var x := print(if col[i]=j then \" O\" else \" .\")";
  [%expect
    {|
    (Let
      (declarations ((
        Variable (
          (ident x)
          (type_id ())
          (expression (
            Call
            (func print)
            (args ((
              If
              (cond (
                Binary Equal
                (Lvalue (Subscript (Ident col) (Lvalue (Ident i))))
                (Lvalue (Ident j))))
              (then_ (Literal (String " O")))
              (else_ ((Literal (String " .")))))))))))))
      (exps ((Sequence ())))) |}]
;;
