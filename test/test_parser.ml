open! Core
open! Helpers

let%expect_test "asdf" =
  test_parser {| (row[r] := 1) |};
  [%expect
    {|
    (ESequence ((
      EAssign (LSubscript (LIdent row) (ELvalue (LIdent r))) (ELiteral (LInt 1))))) |}]
;;

let test_parser s = test_parser [%string "let %{s} in () end"]

let%expect_test _ =
  (* test_parser "type a = b"; *)
  [%expect {||}];
  test_parser "type a = { x : int }";
  [%expect
    {|
    (ELet
      (declarations ((DType (a (TRecord ((x int)))))))
      (exps ((ESequence ())))) |}];
  test_parser "type a = { x : int }   var x := nil";
  [%expect
    {|
    (ELet
      (declarations (
        (DType (a (TRecord ((x int)))))
        (DVariable ((ident x) (type_id ()) (expression ENil)))))
      (exps ((ESequence ())))) |}];
  test_parser "function treeLeaves(t : tree) = nil";
  [%expect
    {|
    (ELet
      (declarations ((
        DFunction (
          (ident treeLeaves) (fields ((t tree))) (return_type ()) (body ENil)))))
      (exps ((ESequence ())))) |}];
  test_parser "function treeLeaves(t : tree) : asdf = nil";
  [%expect
    {|
    (ELet
      (declarations ((
        DFunction (
          (ident treeLeaves) (fields ((t tree))) (return_type (asdf)) (body ENil)))))
      (exps ((ESequence ())))) |}];
  test_parser "var x := 1 + 2 * 3";
  [%expect
    {|
    (ELet
      (declarations ((
        DVariable (
          (ident x)
          (type_id ())
          (expression (
            EBinary Plus
            (ELiteral (LInt 1))
            (EBinary Times
              (ELiteral (LInt 2))
              (ELiteral (LInt 3)))))))))
      (exps ((ESequence ())))) |}];
  test_parser "var x := (a = b) = c";
  [%expect
    {|
    (ELet
      (declarations ((
        DVariable (
          (ident x)
          (type_id ())
          (expression (
            EBinary Equal
            (ESequence ((
              EBinary Equal
              (ELvalue (LIdent a))
              (ELvalue (LIdent b)))))
            (ELvalue (LIdent c))))))))
      (exps ((ESequence ())))) |}];
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
    (ELet
      (declarations ((
        DVariable (
          (ident x)
          (type_id ())
          (expression (
            EBinary Divide
            (EBinary Times
              (ELvalue (LIdent c))
              (ELvalue (LIdent d)))
            (ELvalue (LIdent e))))))))
      (exps ((ESequence ())))) |}];
  test_parser "var x := a + b - c * d / e";
  [%expect
    {|
    (ELet
      (declarations ((
        DVariable (
          (ident x)
          (type_id ())
          (expression (
            EBinary Minus
            (EBinary Plus
              (ELvalue (LIdent a))
              (ELvalue (LIdent b)))
            (EBinary Divide
              (EBinary Times
                (ELvalue (LIdent c))
                (ELvalue (LIdent d)))
              (ELvalue (LIdent e)))))))))
      (exps ((ESequence ())))) |}];
  test_parser "var x := a + b - c * d / e = f & g | h";
  [%expect
    {|
    (ELet
      (declarations ((
        DVariable (
          (ident x)
          (type_id ())
          (expression (
            EBinary Or
            (EBinary And
              (EBinary Equal
                (EBinary Minus
                  (EBinary Plus
                    (ELvalue (LIdent a))
                    (ELvalue (LIdent b)))
                  (EBinary Divide
                    (EBinary Times
                      (ELvalue (LIdent c))
                      (ELvalue (LIdent d)))
                    (ELvalue (LIdent e))))
                (ELvalue (LIdent f)))
              (ELvalue (LIdent g)))
            (ELvalue (LIdent h))))))))
      (exps ((ESequence ())))) |}];
  test_parser "var x := record { asdf = 1234, defg = 999 }";
  [%expect
    {|
    (ELet
      (declarations ((
        DVariable (
          (ident x)
          (type_id ())
          (expression (
            ERecord record (
              (asdf (ELiteral (LInt 1234)))
              (defg (ELiteral (LInt 999))))))))))
      (exps ((ESequence ())))) |}];
  test_parser "var x := record { asdf = 1234 }";
  [%expect
    {|
    (ELet
      (declarations ((
        DVariable (
          (ident x)
          (type_id ())
          (expression (ERecord record ((asdf (ELiteral (LInt 1234))))))))))
      (exps ((ESequence ())))) |}];
  test_parser "var diag1 := intArray [N+N-1] of 0";
  [%expect
    {|
    (ELet
      (declarations ((
        DVariable (
          (ident diag1)
          (type_id ())
          (expression (
            EArray
            (element_type intArray)
            (size (
              EBinary Minus
              (EBinary Plus
                (ELvalue (LIdent N))
                (ELvalue (LIdent N)))
              (ELiteral (LInt 1))))
            (init (ELiteral (LInt 0)))))))))
      (exps ((ESequence ())))) |}];
  test_parser "var diag1 := intArray [-1] of 0";
  [%expect
    {|
    (ELet
      (declarations ((
        DVariable (
          (ident diag1)
          (type_id ())
          (expression (
            EArray
            (element_type intArray)
            (size (ENegative (ELiteral (LInt 1))))
            (init (ELiteral (LInt 0)))))))))
      (exps ((ESequence ())))) |}];
  test_parser "var x := print(\"asdf\")";
  [%expect
    {|
    (ELet
      (declarations ((
        DVariable (
          (ident x)
          (type_id ())
          (expression (ECall (func print) (args ((ELiteral (LString asdf))))))))))
      (exps ((ESequence ())))) |}];
  test_parser "var x := col[i]";
  [%expect
    {|
    (ELet
      (declarations ((
        DVariable (
          (ident x)
          (type_id ())
          (expression (ELvalue (LSubscript (LIdent col) (ELvalue (LIdent i)))))))))
      (exps ((ESequence ())))) |}];
  test_parser "var x := col[i]=j";
  [%expect
    {|
    (ELet
      (declarations ((
        DVariable (
          (ident x)
          (type_id ())
          (expression (
            EBinary Equal
            (ELvalue (LSubscript (LIdent col) (ELvalue (LIdent i))))
            (ELvalue (LIdent j))))))))
      (exps ((ESequence ())))) |}];
  test_parser "var x := print(if col[i]=j then \" O\" else \" .\")";
  [%expect
    {|
    (ELet
      (declarations ((
        DVariable (
          (ident x)
          (type_id ())
          (expression (
            ECall
            (func print)
            (args ((
              EIf
              (cond (
                EBinary Equal
                (ELvalue (LSubscript (LIdent col) (ELvalue (LIdent i))))
                (ELvalue (LIdent j))))
              (then_ (ELiteral (LString " O")))
              (else_ ((ELiteral (LString " .")))))))))))))
      (exps ((ESequence ())))) |}]
;;
