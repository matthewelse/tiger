open! Core

let test str =
  let lexer = Lexing.from_string str in
  let decs = Tiger.Parser.declarations Tiger.Lexer.read lexer in
  print_s ([%sexp_of: Tiger.Ast.declarations] decs)
;;

let%expect_test _ =
  test "type a = b";
  [%expect {| ((DType (a (TIdent b)))) |}];
  test "type a = { x : int }";
  [%expect {| ((DType (a (TRecord ((x int)))))) |}];
  test "type a = { x : int }   var x = nil";
  [%expect
    {|
    ((DType (a (TRecord ((x int)))))
     (DVariable ((ident x) (type_id ()) (expression ENil)))) |}];
  test "function treeLeaves(t : tree) = nil";
  [%expect
    {|
    ((DFunction
      ((ident treeLeaves) (fields ((t tree))) (return_type ()) (body ENil)))) |}];
  test "function treeLeaves(t : tree) : asdf = nil";
  [%expect
    {|
    ((DFunction
      ((ident treeLeaves) (fields ((t tree))) (return_type (asdf)) (body ENil)))) |}];
  test "var x = 1 + 2 * 3";
  [%expect
    {|
    ((DVariable
      ((ident x) (type_id ())
       (expression
        (EBinary Plus (ELiteral (LInt 1))
         (EBinary Times (ELiteral (LInt 2)) (ELiteral (LInt 3)))))))) |}];
  test "var x = (a = b) = c";
  [%expect
    {|
    ((DVariable
      ((ident x) (type_id ())
       (expression
        (EBinary Equal
         (ESequence ((EBinary Equal (ELvalue (LIdent a)) (ELvalue (LIdent b)))))
         (ELvalue (LIdent c))))))) |}];
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    test "var x = (a = b = c");
  [%expect {| (Tiger__Parser0.MenhirBasics.Error) |}];
  test "var x = c * d / e";
  [%expect
    {|
    ((DVariable
      ((ident x) (type_id ())
       (expression
        (EBinary Divide (EBinary Times (ELvalue (LIdent c)) (ELvalue (LIdent d)))
         (ELvalue (LIdent e))))))) |}];
  test "var x = a + b - c * d / e";
  [%expect
    {|
    ((DVariable
      ((ident x) (type_id ())
       (expression
        (EBinary Minus (EBinary Plus (ELvalue (LIdent a)) (ELvalue (LIdent b)))
         (EBinary Divide
          (EBinary Times (ELvalue (LIdent c)) (ELvalue (LIdent d)))
          (ELvalue (LIdent e)))))))) |}];
  test "var x = a + b - c * d / e = f & g | h";
  [%expect
    {|
    ((DVariable
      ((ident x) (type_id ())
       (expression
        (EBinary Or
         (EBinary And
          (EBinary Equal
           (EBinary Minus
            (EBinary Plus (ELvalue (LIdent a)) (ELvalue (LIdent b)))
            (EBinary Divide
             (EBinary Times (ELvalue (LIdent c)) (ELvalue (LIdent d)))
             (ELvalue (LIdent e))))
           (ELvalue (LIdent f)))
          (ELvalue (LIdent g)))
         (ELvalue (LIdent h))))))) |}];
  test "var x = { asdf = 1234, defg = 999 }";
  [%expect
    {|
    ((DVariable
      ((ident x) (type_id ())
       (expression
        (ERecord ((asdf (ELiteral (LInt 1234))) (defg (ELiteral (LInt 999))))))))) |}];
  test "var x = { asdf = 1234 }";
  [%expect
    {|
    ((DVariable
      ((ident x) (type_id ())
       (expression (ERecord ((asdf (ELiteral (LInt 1234))))))))) |}]
;;
