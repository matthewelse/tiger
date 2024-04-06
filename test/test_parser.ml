open! Core

let test str =
  let lexer = Lexing.from_string str in
  let decs = Tiger.Parser.decs Tiger.Lexer.read lexer in
  print_s ([%sexp_of: Tiger.Ast.decs] decs)
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
     (DVar ((ident x) (type_id ()) (expression ENil)))) |}];
  test "function treeLeaves(t : tree) = nil";
  [%expect
    {|
    ((DFunc
      ((ident treeLeaves) (fields ((t tree))) (return_type ()) (body ENil)))) |}];
  test "function treeLeaves(t : tree) : asdf = nil";
  [%expect
    {|
    ((DFunc
      ((ident treeLeaves) (fields ((t tree))) (return_type (asdf)) (body ENil)))) |}];
  test "var x = 1 + 2 * 3";
  [%expect
    {|
    ((DVar
      ((ident x) (type_id ())
       (expression
        (EBinary Plus (EConst (CInt 1))
         (EBinary Times (EConst (CInt 2)) (EConst (CInt 3)))))))) |}];
  test "var x = (a = b) = c";
  [%expect
    {|
    ((DVar
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
    ((DVar
      ((ident x) (type_id ())
       (expression
        (EBinary Divide (EBinary Times (ELvalue (LIdent c)) (ELvalue (LIdent d)))
         (ELvalue (LIdent e))))))) |}];
  test "var x = a + b - c * d / e";
  [%expect
    {|
    ((DVar
      ((ident x) (type_id ())
       (expression
        (EBinary Minus (EBinary Plus (ELvalue (LIdent a)) (ELvalue (LIdent b)))
         (EBinary Divide
          (EBinary Times (ELvalue (LIdent c)) (ELvalue (LIdent d)))
          (ELvalue (LIdent e)))))))) |}];
  test "var x = a + b - c * d / e = f & g | h";
  [%expect
    {|
    ((DVar
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
    ((DVar
      ((ident x) (type_id ())
       (expression
        (ERecord ((asdf (EConst (CInt 1234))) (defg (EConst (CInt 999))))))))) |}];
  test "var x = { asdf = 1234 }";
  [%expect
    {|
    ((DVar
      ((ident x) (type_id ())
       (expression (ERecord ((asdf (EConst (CInt 1234))))))))) |}]
;;
