open! Core
open! Helpers

let program =
  {|
let
  var N := 8

  type intArray = array of int

  var row := intArray [ N ] of 0
  var col := intArray [ N ] of 0

  var diag1 := intArray [N+N-1] of 0
  var diag2 := intArray [N+N-1] of 0
  
  function printboard() =
    (for i := 0 to N - 1 do
      (for j := 0 to N - 1 do
        print(if col[i]=j then " O" else " .");
        print("\n"));
      print("\n"))
  
  function try(c:int) =
    if c = N then printboard()
    else
      (for r := 0 to N - 1 do
        if row[r] = 0 & diag1[r+c] = 0 & diag2[r-c+N-1] = 0 then
          (row[r] := 1; diag1[r+c] := 1; diag2[r-c+N-1] := 1;
           col[c] := r;
          try(c+1);
          row[r] := 0; diag1[r+c] := 0; diag2[r-c+N-1] := 0))
in  
  try(0)
end
|}
;;

let%expect_test "can parse" =
  test_parser program;
  [%expect
    {|
    (ELet
      (declarations (
        (DVariable ((ident N) (type_id ()) (expression (ELiteral (LInt 8)))))
        (DType (intArray (TArray int)))
        (DVariable (
          (ident row)
          (type_id ())
          (expression (
            EArray
            (element_type intArray)
            (size (ELvalue  (LIdent N)))
            (init (ELiteral (LInt   0)))))))
        (DVariable (
          (ident col)
          (type_id ())
          (expression (
            EArray
            (element_type intArray)
            (size (ELvalue  (LIdent N)))
            (init (ELiteral (LInt   0)))))))
        (DVariable (
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
            (init (ELiteral (LInt 0)))))))
        (DVariable (
          (ident diag2)
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
            (init (ELiteral (LInt 0)))))))
        (DFunction (
          (ident printboard)
          (args        ())
          (return_type ())
          (body (
            ESequence (
              (EFor
                (ident i)
                (lo (ELiteral (LInt 0)))
                (hi (
                  EBinary Minus
                  (ELvalue  (LIdent N))
                  (ELiteral (LInt   1))))
                (body (
                  ESequence (
                    (EFor
                      (ident j)
                      (lo (ELiteral (LInt 0)))
                      (hi (
                        EBinary Minus
                        (ELvalue  (LIdent N))
                        (ELiteral (LInt   1))))
                      (body (
                        ECall
                        (func print)
                        (args ((
                          EIf
                          (cond (
                            EBinary Equal
                            (ELvalue (
                              LSubscript (LIdent col) (ELvalue (LIdent i))))
                            (ELvalue (LIdent j))))
                          (then_ (ELiteral (LString " O")))
                          (else_ ((ELiteral (LString " ."))))))))))
                    (ECall (func print) (args ((ELiteral (LString "\\n")))))))))
              (ECall (func print) (args ((ELiteral (LString "\\n"))))))))))
        (DFunction (
          (ident try)
          (args ((c int)))
          (return_type ())
          (body (
            EIf
            (cond (
              EBinary Equal
              (ELvalue (LIdent c))
              (ELvalue (LIdent N))))
            (then_ (ECall (func printboard) (args ())))
            (else_ ((
              ESequence ((
                EFor
                (ident r)
                (lo (ELiteral (LInt 0)))
                (hi (
                  EBinary Minus
                  (ELvalue  (LIdent N))
                  (ELiteral (LInt   1))))
                (body (
                  EIf
                  (cond (
                    EBinary And
                    (EBinary And
                      (EBinary Equal
                        (ELvalue (LSubscript (LIdent row) (ELvalue (LIdent r))))
                        (ELiteral (LInt 0)))
                      (EBinary Equal
                        (ELvalue (
                          LSubscript
                          (LIdent diag1)
                          (EBinary Plus
                            (ELvalue (LIdent r))
                            (ELvalue (LIdent c)))))
                        (ELiteral (LInt 0))))
                    (EBinary Equal
                      (ELvalue (
                        LSubscript
                        (LIdent diag2)
                        (EBinary Minus
                          (EBinary Plus
                            (EBinary Minus
                              (ELvalue (LIdent r))
                              (ELvalue (LIdent c)))
                            (ELvalue (LIdent N)))
                          (ELiteral (LInt 1)))))
                      (ELiteral (LInt 0)))))
                  (then_ (
                    ESequence (
                      (EAssign
                        (LSubscript (LIdent row) (ELvalue (LIdent r)))
                        (ELiteral (LInt 1)))
                      (EAssign
                        (LSubscript
                          (LIdent diag1)
                          (EBinary Plus
                            (ELvalue (LIdent r))
                            (ELvalue (LIdent c))))
                        (ELiteral (LInt 1)))
                      (EAssign
                        (LSubscript
                          (LIdent diag2)
                          (EBinary Minus
                            (EBinary Plus
                              (EBinary Minus
                                (ELvalue (LIdent r))
                                (ELvalue (LIdent c)))
                              (ELvalue (LIdent N)))
                            (ELiteral (LInt 1))))
                        (ELiteral (LInt 1)))
                      (EAssign
                        (LSubscript (LIdent col) (ELvalue (LIdent c)))
                        (ELvalue (LIdent r)))
                      (ECall
                        (func try)
                        (args ((
                          EBinary Plus
                          (ELvalue  (LIdent c))
                          (ELiteral (LInt   1))))))
                      (EAssign
                        (LSubscript (LIdent row) (ELvalue (LIdent r)))
                        (ELiteral (LInt 0)))
                      (EAssign
                        (LSubscript
                          (LIdent diag1)
                          (EBinary Plus
                            (ELvalue (LIdent r))
                            (ELvalue (LIdent c))))
                        (ELiteral (LInt 0)))
                      (EAssign
                        (LSubscript
                          (LIdent diag2)
                          (EBinary Minus
                            (EBinary Plus
                              (EBinary Minus
                                (ELvalue (LIdent r))
                                (ELvalue (LIdent c)))
                              (ELvalue (LIdent N)))
                            (ELiteral (LInt 1))))
                        (ELiteral (LInt 0))))))
                  (else_ ()))))))))))))))
      (exps ((ECall (func try) (args ((ELiteral (LInt 0)))))))) |}]
;;

let%expect_test "execute" =
  Helpers.test_interpreter program;
  [%expect
    {|
     O . . . . . . .
     . . . . O . . .
     . . . . . . . O
     . . . . . O . .
     . . O . . . . .
     . . . . . . O .
     . O . . . . . .
     . . . O . . . .

     O . . . . . . .
     . . . . . O . .
     . . . . . . . O
     . . O . . . . .
     . . . . . . O .
     . . . O . . . .
     . O . . . . . .
     . . . . O . . .

     O . . . . . . .
     . . . . . . O .
     . . . O . . . .
     . . . . . O . .
     . . . . . . . O
     . O . . . . . .
     . . . . O . . .
     . . O . . . . .

     O . . . . . . .
     . . . . . . O .
     . . . . O . . .
     . . . . . . . O
     . O . . . . . .
     . . . O . . . .
     . . . . . O . .
     . . O . . . . .

     . O . . . . . .
     . . . O . . . .
     . . . . . O . .
     . . . . . . . O
     . . O . . . . .
     O . . . . . . .
     . . . . . . O .
     . . . . O . . .

     . O . . . . . .
     . . . . O . . .
     . . . . . . O .
     O . . . . . . .
     . . O . . . . .
     . . . . . . . O
     . . . . . O . .
     . . . O . . . .

     . O . . . . . .
     . . . . O . . .
     . . . . . . O .
     . . . O . . . .
     O . . . . . . .
     . . . . . . . O
     . . . . . O . .
     . . O . . . . .

     . O . . . . . .
     . . . . . O . .
     O . . . . . . .
     . . . . . . O .
     . . . O . . . .
     . . . . . . . O
     . . O . . . . .
     . . . . O . . .

     . O . . . . . .
     . . . . . O . .
     . . . . . . . O
     . . O . . . . .
     O . . . . . . .
     . . . O . . . .
     . . . . . . O .
     . . . . O . . .

     . O . . . . . .
     . . . . . . O .
     . . O . . . . .
     . . . . . O . .
     . . . . . . . O
     . . . . O . . .
     O . . . . . . .
     . . . O . . . .

     . O . . . . . .
     . . . . . . O .
     . . . . O . . .
     . . . . . . . O
     O . . . . . . .
     . . . O . . . .
     . . . . . O . .
     . . O . . . . .

     . O . . . . . .
     . . . . . . . O
     . . . . . O . .
     O . . . . . . .
     . . O . . . . .
     . . . . O . . .
     . . . . . . O .
     . . . O . . . .

     . . O . . . . .
     O . . . . . . .
     . . . . . . O .
     . . . . O . . .
     . . . . . . . O
     . O . . . . . .
     . . . O . . . .
     . . . . . O . .

     . . O . . . . .
     . . . . O . . .
     . O . . . . . .
     . . . . . . . O
     O . . . . . . .
     . . . . . . O .
     . . . O . . . .
     . . . . . O . .

     . . O . . . . .
     . . . . O . . .
     . O . . . . . .
     . . . . . . . O
     . . . . . O . .
     . . . O . . . .
     . . . . . . O .
     O . . . . . . .

     . . O . . . . .
     . . . . O . . .
     . . . . . . O .
     O . . . . . . .
     . . . O . . . .
     . O . . . . . .
     . . . . . . . O
     . . . . . O . .

     . . O . . . . .
     . . . . O . . .
     . . . . . . . O
     . . . O . . . .
     O . . . . . . .
     . . . . . . O .
     . O . . . . . .
     . . . . . O . .

     . . O . . . . .
     . . . . . O . .
     . O . . . . . .
     . . . . O . . .
     . . . . . . . O
     O . . . . . . .
     . . . . . . O .
     . . . O . . . .

     . . O . . . . .
     . . . . . O . .
     . O . . . . . .
     . . . . . . O .
     O . . . . . . .
     . . . O . . . .
     . . . . . . . O
     . . . . O . . .

     . . O . . . . .
     . . . . . O . .
     . O . . . . . .
     . . . . . . O .
     . . . . O . . .
     O . . . . . . .
     . . . . . . . O
     . . . O . . . .

     . . O . . . . .
     . . . . . O . .
     . . . O . . . .
     O . . . . . . .
     . . . . . . . O
     . . . . O . . .
     . . . . . . O .
     . O . . . . . .

     . . O . . . . .
     . . . . . O . .
     . . . O . . . .
     . O . . . . . .
     . . . . . . . O
     . . . . O . . .
     . . . . . . O .
     O . . . . . . .

     . . O . . . . .
     . . . . . O . .
     . . . . . . . O
     O . . . . . . .
     . . . O . . . .
     . . . . . . O .
     . . . . O . . .
     . O . . . . . .

     . . O . . . . .
     . . . . . O . .
     . . . . . . . O
     O . . . . . . .
     . . . . O . . .
     . . . . . . O .
     . O . . . . . .
     . . . O . . . .

     . . O . . . . .
     . . . . . O . .
     . . . . . . . O
     . O . . . . . .
     . . . O . . . .
     O . . . . . . .
     . . . . . . O .
     . . . . O . . .

     . . O . . . . .
     . . . . . . O .
     . O . . . . . .
     . . . . . . . O
     . . . . O . . .
     O . . . . . . .
     . . . O . . . .
     . . . . . O . .

     . . O . . . . .
     . . . . . . O .
     . O . . . . . .
     . . . . . . . O
     . . . . . O . .
     . . . O . . . .
     O . . . . . . .
     . . . . O . . .

     . . O . . . . .
     . . . . . . . O
     . . . O . . . .
     . . . . . . O .
     O . . . . . . .
     . . . . . O . .
     . O . . . . . .
     . . . . O . . .

     . . . O . . . .
     O . . . . . . .
     . . . . O . . .
     . . . . . . . O
     . O . . . . . .
     . . . . . . O .
     . . O . . . . .
     . . . . . O . .

     . . . O . . . .
     O . . . . . . .
     . . . . O . . .
     . . . . . . . O
     . . . . . O . .
     . . O . . . . .
     . . . . . . O .
     . O . . . . . .

     . . . O . . . .
     . O . . . . . .
     . . . . O . . .
     . . . . . . . O
     . . . . . O . .
     O . . . . . . .
     . . O . . . . .
     . . . . . . O .

     . . . O . . . .
     . O . . . . . .
     . . . . . . O .
     . . O . . . . .
     . . . . . O . .
     . . . . . . . O
     O . . . . . . .
     . . . . O . . .

     . . . O . . . .
     . O . . . . . .
     . . . . . . O .
     . . O . . . . .
     . . . . . O . .
     . . . . . . . O
     . . . . O . . .
     O . . . . . . .

     . . . O . . . .
     . O . . . . . .
     . . . . . . O .
     . . . . O . . .
     O . . . . . . .
     . . . . . . . O
     . . . . . O . .
     . . O . . . . .

     . . . O . . . .
     . O . . . . . .
     . . . . . . . O
     . . . . O . . .
     . . . . . . O .
     O . . . . . . .
     . . O . . . . .
     . . . . . O . .

     . . . O . . . .
     . O . . . . . .
     . . . . . . . O
     . . . . . O . .
     O . . . . . . .
     . . O . . . . .
     . . . . O . . .
     . . . . . . O .

     . . . O . . . .
     . . . . . O . .
     O . . . . . . .
     . . . . O . . .
     . O . . . . . .
     . . . . . . . O
     . . O . . . . .
     . . . . . . O .

     . . . O . . . .
     . . . . . O . .
     . . . . . . . O
     . O . . . . . .
     . . . . . . O .
     O . . . . . . .
     . . O . . . . .
     . . . . O . . .

     . . . O . . . .
     . . . . . O . .
     . . . . . . . O
     . . O . . . . .
     O . . . . . . .
     . . . . . . O .
     . . . . O . . .
     . O . . . . . .

     . . . O . . . .
     . . . . . . O .
     O . . . . . . .
     . . . . . . . O
     . . . . O . . .
     . O . . . . . .
     . . . . . O . .
     . . O . . . . .

     . . . O . . . .
     . . . . . . O .
     . . O . . . . .
     . . . . . . . O
     . O . . . . . .
     . . . . O . . .
     O . . . . . . .
     . . . . . O . .

     . . . O . . . .
     . . . . . . O .
     . . . . O . . .
     . O . . . . . .
     . . . . . O . .
     O . . . . . . .
     . . O . . . . .
     . . . . . . . O

     . . . O . . . .
     . . . . . . O .
     . . . . O . . .
     . . O . . . . .
     O . . . . . . .
     . . . . . O . .
     . . . . . . . O
     . O . . . . . .

     . . . O . . . .
     . . . . . . . O
     O . . . . . . .
     . . O . . . . .
     . . . . . O . .
     . O . . . . . .
     . . . . . . O .
     . . . . O . . .

     . . . O . . . .
     . . . . . . . O
     O . . . . . . .
     . . . . O . . .
     . . . . . . O .
     . O . . . . . .
     . . . . . O . .
     . . O . . . . .

     . . . O . . . .
     . . . . . . . O
     . . . . O . . .
     . . O . . . . .
     O . . . . . . .
     . . . . . . O .
     . O . . . . . .
     . . . . . O . .

     . . . . O . . .
     O . . . . . . .
     . . . O . . . .
     . . . . . O . .
     . . . . . . . O
     . O . . . . . .
     . . . . . . O .
     . . O . . . . .

     . . . . O . . .
     O . . . . . . .
     . . . . . . . O
     . . . O . . . .
     . O . . . . . .
     . . . . . . O .
     . . O . . . . .
     . . . . . O . .

     . . . . O . . .
     O . . . . . . .
     . . . . . . . O
     . . . . . O . .
     . . O . . . . .
     . . . . . . O .
     . O . . . . . .
     . . . O . . . .

     . . . . O . . .
     . O . . . . . .
     . . . O . . . .
     . . . . . O . .
     . . . . . . . O
     . . O . . . . .
     O . . . . . . .
     . . . . . . O .

     . . . . O . . .
     . O . . . . . .
     . . . O . . . .
     . . . . . . O .
     . . O . . . . .
     . . . . . . . O
     . . . . . O . .
     O . . . . . . .

     . . . . O . . .
     . O . . . . . .
     . . . . . O . .
     O . . . . . . .
     . . . . . . O .
     . . . O . . . .
     . . . . . . . O
     . . O . . . . .

     . . . . O . . .
     . O . . . . . .
     . . . . . . . O
     O . . . . . . .
     . . . O . . . .
     . . . . . . O .
     . . O . . . . .
     . . . . . O . .

     . . . . O . . .
     . . O . . . . .
     O . . . . . . .
     . . . . . O . .
     . . . . . . . O
     . O . . . . . .
     . . . O . . . .
     . . . . . . O .

     . . . . O . . .
     . . O . . . . .
     O . . . . . . .
     . . . . . . O .
     . O . . . . . .
     . . . . . . . O
     . . . . . O . .
     . . . O . . . .

     . . . . O . . .
     . . O . . . . .
     . . . . . . . O
     . . . O . . . .
     . . . . . . O .
     O . . . . . . .
     . . . . . O . .
     . O . . . . . .

     . . . . O . . .
     . . . . . . O .
     O . . . . . . .
     . . O . . . . .
     . . . . . . . O
     . . . . . O . .
     . . . O . . . .
     . O . . . . . .

     . . . . O . . .
     . . . . . . O .
     O . . . . . . .
     . . . O . . . .
     . O . . . . . .
     . . . . . . . O
     . . . . . O . .
     . . O . . . . .

     . . . . O . . .
     . . . . . . O .
     . O . . . . . .
     . . . O . . . .
     . . . . . . . O
     O . . . . . . .
     . . O . . . . .
     . . . . . O . .

     . . . . O . . .
     . . . . . . O .
     . O . . . . . .
     . . . . . O . .
     . . O . . . . .
     O . . . . . . .
     . . . O . . . .
     . . . . . . . O

     . . . . O . . .
     . . . . . . O .
     . O . . . . . .
     . . . . . O . .
     . . O . . . . .
     O . . . . . . .
     . . . . . . . O
     . . . O . . . .

     . . . . O . . .
     . . . . . . O .
     . . . O . . . .
     O . . . . . . .
     . . O . . . . .
     . . . . . . . O
     . . . . . O . .
     . O . . . . . .

     . . . . O . . .
     . . . . . . . O
     . . . O . . . .
     O . . . . . . .
     . . O . . . . .
     . . . . . O . .
     . O . . . . . .
     . . . . . . O .

     . . . . O . . .
     . . . . . . . O
     . . . O . . . .
     O . . . . . . .
     . . . . . . O .
     . O . . . . . .
     . . . . . O . .
     . . O . . . . .

     . . . . . O . .
     O . . . . . . .
     . . . . O . . .
     . O . . . . . .
     . . . . . . . O
     . . O . . . . .
     . . . . . . O .
     . . . O . . . .

     . . . . . O . .
     . O . . . . . .
     . . . . . . O .
     O . . . . . . .
     . . O . . . . .
     . . . . O . . .
     . . . . . . . O
     . . . O . . . .

     . . . . . O . .
     . O . . . . . .
     . . . . . . O .
     O . . . . . . .
     . . . O . . . .
     . . . . . . . O
     . . . . O . . .
     . . O . . . . .

     . . . . . O . .
     . . O . . . . .
     O . . . . . . .
     . . . . . . O .
     . . . . O . . .
     . . . . . . . O
     . O . . . . . .
     . . . O . . . .

     . . . . . O . .
     . . O . . . . .
     O . . . . . . .
     . . . . . . . O
     . . . O . . . .
     . O . . . . . .
     . . . . . . O .
     . . . . O . . .

     . . . . . O . .
     . . O . . . . .
     O . . . . . . .
     . . . . . . . O
     . . . . O . . .
     . O . . . . . .
     . . . O . . . .
     . . . . . . O .

     . . . . . O . .
     . . O . . . . .
     . . . . O . . .
     . . . . . . O .
     O . . . . . . .
     . . . O . . . .
     . O . . . . . .
     . . . . . . . O

     . . . . . O . .
     . . O . . . . .
     . . . . O . . .
     . . . . . . . O
     O . . . . . . .
     . . . O . . . .
     . O . . . . . .
     . . . . . . O .

     . . . . . O . .
     . . O . . . . .
     . . . . . . O .
     . O . . . . . .
     . . . O . . . .
     . . . . . . . O
     O . . . . . . .
     . . . . O . . .

     . . . . . O . .
     . . O . . . . .
     . . . . . . O .
     . O . . . . . .
     . . . . . . . O
     . . . . O . . .
     O . . . . . . .
     . . . O . . . .

     . . . . . O . .
     . . O . . . . .
     . . . . . . O .
     . . . O . . . .
     O . . . . . . .
     . . . . . . . O
     . O . . . . . .
     . . . . O . . .

     . . . . . O . .
     . . . O . . . .
     O . . . . . . .
     . . . . O . . .
     . . . . . . . O
     . O . . . . . .
     . . . . . . O .
     . . O . . . . .

     . . . . . O . .
     . . . O . . . .
     . O . . . . . .
     . . . . . . . O
     . . . . O . . .
     . . . . . . O .
     O . . . . . . .
     . . O . . . . .

     . . . . . O . .
     . . . O . . . .
     . . . . . . O .
     O . . . . . . .
     . . O . . . . .
     . . . . O . . .
     . O . . . . . .
     . . . . . . . O

     . . . . . O . .
     . . . O . . . .
     . . . . . . O .
     O . . . . . . .
     . . . . . . . O
     . O . . . . . .
     . . . . O . . .
     . . O . . . . .

     . . . . . O . .
     . . . . . . . O
     . O . . . . . .
     . . . O . . . .
     O . . . . . . .
     . . . . . . O .
     . . . . O . . .
     . . O . . . . .

     . . . . . . O .
     O . . . . . . .
     . . O . . . . .
     . . . . . . . O
     . . . . . O . .
     . . . O . . . .
     . O . . . . . .
     . . . . O . . .

     . . . . . . O .
     . O . . . . . .
     . . . O . . . .
     O . . . . . . .
     . . . . . . . O
     . . . . O . . .
     . . O . . . . .
     . . . . . O . .

     . . . . . . O .
     . O . . . . . .
     . . . . . O . .
     . . O . . . . .
     O . . . . . . .
     . . . O . . . .
     . . . . . . . O
     . . . . O . . .

     . . . . . . O .
     . . O . . . . .
     O . . . . . . .
     . . . . . O . .
     . . . . . . . O
     . . . . O . . .
     . O . . . . . .
     . . . O . . . .

     . . . . . . O .
     . . O . . . . .
     . . . . . . . O
     . O . . . . . .
     . . . . O . . .
     O . . . . . . .
     . . . . . O . .
     . . . O . . . .

     . . . . . . O .
     . . . O . . . .
     . O . . . . . .
     . . . . O . . .
     . . . . . . . O
     O . . . . . . .
     . . O . . . . .
     . . . . . O . .

     . . . . . . O .
     . . . O . . . .
     . O . . . . . .
     . . . . . . . O
     . . . . . O . .
     O . . . . . . .
     . . O . . . . .
     . . . . O . . .

     . . . . . . O .
     . . . . O . . .
     . . O . . . . .
     O . . . . . . .
     . . . . . O . .
     . . . . . . . O
     . O . . . . . .
     . . . O . . . .

     . . . . . . . O
     . O . . . . . .
     . . . O . . . .
     O . . . . . . .
     . . . . . . O .
     . . . . O . . .
     . . O . . . . .
     . . . . . O . .

     . . . . . . . O
     . O . . . . . .
     . . . . O . . .
     . . O . . . . .
     O . . . . . . .
     . . . . . . O .
     . . . O . . . .
     . . . . . O . .

     . . . . . . . O
     . . O . . . . .
     O . . . . . . .
     . . . . . O . .
     . O . . . . . .
     . . . . O . . .
     . . . . . . O .
     . . . O . . . .

     . . . . . . . O
     . . . O . . . .
     O . . . . . . .
     . . O . . . . .
     . . . . . O . .
     . O . . . . . .
     . . . . . . O .
     . . . . O . . .

    Unit |}]
;;
