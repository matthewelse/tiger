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
    (Let
      (declarations (
        (Variable ((ident N) (type_id ()) (expression (Literal (Int 8)))))
        (Type ((name intArray) (desc (Array int))))
        (Variable (
          (ident row)
          (type_id ())
          (expression (
            Array
            (element_type intArray)
            (size (Lvalue  (Ident N)))
            (init (Literal (Int   0)))))))
        (Variable (
          (ident col)
          (type_id ())
          (expression (
            Array
            (element_type intArray)
            (size (Lvalue  (Ident N)))
            (init (Literal (Int   0)))))))
        (Variable (
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
            (init (Literal (Int 0)))))))
        (Variable (
          (ident diag2)
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
            (init (Literal (Int 0)))))))
        (Function (
          (ident printboard)
          (args        ())
          (return_type ())
          (body (
            Sequence (
              (For
                (ident i)
                (lo (Literal (Int 0)))
                (hi (
                  Binary Minus
                  (Lvalue  (Ident N))
                  (Literal (Int   1))))
                (body (
                  Sequence (
                    (For
                      (ident j)
                      (lo (Literal (Int 0)))
                      (hi (
                        Binary Minus
                        (Lvalue  (Ident N))
                        (Literal (Int   1))))
                      (body (
                        Call
                        (func print)
                        (args ((
                          If
                          (cond (
                            Binary Equal
                            (Lvalue (Subscript (Ident col) (Lvalue (Ident i))))
                            (Lvalue (Ident j))))
                          (then_ (Literal (String " O")))
                          (else_ ((Literal (String " ."))))))))))
                    (Call (func print) (args ((Literal (String "\\n")))))))))
              (Call (func print) (args ((Literal (String "\\n"))))))))))
        (Function (
          (ident try)
          (args ((c int)))
          (return_type ())
          (body (
            If
            (cond (
              Binary Equal
              (Lvalue (Ident c))
              (Lvalue (Ident N))))
            (then_ (Call (func printboard) (args ())))
            (else_ ((
              Sequence ((
                For
                (ident r)
                (lo (Literal (Int 0)))
                (hi (
                  Binary Minus
                  (Lvalue  (Ident N))
                  (Literal (Int   1))))
                (body (
                  If
                  (cond (
                    Binary And
                    (Binary And
                      (Binary Equal
                        (Lvalue (Subscript (Ident row) (Lvalue (Ident r))))
                        (Literal (Int 0)))
                      (Binary Equal
                        (Lvalue (
                          Subscript
                          (Ident diag1)
                          (Binary Plus
                            (Lvalue (Ident r))
                            (Lvalue (Ident c)))))
                        (Literal (Int 0))))
                    (Binary Equal
                      (Lvalue (
                        Subscript
                        (Ident diag2)
                        (Binary Minus
                          (Binary Plus
                            (Binary Minus
                              (Lvalue (Ident r))
                              (Lvalue (Ident c)))
                            (Lvalue (Ident N)))
                          (Literal (Int 1)))))
                      (Literal (Int 0)))))
                  (then_ (
                    Sequence (
                      (Assign
                        (Subscript (Ident row) (Lvalue (Ident r)))
                        (Literal (Int 1)))
                      (Assign
                        (Subscript
                          (Ident diag1)
                          (Binary Plus
                            (Lvalue (Ident r))
                            (Lvalue (Ident c))))
                        (Literal (Int 1)))
                      (Assign
                        (Subscript
                          (Ident diag2)
                          (Binary Minus
                            (Binary Plus
                              (Binary Minus
                                (Lvalue (Ident r))
                                (Lvalue (Ident c)))
                              (Lvalue (Ident N)))
                            (Literal (Int 1))))
                        (Literal (Int 1)))
                      (Assign
                        (Subscript (Ident col) (Lvalue (Ident c)))
                        (Lvalue (Ident r)))
                      (Call
                        (func try)
                        (args ((
                          Binary Plus
                          (Lvalue  (Ident c))
                          (Literal (Int   1))))))
                      (Assign
                        (Subscript (Ident row) (Lvalue (Ident r)))
                        (Literal (Int 0)))
                      (Assign
                        (Subscript
                          (Ident diag1)
                          (Binary Plus
                            (Lvalue (Ident r))
                            (Lvalue (Ident c))))
                        (Literal (Int 0)))
                      (Assign
                        (Subscript
                          (Ident diag2)
                          (Binary Minus
                            (Binary Plus
                              (Binary Minus
                                (Lvalue (Ident r))
                                (Lvalue (Ident c)))
                              (Lvalue (Ident N)))
                            (Literal (Int 1))))
                        (Literal (Int 0))))))
                  (else_ ()))))))))))))))
      (exps ((Call (func try) (args ((Literal (Int 0)))))))) |}]
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
