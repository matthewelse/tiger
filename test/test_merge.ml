open! Core
open! Helpers

let program =
  {|
let
  type any = { any : int }

  var buffer := getchar()

  function readint(any : any) : int =
    let var i := 0
        function isdigit(s : string) : int =
          ord(s) >= ord("0") & ord(s) <= ord("9")
    in
      while buffer=" " | buffer = "\n" do buffer := getchar();
      any.any := isdigit(buffer);
      while isdigit(buffer)
        do (
            i := i * 10 + ord(buffer) - ord("0");
            buffer := getchar());
      i
    end
  
  type list = { first : int , rest : list }

  function readlist() : list =
    let var any := any{any=0}
        var i := reading(any)
    in
      if any.any then
        list { first = i, rest = readlist() }
      else (buffer := getchar(); nil)
    end
  
  function merge (a : list, b : list) : list =
    if a = nil then b
    else if b = nil then a
    else if a.first < b.first then
      list { first = a.first, rest = merge(a.rest, b) }
    else
      list { first = b.first, rest = merge(a, b.rest) }
  
  function printint(i : int) =
    let function f(i : int) = if i > 0 then (f(i / 10); putchar(i - i / 10 * 10 + ord("0")))
    in
      if i < 0 then (print("-"); f(-i))
      else if i > 0 then f(i)
      else print("0")
    end
  
  function printlist(l : list) =
    if l = nil then ()
    else (printint(l.first); print(" "); printlist(l.rest))
in  
  printlist(merge(readlist(), readlist()))
end
|}
;;

let%expect_test "can parse" =
  test_parser program;
  [%expect
    {|
    (Let
      (declarations (
        (Type ((name any) (desc (Record ((any int))))))
        (Variable (
          (ident buffer)
          (type_id ())
          (expression (Call (func getchar) (args ())))))
        (Function (
          (ident readint)
          (args ((any any)))
          (return_type (int))
          (body (
            Let
            (declarations (
              (Variable ((ident i) (type_id ()) (expression (Literal (Int 0)))))
              (Function (
                (ident isdigit)
                (args ((s string)))
                (return_type (int))
                (body (
                  Binary And
                  (Binary Ge
                    (Call (func ord) (args ((Lvalue (Ident s)))))
                    (Call (func ord) (args ((Literal (String 0))))))
                  (Binary Le
                    (Call (func ord) (args ((Lvalue (Ident s)))))
                    (Call (func ord) (args ((Literal (String 9))))))))))))
            (exps (
              (While
                (cond (
                  Binary Or
                  (Binary Equal
                    (Lvalue  (Ident  buffer))
                    (Literal (String " ")))
                  (Binary Equal
                    (Lvalue  (Ident  buffer))
                    (Literal (String "\\n")))))
                (body (Assign (Ident buffer) (Call (func getchar) (args ())))))
              (Assign
                (Dot (Ident any) any)
                (Call (func isdigit) (args ((Lvalue (Ident buffer))))))
              (While
                (cond (Call (func isdigit) (args ((Lvalue (Ident buffer))))))
                (body (
                  Sequence (
                    (Assign
                      (Ident i)
                      (Binary Minus
                        (Binary Plus
                          (Binary Times
                            (Lvalue  (Ident i))
                            (Literal (Int   10)))
                          (Call (func ord) (args ((Lvalue (Ident buffer))))))
                        (Call (func ord) (args ((Literal (String 0)))))))
                    (Assign (Ident buffer) (Call (func getchar) (args ())))))))
              (Lvalue (Ident i))))))))
        (Type (
          (name list)
          (desc (
            Record (
              (first int)
              (rest  list))))))
        (Function (
          (ident readlist)
          (args ())
          (return_type (list))
          (body (
            Let
            (declarations (
              (Variable (
                (ident any)
                (type_id ())
                (expression (Record any ((any (Literal (Int 0))))))))
              (Variable (
                (ident i)
                (type_id ())
                (expression (Call (func reading) (args ((Lvalue (Ident any))))))))))
            (exps ((
              If
              (cond (Lvalue (Dot (Ident any) any)))
              (then_ (
                Record list (
                  (first (Lvalue (Ident i)))
                  (rest (Call (func readlist) (args ()))))))
              (else_ ((
                Sequence (
                  (Assign (Ident buffer) (Call (func getchar) (args ()))) Nil)))))))))))
        (Function (
          (ident merge)
          (args (
            (a list)
            (b list)))
          (return_type (list))
          (body (
            If
            (cond (Binary Equal (Lvalue (Ident a)) Nil))
            (then_ (Lvalue (Ident b)))
            (else_ ((
              If
              (cond (Binary Equal (Lvalue (Ident b)) Nil))
              (then_ (Lvalue (Ident a)))
              (else_ ((
                If
                (cond (
                  Binary Lt
                  (Lvalue (Dot (Ident a) first))
                  (Lvalue (Dot (Ident b) first))))
                (then_ (
                  Record list (
                    (first (Lvalue (Dot (Ident a) first)))
                    (rest (
                      Call
                      (func merge)
                      (args ((Lvalue (Dot (Ident a) rest)) (Lvalue (Ident b)))))))))
                (else_ ((
                  Record list (
                    (first (Lvalue (Dot (Ident b) first)))
                    (rest (
                      Call
                      (func merge)
                      (args ((Lvalue (Ident a)) (Lvalue (Dot (Ident b) rest))))))))))))))))))))
        (Function (
          (ident printint)
          (args ((i int)))
          (return_type ())
          (body (
            Let
            (declarations ((
              Function (
                (ident f)
                (args ((i int)))
                (return_type ())
                (body (
                  If
                  (cond (
                    Binary Gt
                    (Lvalue  (Ident i))
                    (Literal (Int   0))))
                  (then_ (
                    Sequence (
                      (Call
                        (func f)
                        (args ((
                          Binary Divide
                          (Lvalue  (Ident i))
                          (Literal (Int   10))))))
                      (Call
                        (func putchar)
                        (args ((
                          Binary Plus
                          (Binary Minus
                            (Lvalue (Ident i))
                            (Binary Times
                              (Binary Divide
                                (Lvalue  (Ident i))
                                (Literal (Int   10)))
                              (Literal (Int 10))))
                          (Call (func ord) (args ((Literal (String 0))))))))))))
                  (else_ ())))))))
            (exps ((
              If
              (cond (
                Binary Lt
                (Lvalue  (Ident i))
                (Literal (Int   0))))
              (then_ (
                Sequence (
                  (Call (func print) (args ((Literal (String -)))))
                  (Call (func f) (args ((Negative (Lvalue (Ident i)))))))))
              (else_ ((
                If
                (cond (
                  Binary Gt
                  (Lvalue  (Ident i))
                  (Literal (Int   0))))
                (then_ (Call (func f) (args ((Lvalue (Ident i))))))
                (else_ ((Call (func print) (args ((Literal (String 0)))))))))))))))))
        (Function (
          (ident printlist)
          (args ((l list)))
          (return_type ())
          (body (
            If
            (cond (Binary Equal (Lvalue (Ident l)) Nil))
            (then_ (Sequence ()))
            (else_ ((
              Sequence (
                (Call (func printint) (args ((Lvalue (Dot (Ident l) first)))))
                (Call (func print) (args ((Literal (String " ")))))
                (Call (func printlist) (args ((Lvalue (Dot (Ident l) rest)))))))))))))))
      (exps ((
        Call
        (func printlist)
        (args ((
          Call
          (func merge)
          (args (
            (Call (func readlist) (args ()))
            (Call (func readlist) (args ()))))))))))) |}]
;;
