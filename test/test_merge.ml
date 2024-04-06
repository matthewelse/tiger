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
    (ELet
      (declarations (
        (DType (any (TRecord ((any int)))))
        (DVariable (
          (ident buffer)
          (type_id ())
          (expression (ECall (func getchar) (args ())))))
        (DFunction (
          (ident readint)
          (fields ((any any)))
          (return_type (int))
          (body (
            ELet
            (declarations (
              (DVariable ((ident i) (type_id ()) (expression (ELiteral (LInt 0)))))
              (DFunction (
                (ident isdigit)
                (fields ((s string)))
                (return_type (int))
                (body (
                  EBinary And
                  (EBinary Ge
                    (ECall (func ord) (args ((ELvalue (LIdent s)))))
                    (ECall (func ord) (args ((ELiteral (LString 0))))))
                  (EBinary Le
                    (ECall (func ord) (args ((ELvalue (LIdent s)))))
                    (ECall (func ord) (args ((ELiteral (LString 9))))))))))))
            (exps (
              (EWhile
                (cond (
                  EBinary Or
                  (EBinary Equal
                    (ELvalue  (LIdent  buffer))
                    (ELiteral (LString " ")))
                  (EBinary Equal
                    (ELvalue  (LIdent  buffer))
                    (ELiteral (LString "\\n")))))
                (body (EAssign (LIdent buffer) (ECall (func getchar) (args ())))))
              (EAssign
                (LDot (LIdent any) any)
                (ECall (func isdigit) (args ((ELvalue (LIdent buffer))))))
              (EWhile
                (cond (ECall (func isdigit) (args ((ELvalue (LIdent buffer))))))
                (body (
                  ESequence (
                    (EAssign
                      (LIdent i)
                      (EBinary Minus
                        (EBinary Plus
                          (EBinary Times
                            (ELvalue  (LIdent i))
                            (ELiteral (LInt   10)))
                          (ECall (func ord) (args ((ELvalue (LIdent buffer))))))
                        (ECall (func ord) (args ((ELiteral (LString 0)))))))
                    (EAssign (LIdent buffer) (ECall (func getchar) (args ())))))))
              (ELvalue (LIdent i))))))))
        (DType (
          list (
            TRecord (
              (first int)
              (rest  list)))))
        (DFunction (
          (ident readlist)
          (fields ())
          (return_type (list))
          (body (
            ELet
            (declarations (
              (DVariable (
                (ident any)
                (type_id ())
                (expression (ERecord any ((any (ELiteral (LInt 0))))))))
              (DVariable (
                (ident i)
                (type_id ())
                (expression (ECall (func reading) (args ((ELvalue (LIdent any))))))))))
            (exps ((
              EIf
              (cond (ELvalue (LDot (LIdent any) any)))
              (then_ (
                ERecord list (
                  (first (ELvalue (LIdent i)))
                  (rest (ECall (func readlist) (args ()))))))
              (else_ ((
                ESequence (
                  (EAssign (LIdent buffer) (ECall (func getchar) (args ()))) ENil)))))))))))
        (DFunction (
          (ident merge)
          (fields (
            (a list)
            (b list)))
          (return_type (list))
          (body (
            EIf
            (cond (EBinary Equal (ELvalue (LIdent a)) ENil))
            (then_ (ELvalue (LIdent b)))
            (else_ ((
              EIf
              (cond (EBinary Equal (ELvalue (LIdent b)) ENil))
              (then_ (ELvalue (LIdent a)))
              (else_ ((
                EIf
                (cond (
                  EBinary Lt
                  (ELvalue (LDot (LIdent a) first))
                  (ELvalue (LDot (LIdent b) first))))
                (then_ (
                  ERecord list (
                    (first (ELvalue (LDot (LIdent a) first)))
                    (rest (
                      ECall
                      (func merge)
                      (args (
                        (ELvalue (LDot (LIdent a) rest)) (ELvalue (LIdent b)))))))))
                (else_ ((
                  ERecord list (
                    (first (ELvalue (LDot (LIdent b) first)))
                    (rest (
                      ECall
                      (func merge)
                      (args (
                        (ELvalue (LIdent a)) (ELvalue (LDot (LIdent b) rest))))))))))))))))))))
        (DFunction (
          (ident printint)
          (fields ((i int)))
          (return_type ())
          (body (
            ELet
            (declarations ((
              DFunction (
                (ident f)
                (fields ((i int)))
                (return_type ())
                (body (
                  EIf
                  (cond (
                    EBinary Gt
                    (ELvalue  (LIdent i))
                    (ELiteral (LInt   0))))
                  (then_ (
                    ESequence (
                      (ECall
                        (func f)
                        (args ((
                          EBinary Divide
                          (ELvalue  (LIdent i))
                          (ELiteral (LInt   10))))))
                      (ECall
                        (func putchar)
                        (args ((
                          EBinary Plus
                          (EBinary Minus
                            (ELvalue (LIdent i))
                            (EBinary Times
                              (EBinary Divide
                                (ELvalue  (LIdent i))
                                (ELiteral (LInt   10)))
                              (ELiteral (LInt 10))))
                          (ECall (func ord) (args ((ELiteral (LString 0))))))))))))
                  (else_ ())))))))
            (exps ((
              EIf
              (cond (
                EBinary Lt
                (ELvalue  (LIdent i))
                (ELiteral (LInt   0))))
              (then_ (
                ESequence (
                  (ECall (func print) (args ((ELiteral (LString -)))))
                  (ECall (func f) (args ((ENegative (ELvalue (LIdent i)))))))))
              (else_ ((
                EIf
                (cond (
                  EBinary Gt
                  (ELvalue  (LIdent i))
                  (ELiteral (LInt   0))))
                (then_ (ECall (func f) (args ((ELvalue (LIdent i))))))
                (else_ ((ECall (func print) (args ((ELiteral (LString 0)))))))))))))))))
        (DFunction (
          (ident printlist)
          (fields ((l list)))
          (return_type ())
          (body (
            EIf
            (cond (EBinary Equal (ELvalue (LIdent l)) ENil))
            (then_ (ESequence ()))
            (else_ ((
              ESequence (
                (ECall (func printint) (args ((ELvalue (LDot (LIdent l) first)))))
                (ECall (func print) (args ((ELiteral (LString " ")))))
                (ECall (func printlist) (args ((ELvalue (LDot (LIdent l) rest)))))))))))))))
      (exps ((
        ECall
        (func printlist)
        (args ((
          ECall
          (func merge)
          (args (
            (ECall (func readlist) (args ()))
            (ECall (func readlist) (args ()))))))))))) |}]
;;
