open! Core

type native_function = Print [@@deriving sexp_of]

type value =
  | Nil
  | Unit
  | Int of int
  | String of string
  | Record of value ref Ast.Field_id.Map.t
  | Function of Ast.Ident.t list * Ast.expression
  | Array of value array
  | Native of native_function
[@@deriving sexp_of]

let int_exn value =
  match value with
  | Int n -> n
  | _ -> raise_s [%message "Not an int" (value : value)]
;;

let record_exn value =
  match value with
  | Record r -> r
  | _ -> raise_s [%message "Not a record" (value : value)]
;;

let array_exn value =
  match value with
  | Array a -> a
  | _ -> raise_s [%message "Not an array" (value : value)]
;;

let function_exn value =
  match value with
  | Function (args, body) -> args, body
  | _ -> raise_s [%message "Not an function" (value : value)]
;;

type env = value ref Ast.Ident.Map.t

let escapeworthy_map = [ '\n', 'n'; '\t', 't'; '"', '"' ]

let unescape =
  String.Escaping.unescape_gen_exn ~escapeworthy_map ~escape_char:'\\' |> Staged.unstage
;;

let rec interpret (env : env) (expr : Ast.expression) : value =
  match expr with
  | ELet { declarations; exps } ->
    let env =
      List.fold
        ~init:env
        ~f:(fun env (declaration : Ast.declaration) ->
          match declaration with
          | DType _ -> env
          | DVariable { ident; type_id = _; expression } ->
            let value = interpret env expression in
            Map.set env ~key:ident ~data:(ref value)
          | DFunction { ident; args; return_type = _; body } ->
            Map.set env ~key:ident ~data:(ref (Function (List.map ~f:fst args, body))))
        declarations
    in
    List.fold exps ~init:Unit ~f:(fun _ exp -> interpret env exp)
  | ENil -> Nil
  | EBreak ->
    (* CR melse: [raise Break] *)
    failwith "Not supported: EBreak."
  | ELvalue l ->
    let lvalue, _ = lvalue env l in
    lvalue ()
  | ESequence exps -> List.fold exps ~init:Unit ~f:(fun _ exp -> interpret env exp)
  | ELiteral literal ->
    (match literal with
     | LInt n ->
       let n = Int.of_string n in
       Int n
     | LString s ->
       let s = unescape s in
       String s)
  | ENegative expr -> Int ~-(int_exn (interpret env expr))
  | EBinary (op, l, r) ->
    let l, r = int_exn (interpret env l), int_exn (interpret env r) in
    Int
      (match op with
       | Ast.And -> Bool.to_int (l <> 0 && r <> 0)
       | Ast.Divide -> l / r
       | Ast.Equal -> Bool.to_int (l = r)
       | Ast.Ge -> Bool.to_int (l >= r)
       | Ast.Gt -> Bool.to_int (l > r)
       | Ast.Le -> Bool.to_int (l <= r)
       | Ast.Lt -> Bool.to_int (l < r)
       | Ast.Minus -> l - r
       | Ast.NotEqual -> Bool.to_int (l <> r)
       | Ast.Or -> Bool.to_int (l <> 0 || r <> 0)
       | Ast.Plus -> l + r
       | Ast.Times -> l * r)
  | ERecord (_, fields) ->
    let map =
      List.map fields ~f:(fun (name, expr) -> name, ref (interpret env expr))
      |> Ast.Field_id.Map.of_alist_exn
    in
    Record map
  | EArray { element_type = _; size; init } ->
    let array =
      Array.init (int_exn (interpret env size)) ~f:(fun _ -> interpret env init)
    in
    Array array
  | EAssign (l, set_to) ->
    let _, set_lvalue = lvalue env l in
    let set_to = interpret env set_to in
    set_lvalue set_to;
    Unit
  | EIf { cond; then_; else_ } ->
    let cond = interpret env cond |> int_exn in
    if cond <> 0
    then interpret env then_
    else (
      match else_ with
      | None -> Unit
      | Some else_ -> interpret env else_)
  | EWhile _ -> failwith "EWhile not supported"
  | EFor { ident; lo; hi; body } ->
    let lo = interpret env lo |> int_exn in
    let hi = interpret env hi |> int_exn in
    for i = lo to hi do
      let env' = Map.set env ~key:ident ~data:(ref (Int i)) in
      let (_ : value) = interpret env' body in
      ()
    done;
    Unit
  | ECall { func; args } ->
    (match !(Map.find_exn env func) with
     | Native Print ->
       (match List.map ~f:(interpret env) args with
        | [ String arg ] ->
          print_string arg;
          Unit
        | args -> raise_s [%message "Invalid arguments to [print]" (args : value list)])
     | Function (formal_args, body) ->
       let env' =
         List.fold2_exn formal_args args ~init:env ~f:(fun env' formal_arg arg ->
           let arg = interpret env arg in
           Map.set env' ~key:formal_arg ~data:(ref arg))
       in
       interpret env' body
     | _ ->
       raise_s
         [%message "Trying to call a function which is neither [native] nor [function]"])

and lvalue env (l : Ast.lvalue) : (unit -> value) * (value -> unit) =
  match l with
  | LIdent ident ->
    let ref = Map.find_exn env ident in
    (fun () -> !ref), fun value -> ref := value
  | LDot (variable, field) ->
    let value, _ = lvalue env variable in
    let record = record_exn (value ()) in
    let ref = Map.find_exn record field in
    (fun () -> !ref), fun value -> ref := value
  | LSubscript (variable, index) ->
    let array, _ = lvalue env variable in
    let elem = array_exn (array ()) in
    ( (fun () -> elem.(int_exn (interpret env index)))
    , fun value -> elem.(int_exn (interpret env index)) <- value )
;;

let run expr =
  let ident = Ast.Ident.of_string in
  let defaults = [ ident "print", ref (Native Print) ] in
  interpret (Ast.Ident.Map.of_alist_exn defaults) expr
;;
