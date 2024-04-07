open! Core

type native_function = Print [@@deriving sexp_of]

module Value = struct
  type t =
    | Nil
    | Unit
    | Int of int
    | String of string
    | Record of t ref Ast.Field_id.Map.t
    | Function of Ast.Ident.t list * Ast.Expression.t
    | Array of t array
    | Native of native_function
  [@@deriving sexp_of]

  let int_exn t =
    match t with
    | Int n -> n
    | _ -> raise_s [%message "Not an int" (t : t)]
  ;;

  let record_exn t =
    match t with
    | Record r -> r
    | _ -> raise_s [%message "Not a record" (t : t)]
  ;;

  let array_exn t =
    match t with
    | Array a -> a
    | _ -> raise_s [%message "Not an array" (t : t)]
  ;;

  let function_exn t =
    match t with
    | Function (args, body) -> args, body
    | _ -> raise_s [%message "Not an function" (t : t)]
  ;;
end

module Env = struct
  type t = Value.t ref Ast.Ident.Map.t
end

let escapeworthy_map = [ '\n', 'n'; '\t', 't'; '"', '"' ]

let unescape =
  String.Escaping.unescape_gen_exn ~escapeworthy_map ~escape_char:'\\' |> Staged.unstage
;;

exception Break

let rec interpret (env : Env.t) (expr : Ast.Expression.t) : Value.t =
  match expr with
  | Let { declarations; exps } ->
    let env =
      List.fold
        ~init:env
        ~f:(fun env (declaration : _ Ast.Declaration.t) ->
          match declaration with
          | Type _ -> env
          | Variable { ident; type_id = _; expression } ->
            let value = interpret env expression in
            Map.set env ~key:ident ~data:(ref value)
          | Function { ident; args; return_type = _; body } ->
            Map.set
              env
              ~key:ident
              ~data:(ref (Value.Function (List.map ~f:fst args, body))))
        declarations
    in
    List.fold exps ~init:Value.Unit ~f:(fun _ exp -> interpret env exp)
  | Nil -> Nil
  | Break -> raise_notrace Break
  | Lvalue l ->
    let lvalue, _ = lvalue env l in
    lvalue ()
  | Sequence exps -> List.fold exps ~init:Value.Unit ~f:(fun _ exp -> interpret env exp)
  | Literal literal ->
    (match literal with
     | Int n ->
       let n = Int.of_string n in
       Int n
     | String s ->
       let s = unescape s in
       String s)
  | Negative expr -> Int ~-(Value.int_exn (interpret env expr))
  | Binary (op, l, r) ->
    let l, r = Value.int_exn (interpret env l), Value.int_exn (interpret env r) in
    Int
      (match op with
       | And -> Bool.to_int (l <> 0 && r <> 0)
       | Divide -> l / r
       | Equal -> Bool.to_int (l = r)
       | Ge -> Bool.to_int (l >= r)
       | Gt -> Bool.to_int (l > r)
       | Le -> Bool.to_int (l <= r)
       | Lt -> Bool.to_int (l < r)
       | Minus -> l - r
       | NotEqual -> Bool.to_int (l <> r)
       | Or -> Bool.to_int (l <> 0 || r <> 0)
       | Plus -> l + r
       | Times -> l * r)
  | Record (_, fields) ->
    let map =
      List.map fields ~f:(fun (name, expr) -> name, ref (interpret env expr))
      |> Ast.Field_id.Map.of_alist_exn
    in
    Record map
  | Array { element_type = _; size; init } ->
    let array =
      Array.init (Value.int_exn (interpret env size)) ~f:(fun _ -> interpret env init)
    in
    Array array
  | Assign (l, set_to) ->
    let _, set_lvalue = lvalue env l in
    let set_to = interpret env set_to in
    set_lvalue set_to;
    Unit
  | If { cond; then_; else_ } ->
    let cond = interpret env cond |> Value.int_exn in
    if cond <> 0
    then interpret env then_
    else (
      match else_ with
      | None -> Unit
      | Some else_ -> interpret env else_)
  | While { cond; body } ->
    (try
       while
         let cond = interpret env cond in
         Value.int_exn cond <> 0
       do
         let (_ : Value.t) = interpret env body in
         ()
       done
     with
     | Break -> ());
    Unit
  | For { ident; lo; hi; body } ->
    let lo = interpret env lo |> Value.int_exn in
    let hi = interpret env hi |> Value.int_exn in
    (try
       for i = lo to hi do
         let env' = Map.set env ~key:ident ~data:(ref (Value.Int i)) in
         let (_ : Value.t) = interpret env' body in
         ()
       done
     with
     | Break -> ());
    Unit
  | Call { func; args } ->
    (match !(Map.find_exn env func) with
     | Native Print ->
       (match List.map ~f:(interpret env) args with
        | [ String arg ] ->
          print_string arg;
          Unit
        | args -> raise_s [%message "Invalid arguments to [print]" (args : Value.t list)])
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

and lvalue env (l : _ Ast.Lvalue.t) : (unit -> Value.t) * (Value.t -> unit) =
  match l with
  | Ident ident ->
    let ref = Map.find_exn env ident in
    (fun () -> !ref), fun value -> ref := value
  | Dot (variable, field) ->
    let value, _ = lvalue env variable in
    let record = Value.record_exn (value ()) in
    let ref = Map.find_exn record field in
    (fun () -> !ref), fun value -> ref := value
  | Subscript (variable, index) ->
    let array, _ = lvalue env variable in
    let elem = Value.array_exn (array ()) in
    ( (fun () -> elem.(Value.int_exn (interpret env index)))
    , fun value -> elem.(Value.int_exn (interpret env index)) <- value )
;;

let run expr =
  let ident = Ast.Ident.of_string in
  let defaults = [ ident "print", ref (Value.Native Print) ] in
  interpret (Ast.Ident.Map.of_alist_exn defaults) expr
;;
