open! Core
open Ast

module Variable_environment = struct
  module Entry = struct
    type t =
      | Variable of Type.t
      | Function of Type.Function.t
  end

  type t = (Ident.t, Entry.t) Scoped_table.t

  let variable_exn (t : t) name =
    match Scoped_table.find t name with
    | None -> failwith [%string "Variable not found %{name#Ident}"]
    | Some (Variable type_) -> type_
    | Some (Function _) ->
      failwith [%string "Expected variable, but found function %{name#Ident}"]
  ;;

  let function_exn (t : t) name =
    match Scoped_table.find t name with
    | None -> failwith [%string "Function not found %{name#Ident}"]
    | Some (Function func) -> func
    | Some (Variable _) ->
      failwith [%string "Expected function, but found variable %{name#Ident}"]
  ;;
end

module Type_environment = struct
  type t = (Type_id.t, Type.t) Scoped_table.t
end

let rec declarations
  (venv : Variable_environment.t)
  (tenv : Type_environment.t)
  (decs : Expression.t Declaration.t list)
  =
  let module Group = struct
    type t =
      | Type of Type_declaration.t list
      | Variable of Expression.t Variable_declaration.t list
      | Function of Expression.t Function_declaration.t list
  end
  in
  let groups = List.group decs ~break:Ast.Declaration.diff_type in
  let mutually_recursive_groups =
    List.map groups ~f:(fun group : Group.t ->
      let group_kind = List.hd_exn group |> Declaration.Typed_variant.which in
      let { f = T group_kind } = group_kind in
      let group =
        List.map group ~f:(fun decl ->
          Declaration.Typed_variant.get group_kind decl
          |> Option.value_exn
               ~message:"Unexpected declaration: should be grouped by declaration kind.")
      in
      match group_kind with
      | Type -> Type group
      | Variable -> Variable group
      | Function -> Function group)
  in
  List.iter mutually_recursive_groups ~f:(fun group ->
    match group with
    | Type types ->
      let types =
        List.map types ~f:(fun { name; desc } ->
          let type_ = type_desc desc in
          Scoped_table.set tenv ~key:name ~data:type_;
          type_)
      in
      List.iter types ~f:(fun type_ -> Type.resolve type_ tenv)
    | Variable variables ->
      List.iter variables ~f:(fun { ident; type_id; expression = expr } ->
        let expression_type = expression venv tenv expr in
        let expected_type =
          match type_id with
          | None -> None
          | Some type_id ->
            Some
              (Option.value_exn ~message:[%string "Unknown type %{type_id#Type_id}"]
               @@ Scoped_table.find tenv type_id)
        in
        (match expression_type, expected_type with
         | Nil, Some must_be_record ->
           let _ = Type.require_exn must_be_record Record in
           ()
         | Nil, _ -> failwith "Nil values must be constrained by a type annotation."
         | _ -> ());
        (match expected_type with
         | None -> ()
         | Some expected_type -> Type.require_match_exn expected_type expression_type);
        Scoped_table.set venv ~key:ident ~data:(Variable expression_type))
    | Function functions ->
      List.iter functions ~f:(fun { ident; formal_args; return_type; body } ->
        let formal_args =
          List.map formal_args ~f:(fun (name, type_id) ->
            let type_ =
              Option.value_exn ~message:[%string "Unknown type name %{type_id#Type_id}"]
              @@ Scoped_table.find tenv type_id
            in
            Scoped_table.set venv ~key:name ~data:(Variable type_);
            type_)
        in
        let return_type =
          match return_type with
          | None -> Type.Unit
          | Some return_type ->
            Option.value_exn ~message:[%string "Unknown type name %{return_type#Type_id}"]
            @@ Scoped_table.find tenv return_type
        in
        Scoped_table.set
          venv
          ~key:ident
          ~data:
            (Variable_environment.Entry.Function { formal_args; result = return_type });
        let venv = Scoped_table.enter_scope venv in
        let body_type = expression venv tenv body in
        Type.require_match_exn return_type body_type))

and type_desc (desc : Type_desc.t) : Type.t =
  match desc with
  | Alias other_type_id -> Type.named other_type_id
  | Record fields ->
    let fields = List.Assoc.map fields ~f:Type.named in
    Record { fields; id = Type.Record_id.create () }
  | Array element_type ->
    Array { element_type = Type.named element_type; id = Type.Array_id.create () }

and expression
  (venv : Variable_environment.t)
  (tenv : Type_environment.t)
  (expr : Expression.t)
  : Type.t
  =
  match expr with
  | Nil -> Nil
  | Break -> Unit
  | Sequence exprs ->
    List.fold exprs ~init:Type.Unit ~f:(fun _prev expr : Type.t ->
      expression venv tenv expr)
  | Lvalue lv -> lvalue venv tenv lv
  | Literal (Int _) -> Int
  | Literal (String _) -> String
  | Negative expr ->
    let () = Type.require_exn (expression venv tenv expr) Int in
    Int
  | Binary ((Equal | NotEqual), l, r) ->
    let l = expression venv tenv l in
    let r = expression venv tenv r in
    Type.require_match_exn l r;
    Int
  | Binary (_op, l, r) ->
    let () = Type.require_exn (expression venv tenv l) Int in
    let () = Type.require_exn (expression venv tenv r) Int in
    Int
  | Record (type_id, field_exprs) ->
    let type_ =
      Option.value_exn ~message:[%string "Unknown type name %{type_id#Type_id}"]
      @@ Scoped_table.find tenv type_id
    in
    let%tydi { fields = field_types; id = _ } = Type.require_exn type_ Record in
    let field_types = List.sort field_types ~compare:[%compare: Field_id.t * _] in
    let field_exprs = List.sort field_exprs ~compare:[%compare: Field_id.t * _] in
    (match
       List.iter2
         field_types
         field_exprs
         ~f:(fun (field_type_name, field_type) (field_expr_name, field_expr) ->
           if Field_id.equal field_expr_name field_type_name
           then (
             let expr_type = expression venv tenv field_expr in
             Type.require_match_exn field_type expr_type)
           else
             failwith
               [%string "Unexpected field name in record: %{field_expr_name#Field_id}"])
     with
     | Unequal_lengths -> failwith "asdf"
     | Ok () -> type_)
  | Array { element_type; size = _; init } ->
    let init_type = expression venv tenv init in
    let size_type = expression venv tenv init in
    let typ =
      Option.value_exn ~message:[%string "Unknown type name %{element_type#Type_id}"]
      @@ Scoped_table.find tenv element_type
    in
    let%tydi { element_type; _ } = Type.require_exn typ Array in
    Type.require_match_exn element_type init_type;
    Type.require_exn size_type Int;
    typ
  | Assign (var, value) ->
    let var_type = lvalue venv tenv var in
    let expr_type = expression venv tenv value in
    Type.require_match_exn var_type expr_type;
    Unit
  | If { cond; then_; else_ } ->
    let cond_type = expression venv tenv cond in
    Type.require_exn cond_type Int;
    let then_type = expression venv tenv then_ in
    let else_type : Type.t =
      match else_ with
      | None -> Unit
      | Some else_ -> expression venv tenv else_
    in
    Type.require_match_exn then_type else_type;
    then_type
  | While { cond; body } ->
    let cond_type = expression venv tenv cond in
    Type.require_exn cond_type Int;
    let body_type : Type.t = expression venv tenv body in
    Type.require_exn body_type Unit;
    Unit
  | For { ident; lo; hi; body } ->
    let lo_type = expression venv tenv lo in
    Type.require_exn lo_type Int;
    let hi_type = expression venv tenv hi in
    Type.require_exn hi_type Int;
    let venv = Scoped_table.enter_scope venv in
    Scoped_table.set venv ~key:ident ~data:(Variable Type.Int);
    let body_type = expression venv tenv body in
    Type.require_exn body_type Unit;
    Unit
  | Let { declarations = decls; exps } ->
    let venv = Scoped_table.enter_scope venv in
    let tenv = Scoped_table.enter_scope tenv in
    declarations venv tenv decls;
    List.fold exps ~init:Type.Unit ~f:(fun _ expr -> expression venv tenv expr)
  | Call { func; args } ->
    let%tydi { formal_args; result } = Variable_environment.function_exn venv func in
    List.iter2_exn args formal_args ~f:(fun arg formal_arg ->
      let arg_type = expression venv tenv arg in
      Type.require_match_exn arg_type formal_arg);
    result

and lvalue (venv : Variable_environment.t) (tenv : Type_environment.t) (lv : _ Lvalue.t)
  : Type.t
  =
  match lv with
  | Ident name -> Variable_environment.variable_exn venv name
  | Dot (record, field) ->
    let record_type = lvalue venv tenv record in
    let%tydi { fields; id = _ } = Type.require_exn record_type Record in
    List.Assoc.find_exn fields field ~equal:Field_id.equal
  | Subscript (array, index) ->
    let array_type = lvalue venv tenv array in
    let%tydi { element_type; id = _ } = Type.require_exn array_type Array in
    let () = Type.require_exn (expression venv tenv index) Int in
    element_type
;;

module For_testing = struct
  let stdlib =
    let ( %: ) name typ = Ident.of_string name, typ in
    let ( @-> ) args result : Variable_environment.Entry.t =
      Function { formal_args = args; result }
    in
    [ "print" %: ([ String ] @-> Unit)
    ; "flush" %: ([] @-> Unit)
    ; "getchar" %: ([] @-> String)
    ; "ord" %: ([ String ] @-> Int)
    ; "chr" %: ([ Int ] @-> String)
    ; "size" %: ([ String ] @-> Int)
    ; "substring" %: ([ String; Int; Int ] @-> String)
    ; "concat" %: ([ String; String ] @-> String)
    ; "not" %: ([ Int ] @-> Int)
    ; "exit" %: ([ Int ] @-> Unit)
    ]
  ;;

  let type_of_expression expr =
    let value_env = Scoped_table.create (module Ident) in
    let type_env = Scoped_table.create (module Type_id) in
    Scoped_table.set ~key:(Type_id.of_string "int") ~data:Type.Int type_env;
    Scoped_table.set ~key:(Type_id.of_string "string") ~data:Type.String type_env;
    List.iter stdlib ~f:(fun (name, typ) ->
      Scoped_table.set value_env ~key:name ~data:typ);
    expression value_env type_env expr
  ;;
end
