open! Core

module Binary_operator = struct
  type t =
    | And
    | Divide
    | Equal
    | Ge
    | Gt
    | Le
    | Lt
    | Minus
    | NotEqual
    | Or
    | Plus
    | Times
  [@@deriving sexp_of]
end

module Literal = struct
  type t =
    | Int of string
    | String of string
  [@@deriving sexp_of]
end

module Type_desc = struct
  type t =
    | Alias of Type_id.t
    | Record of (Field_id.t * Type_id.t) list
    | Array of Type_id.t
  [@@deriving sexp_of]
end

module Type_declaration = struct
  type t =
    { name : Type_id.t
    ; desc : Type_desc.t
    }
  [@@deriving sexp_of]
end

module Variable_declaration = struct
  type 'expr t =
    { ident : Ident.t
    ; type_id : Type_id.t option
    ; expression : 'expr
    }
  [@@deriving sexp_of]
end

module Function_declaration = struct
  type 'expr t =
    { ident : Ident.t
    ; formal_args : (Ident.t * Type_id.t) list
    ; return_type : Type_id.t option
    ; body : 'expr
    }
  [@@deriving sexp_of]
end

module Declaration = struct
  type 'expr t =
    | Type of Type_declaration.t
    | Variable of 'expr Variable_declaration.t
    | Function of 'expr Function_declaration.t
  [@@deriving sexp_of, typed_variants]

  let diff_type t1 t2 =
    let t1 = Typed_variant.which t1 in
    let t2 = Typed_variant.which t2 in
    not (Typed_variant.Packed.equal t1 t2)
  ;;
end

module Lvalue = struct
  type 'expr t =
    | Ident of Ident.t
    | Dot of 'expr t * Field_id.t
    | Subscript of 'expr t * 'expr
  [@@deriving sexp_of]
end

module Expression = struct
  type t =
    | Nil
    | Lvalue of t Lvalue.t
    | Sequence of t list
    | Literal of Literal.t
    | Negative of t
    | Binary of Binary_operator.t * t * t
    | Record of Type_id.t * (Field_id.t * t) list
    | Array of
        { element_type : Type_id.t
        ; size : t
        ; init : t
        }
    | Assign of t Lvalue.t * t
    | If of
        { cond : t
        ; then_ : t
        ; else_ : t option
        }
    | While of
        { cond : t
        ; body : t
        }
    | For of
        { ident : Ident.t
        ; lo : t
        ; hi : t (** inclusive *)
        ; body : t
        }
    | Break
    | Let of
        { declarations : t Declaration.t list
        ; exps : t list
        }
    | Call of
        { func : Ident.t
        ; args : t list
        }
  [@@deriving sexp_of]
end

class ['a] folder =
  object (self)
    method declaration acc : _ Declaration.t -> 'a =
      function
      | Type type_declaration -> self#type_declaration acc type_declaration
      | Variable variable_declaration ->
        self#variable_declaration acc variable_declaration
      | Function function_declaration ->
        self#function_declaration acc function_declaration

    method type_declaration acc : Type_declaration.t -> 'a =
      fun { name; desc } ->
        let acc = self#type_id acc name in
        self#type_desc acc desc

    method type_desc acc : Type_desc.t -> 'a =
      function
      | Alias name -> self#type_id acc name
      | Record fields ->
        List.fold fields ~init:acc ~f:(fun acc (field, typ) ->
          self#field_id (self#type_id acc typ) field)
      | Array typ -> self#type_id acc typ

    method variable_declaration acc : Expression.t Variable_declaration.t -> 'a =
      fun { ident; type_id; expression } ->
        let acc = self#ident acc ident in
        let acc = Option.fold ~init:acc ~f:self#type_id type_id in
        self#expression acc expression

    method function_declaration acc : Expression.t Function_declaration.t -> 'a =
      fun { ident; formal_args; return_type; body } ->
        let acc = self#ident acc ident in
        let acc =
          List.fold formal_args ~init:acc ~f:(fun acc (ident, typ) ->
            self#ident (self#type_id acc typ) ident)
        in
        let acc = Option.fold ~init:acc ~f:self#type_id return_type in
        self#expression acc body

    method expression acc : Expression.t -> 'a =
      function
      | Nil -> acc
      | Lvalue lvalue -> self#lvalue acc lvalue
      | Sequence exps -> List.fold exps ~init:acc ~f:self#expression
      | Literal (Int n) -> self#int n
      | Literal (String s) -> self#string s
      | Negative exp -> self#expression acc exp
      | Binary (binary_op, lhs, rhs) ->
        let acc = self#binary_operator acc binary_op in
        self#expression (self#expression acc lhs) rhs
      | Record (type_id, fields) ->
        let acc = self#type_id acc type_id in
        List.fold fields ~init:acc ~f:(fun acc (field_id, exp) ->
          let acc = self#field_id acc field_id in
          self#expression acc exp)
      | Array { size; init; element_type } ->
        let acc = self#type_id acc element_type in
        self#expression (self#expression acc size) init
      | Assign (lvalue, exp) -> self#expression (self#lvalue acc lvalue) exp
      | If { cond; then_; else_ } ->
        let acc = self#expression acc cond in
        let acc = self#expression acc then_ in
        Option.fold ~init:acc ~f:self#expression else_
      | While { cond; body } -> self#expression (self#expression acc cond) body
      | For { lo; hi; body; ident } ->
        let acc = self#ident acc ident in
        let acc = self#expression acc lo in
        let acc = self#expression acc hi in
        self#expression acc body
      | Break -> self#break acc
      | Let { declarations; exps } ->
        let acc = List.fold declarations ~init:acc ~f:self#declaration in
        List.fold exps ~init:acc ~f:self#expression
      | Call { func; args } ->
        let acc = self#ident acc func in
        List.fold args ~init:acc ~f:self#expression

    method lvalue acc : Expression.t Lvalue.t -> 'a =
      function
      | Ident ident -> self#ident acc ident
      | Dot (lvalue, field) -> self#lvalue (self#field_id acc field) lvalue
      | Subscript (lvalue, exp) -> self#lvalue (self#expression acc exp) lvalue

    method binary_operator acc _ = acc
    method int acc _ = acc
    method string acc _ = acc
    method type_id acc _ = acc
    method field_id acc _ = acc
    method ident acc _ = acc
    method break acc = acc
  end
