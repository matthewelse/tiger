open! Core

module Ident =
  String_id.Make
    (struct
      let module_name = "Ident"
    end)
    ()

module Field_id =
  String_id.Make
    (struct
      let module_name = "Field_id"
    end)
    ()

module Type_id =
  String_id.Make
    (struct
      let module_name = "Type_id"
    end)
    ()

type declarations = declaration list

and declaration =
  | DType of type_declaration
  | DVariable of variable_declaration
  | DFunction of function_declaration

and type_declaration = Type_id.t * type_desc

and variable_declaration =
  { ident : Ident.t
  ; type_id : Type_id.t option
  ; expression : expression
  }

and function_declaration =
  { ident : Ident.t
  ; args : (Ident.t * Type_id.t) list
  ; return_type : Type_id.t option
  ; body : expression
  }

and type_desc =
  | TIdent of Type_id.t
  | TRecord of (Field_id.t * Type_id.t) list
  | TArray of Type_id.t

and expression =
  | ENil
  | ELvalue of lvalue
  | ESequence of expression list
  | ELiteral of literal
  | ENegative of expression
  | EBinary of binary_operator * expression * expression
  | ERecord of Type_id.t * (Field_id.t * expression) list
  | EArray of
      { element_type : Type_id.t
      ; size : expression
      ; init : expression
      }
  | EAssign of lvalue * expression
  | EIf of
      { cond : expression
      ; then_ : expression
      ; else_ : expression option
      }
  | EWhile of
      { cond : expression
      ; body : expression
      }
  | EFor of
      { ident : Ident.t
      ; lo : expression
      ; hi : expression (** inclusive *)
      ; body : expression
      }
  | EBreak
  | ELet of
      { declarations : declarations
      ; exps : expression list
      }
  | ECall of
      { func : Ident.t
      ; args : expression list
      }

and lvalue =
  | LIdent of Ident.t
  | LDot of lvalue * Field_id.t
  | LSubscript of lvalue * expression

and literal =
  | LInt of string
  | LString of string

and binary_operator =
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
