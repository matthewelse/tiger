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

type decs = dec list

and dec =
  | DType of type_dec
  | DVar of var_dec
  | DFunc of func_dec

and type_dec = Type_id.t * type_

and var_dec =
  { ident : Ident.t
  ; type_id : Type_id.t option
  ; expression : expression
  }

and func_dec =
  { ident : Ident.t
  ; fields : (Ident.t * Type_id.t) list
  ; return_type : Type_id.t option
  ; body : expression
  }

and type_ =
  | TIdent of Type_id.t
  | TRecord of (Field_id.t * Type_id.t) list
  | TArray of Type_id.t

and expression =
  | ENil
  | ELvalue of lvalue
  | ESequence of expression list
  | EConst of const
  | ENegative of expression
  | EBinary of binop * expression * expression
  | ERecord of (Field_id.t * expression) list
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
      ; hi : expression
      ; body : expression
      }
  | EBreak
  | ELet of
      { decs : decs
      ; exps : expression list
      }

and lvalue =
  | LIdent of Ident.t
  | LDot of lvalue * Ident.t
  | LSubscript of lvalue * expression

and const =
  | CInt of string
  | CString of string

and binop =
  | Plus
  | Minus
  | Times
  | Divide
  | Equal
  | NotEqual
  | Gt
  | Lt
  | Ge
  | Le
  | And
  | Or
[@@deriving sexp_of]
