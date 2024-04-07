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
    ; args : (Ident.t * Type_id.t) list
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
  [@@deriving sexp_of]
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
