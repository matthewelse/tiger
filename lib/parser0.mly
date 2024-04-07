%{
open Ast
open Ast.Expression
%}

%token<string> Int    "int"
%token<string> String "string"
%token<string> Ident
%token Ampersand    "&"
%token Array        "array"
%token Assign       ":="
%token Break        "break"
%token Colon        ":"
%token Comma        ","
%token Do           "do"
%token Dot          "."
%token Else         "else"
%token End          "end"
%token Equal        "="
%token For          "for"
%token Function     "function"
%token Greater      ">"
%token GreaterEqual ">="
%token If           "if"
%token In           "in"
%token Lbrace       "{"
%token Lbracket     "["
%token Less         "<"
%token LessEqual    "<="
%token Let          "let"
%token Lparen       "("
%token Minus        "-"
%token Nil          "nil"
%token NotEqual     "<>"
%token Of           "of"
%token Pipe         "|"
%token Plus         "+"
%token Rbrace       "}"
%token Rbracket     "]"
%token Rparen       ")"
%token Semicolon    ";"
%token Slash        "/"
%token Star         "*"
%token Then         "then"
%token To           "to"
%token Type         "type"
%token Var          "var"
%token While        "while"
%token Eof

%nonassoc Else
%nonassoc Of

%nonassoc Assign
%left Pipe, Ampersand
%nonassoc LessEqual, GreaterEqual, Less, Greater, NotEqual, Equal
%left Plus, Minus
%left Star, Slash

%start<Ast.Expression.t> program 

%%

let program := ~ = expression; Eof; <>

(* Declarations *)

let declarations == list(declaration)

let declaration :=
  | ~ = type_declaration;     <Declaration.Type>
  | ~ = variable_declaration; <Declaration.Variable>
  | ~ = function_declaration; <Declaration.Function>


(* Global variable declarations *)

let variable_declaration ==
  | "var" ; ~ = ident; ":="; ~ = expression; { {Variable_declaration.ident; type_id = None ; expression } }
  | "var" ; ~ = ident; ":"; ~ = type_id; ":="; ~ = expression; { {Variable_declaration.ident; type_id = Some type_id; expression } }

(* Function declarations *)

let function_declaration ==
  | "function"; ~ = ident; "("; args = func_params; ")"; "=";
    body = expression;
    { { Function_declaration.ident; args; return_type = None; body } }
  | "function"; ~ = ident; "("; args = func_params; ")"; ":"; return_type = type_id; "=";
    body = expression;
    { { Function_declaration.ident; args; return_type = Some return_type; body } }


let func_params == separated_list(",", func_param)

let func_param :=
  | ~ = ident; ":"; ~ = type_id; <>

(* Type declarations *)

let type_declaration ==
  | "type"; name = type_id; "="; desc = type_desc; { { Type_declaration.name; desc } }

let type_desc :=
  | ~ = type_id;                 <Type_desc.Alias>
  | "{"; ~ = record_fields; "}"; <Type_desc.Record>
  | "array"; "of"; ~ = type_id;  <Type_desc.Array>

let record_fields == separated_nonempty_list(",", record_field)

let record_field :=
  | ~ = field_id; ":"; ~ = type_id; <>

(* Expressions *)

let expression := 
  | ~ = one_expression; <>
  | "("; ~ = separated_list(";", one_expression); ")"; <Sequence>

let one_expression := 
  | "nil"; { Nil }
  | ~ = literal; <Literal>
  | "-"; ~ = expression; <Negative>
  | e1 = expression; ~ = binop; e2 = expression; { Binary (binop, e1, e2) }
  | record_type = type_id; "{"; ~ = separated_nonempty_list(",", expr_record_field); "}"; <Record>
  (* We need some redundant indexing rules to work around shift/reduce conflicts. *)
  | element_type = type_id; "["; size = expression; "]"; "of"; init = expression; 
    { Array { element_type; size; init } }
  | ~ = ident; "["; size = expression; "]"; ":="; ~ = expression; { Assign (Subscript (Ident ident, size), expression) } 
  | ~ = ident; "["; size = expression; "]"; { Lvalue (Subscript (Ident ident, size)) } 
  (* Regular lvalue rules. *)
  | ~ = lvalue; <Lvalue>
  | ~ = lvalue; ":="; ~ = expression; <Assign>
  | "if"; cond = expression; "then"; then_ = expression; "else"; else_ = expression; { If { cond; then_; else_ = Some else_ } }
  | "if"; cond = expression; "then"; then_ = expression; { If { cond; then_; else_ = None } }
  | "while"; cond = expression; "do"; body = expression; { While { cond; body } }
  | "for"; ~ = ident; ":="; lo = expression; "to"; hi = expression; "do"; body = expression; { For { ident; lo; hi; body } }
  | "break"; { Break }
  | "let"; ~ = declarations; "in"; exps = separated_list(";", expression); "end"; { Let { declarations; exps } }
  | func = ident; "("; args = separated_list(",", expression); ")"; { Call { func; args } }


let expr_record_field :=
  | ~ = field_id; "="; ~ = expression; <>

let binop ==
  | "|";  { Binary_operator.Or }
  | "&";  { Binary_operator.And }
  | "<="; { Binary_operator.Le }
  | ">="; { Binary_operator.Ge }
  | "<";  { Binary_operator.Lt }
  | ">";  { Binary_operator.Gt }
  | "<>"; { Binary_operator.NotEqual }
  | "=";  { Binary_operator.Equal }
  | "-";  { Binary_operator.Minus }
  | "+";  { Binary_operator.Plus }
  | "/";  { Binary_operator.Divide }
  | "*";  { Binary_operator.Times }


let literal ==
  | ~ = "int"   ; <Literal.Int>
  | ~ = "string"; <Literal.String>

(* Lvalues *)

let lvalue :=
  | ~ = ident; <Ident> 
  | ~ = lvalue; "."; ~ = field_id; <Dot>
  | ~ = lvalue; "["; ~ = expression; "]"; <Subscript>

(* Identifiers *)

let type_id == 
  | ~ = Ident; <Type_id.of_string>

let field_id == 
  | ~ = Ident; <Field_id.of_string>

let ident ==
  | ~ = Ident; <Ident.of_string>
