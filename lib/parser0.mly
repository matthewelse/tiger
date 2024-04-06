%{
open Ast
%}

%token<string> Int    "int"
%token<string> String "string"
%token<string> Ident
// %token Bang         "!"
%token Colon        ":"
%token Dot          "."
%token Comma        ","
%token Minus        "-"
%token Plus         "+"
%token Slash        "/"
%token Star         "*"
%token Ampersand    "&"
%token Pipe         "|"
%token Lparen       "("
%token Rparen       ")"
%token Lbracket     "["
%token Rbracket     "]"
%token Lbrace       "{"
%token Rbrace       "}"
%token Semicolon    ";"
%token QuestionMark "?"
%token Let          "let"
%token In           "in"
%token If           "if"
%token Then         "then"
%token Else         "else"
%token NotEqual     "<>"
%token Equal        "="
%token LessEqual    "<="
%token GreaterEqual ">="
%token Greater      ">"
%token Less         "<"
%token Assign       ":="
%token Type         "type"
%token Array        "array"
%token Of           "of"
%token Var          "var"
%token Nil          "nil"
%token While        "while"
%token Do           "do"
%token For          "for"
%token To           "to"
%token Break        "break"
%token End          "end"
%token Function     "function"
%token Eof

// %nonassoc In

%nonassoc Else
%nonassoc Of

%nonassoc Assign
%left Pipe, Ampersand
%nonassoc LessEqual, GreaterEqual, Less, Greater, NotEqual, Equal
%left Plus, Minus
%left Star, Slash

%start<Ast.decs> decs

%%

let decs := ~ = list(dec); Eof; <>

let dec :=
  | ~ = type_dec; <DType>
  | ~ = var_dec ; <DVar>
  | ~ = func_dec; <DFunc>


(* Global variable declarations *)

let var_dec :=
  | "var" ; ~ = ident; "="; ~ = expression; { {ident; type_id = None ; expression } }
  | "var" ; ~ = ident; ":"; ~ = type_id; "="; ~ = expression; { {ident; type_id = Some type_id; expression } }

(* Function declarations *)

let func_dec :=
  | "function"; ~ = ident; "("; fields = func_params; ")"; "=";
    body = expression;
    { { ident; fields; return_type = None; body } }
  | "function"; ~ = ident; "("; fields = func_params; ")"; ":"; return_type = type_id; "=";
    body = expression;
    { { ident; fields; return_type = Some return_type; body } }


let func_params == separated_nonempty_list(",", func_param)

let func_param :=
  | ~ = ident; ":"; ~ = type_id; <>

(* Type declarations *)

let type_dec :=
  | "type"; ~ = type_id; "="; ~ = type_; <>

let type_ :=
  | ~ = type_id;                 <TIdent>
  | "{"; ~ = record_fields; "}"; <TRecord>
  | "array"; "of"; ~ = type_id;  <TArray>

let record_fields == separated_nonempty_list(",", record_field)

let record_field :=
  | ~ = field_id; ":"; ~ = type_id; <>

(* Expressions *)

let expression := 
  | ~ = one_expression; <>
  | "("; ~ = separated_list(";", one_expression); ")"; <ESequence>

let one_expression := 
  | "nil"; { ENil }
  | ~ = const; <EConst>
  | "-"; ~ = expression; <ENegative>
  | e1 = expression; ~ = binop; e2 = expression; { EBinary (binop, e1, e2) }
  | "{"; ~ = separated_nonempty_list(",", expr_record_field); "}"; <ERecord>
  (* TODO: deal with the shift/reduce conflict between this rule and lvalue. *)
  | element_type = type_id; "["; size = expression; "]"; "of"; init = expression; 
  { EArray { element_type; size; init } }
  | ~ = lvalue; ":="; ~ = expression; <EAssign>
  | ~ = lvalue; <ELvalue>
  | "if"; cond = expression; "then"; then_ = expression; "else"; else_ = expression; { EIf { cond; then_; else_ = Some else_ } }
  | "if"; cond = expression; "then"; then_ = expression; { EIf { cond; then_; else_ = None } }
  | "while"; cond = expression; "do"; body = expression; { EWhile { cond; body } }
  | "for"; ~ = ident; ":="; lo = expression; "to"; hi = expression; "do"; body = expression; { EFor { ident; lo; hi; body } }
  | "break"; { EBreak }
  | "let"; ~ = decs; "in"; exps = separated_list(";", expression); "end"; { ELet { decs; exps } }


let expr_record_field :=
  | ~ = field_id; "="; ~ = expression; <>

let binop ==
  | "|";  { Or }
  | "&";  { And }
  | "<="; { Le }
  | ">="; { Ge }
  | "<";  { Lt }
  | ">";  { Gt }
  | "<>"; { NotEqual }
  | "=";  { Equal }
  | "-";  { Minus }
  | "+";  { Plus }
  | "/";  { Divide }
  | "*";  { Times }


let const ==
  | ~ = "int"; <CInt>
  | ~ = "string"; <CString>

(* Lvalues *)

let lvalue :=
  | ~ = ident; <LIdent> 
  | ~ = lvalue; "."; ~ = ident; <LDot>
  | ~ = lvalue; "["; ~ = expression; "]"; <LSubscript>

(* Identifiers *)

let type_id == 
  | ~ = Ident; <Type_id.of_string>

let field_id == 
  | ~ = Ident; <Field_id.of_string>

let ident ==
  | ~ = Ident; <Ident.of_string>
