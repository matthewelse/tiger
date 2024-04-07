%{
open Ast
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

%start<Ast.expression> program 

%%

let program := ~ = expression; Eof; <>

(* Declarations *)

let declarations == list(declaration)

let declaration :=
  | ~ = type_declaration;     <DType>
  | ~ = variable_declaration; <DVariable>
  | ~ = function_declaration; <DFunction>


(* Global variable declarations *)

let variable_declaration ==
  | "var" ; ~ = ident; ":="; ~ = expression; { {ident; type_id = None ; expression } }
  | "var" ; ~ = ident; ":"; ~ = type_id; ":="; ~ = expression; { {ident; type_id = Some type_id; expression } }

(* Function declarations *)

let function_declaration ==
  | "function"; ~ = ident; "("; args = func_params; ")"; "=";
    body = expression;
    { { ident; args; return_type = None; body } }
  | "function"; ~ = ident; "("; args = func_params; ")"; ":"; return_type = type_id; "=";
    body = expression;
    { { ident; args; return_type = Some return_type; body } }


let func_params == separated_list(",", func_param)

let func_param :=
  | ~ = ident; ":"; ~ = type_id; <>

(* Type declarations *)

let type_declaration ==
  | "type"; ~ = type_id; "="; ~ = type_desc; <>

let type_desc :=
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
  | ~ = literal; <ELiteral>
  | "-"; ~ = expression; <ENegative>
  | e1 = expression; ~ = binop; e2 = expression; { EBinary (binop, e1, e2) }
  | record_type = type_id; "{"; ~ = separated_nonempty_list(",", expr_record_field); "}"; <ERecord>
  (* We need some redundant indexing rules to work around shift/reduce conflicts. *)
  | element_type = type_id; "["; size = expression; "]"; "of"; init = expression; 
    { EArray { element_type; size; init } }
  | ~ = ident; "["; size = expression; "]"; ":="; ~ = expression; { EAssign (LSubscript (LIdent ident, size), expression) } 
  | ~ = ident; "["; size = expression; "]"; { ELvalue (LSubscript (LIdent ident, size)) } 
  (* Regular lvalue rules. *)
  | ~ = lvalue; <ELvalue>
  | ~ = lvalue; ":="; ~ = expression; <EAssign>
  | "if"; cond = expression; "then"; then_ = expression; "else"; else_ = expression; { EIf { cond; then_; else_ = Some else_ } }
  | "if"; cond = expression; "then"; then_ = expression; { EIf { cond; then_; else_ = None } }
  | "while"; cond = expression; "do"; body = expression; { EWhile { cond; body } }
  | "for"; ~ = ident; ":="; lo = expression; "to"; hi = expression; "do"; body = expression; { EFor { ident; lo; hi; body } }
  | "break"; { EBreak }
  | "let"; ~ = declarations; "in"; exps = separated_list(";", expression); "end"; { ELet { declarations; exps } }
  | func = ident; "("; args = separated_list(",", expression); ")"; { ECall { func; args } }


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


let literal ==
  | ~ = "int"; <LInt>
  | ~ = "string"; <LString>

(* Lvalues *)

let lvalue :=
  | ~ = ident; <LIdent> 
  | ~ = lvalue; "."; ~ = field_id; <LDot>
  | ~ = lvalue; "["; ~ = expression; "]"; <LSubscript>

(* Identifiers *)

let type_id == 
  | ~ = Ident; <Type_id.of_string>

let field_id == 
  | ~ = Ident; <Field_id.of_string>

let ident ==
  | ~ = Ident; <Ident.of_string>
