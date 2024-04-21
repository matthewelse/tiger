open! Core

let parse str =
  let lexbuf = Lexing.from_string str in
  let buffer, lex = MenhirLib.ErrorReports.wrap Tiger.Lexer.read in
  let expr =
    try Tiger.Parser.program lex lexbuf with
    | Tiger.Parser.Error _ ->
      let position = Lexing.lexeme_start_p lexbuf in
      let error =
        MenhirLib.ErrorReports.show
          (fun ((start : Lexing.position), (end_ : Lexing.position)) ->
            Lexing.sub_lexeme lexbuf start.pos_cnum end_.pos_cnum)
          buffer
      in
      raise_s
        [%message
          "Parser error while processing token"
            (error : string)
            (position : Source_code_position.t)]
  in
  expr
;;

let test_parser str =
  Expect_test_helpers_core.print_s ([%sexp_of: Tiger.Ast.Expression.t] (parse str))
;;

let test_lexer str =
  let lexer = Lexing.from_string str in
  let tokens = Queue.create () in
  while
    match Tiger.Lexer.read lexer with
    | Eof -> false
    | token ->
      Queue.enqueue tokens token;
      true
  do
    ()
  done;
  Expect_test_helpers_core.print_s ([%sexp_of: Tiger.Token.token Queue.t] tokens)
;;

let test_interpreter str =
  let expr = parse str in
  let value = Tiger.Interpret.run expr in
  print_s [%sexp (value : Tiger.Interpret.Value.t)]
;;

let test_typing str =
  let expr = parse str in
  let value = Tiger.Typing.For_testing.type_of_expression expr in
  print_s [%sexp (value : Tiger.Type.t)]
;;
