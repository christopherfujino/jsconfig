let parse s =
  let lexbuf = Lexing.from_string s in
  Parser.doc Lexer.read lexbuf
