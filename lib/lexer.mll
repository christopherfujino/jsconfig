(* header *)
{
open Parser

exception SyntaxError of string
}

(* identifiers *)
let white = [' ' '\t' '\n']
let digit = ['0'-'9']
let number = '-'? digit+

(* rule and parse are keywords *)
rule read =
  parse
  (* means if `white` matches, call the read rule again and return its
     results--i.e. skip this match *)
  | white     { read lexbuf }
  | ','       { COMMA }
  | '['       { OPEN_BRACKET }
  | ']'       { CLOSE_BRACKET }
  | "{"       { OPEN_CURLY }
  | "}"       { CLOSE_CURLY }
  | ":"       { COLON }
  | number    { NUM (float_of_string (Lexing.lexeme lexbuf))}
  | '"'       { read_string (Buffer.create 17) lexbuf }
  | ','       { COMMA }
  (* Here `eof` is a special regex built into ocamllex *)
  | eof       { EOF }
  | _ as c { failwith (Printf.sprintf "unexpected character: %C" c) }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | eof { raise (SyntaxError "String is not terminated") }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
