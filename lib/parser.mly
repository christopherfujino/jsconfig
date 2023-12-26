(* Menhir *)

(* header *)
%{
  open Ast
%}

(* Declarations *)
%token <float> NUM
%token <string> STRING
%token OPEN_CURLY
%token CLOSE_CURLY
%token OPEN_BRACKET
%token CLOSE_BRACKET
%token COMMA
%token COLON
%token EOF
%token NULL

(* Precedence & Associativity *)

(* N/A, I think... *)

(* Declare the starting point for parsing *)

%start <value> doc

%%

doc:
  | o = obj; EOF { o }

value:
  | n = NUM { Number n }
  | s = STRING { String s }
  | o = obj { o }
  | a = arr { a }
  | NULL { Null }
  ;

obj:
  | OPEN_CURLY; k = STRING; COLON; v = value; CLOSE_CURLY; EOF { Object (k, v) }
  ;

arr:
  | OPEN_BRACKET ; e = value_list ; CLOSE_BRACKET { Array e }
  ;

value_list:
  | (* empty *)                        { [] }
  | v = value                          { [v] }
    (* h @ [v] concatenates h and v *)
  | h = value_list ; COMMA ; v = value { h @ [v] }
