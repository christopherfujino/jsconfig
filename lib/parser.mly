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
%token TRUE
%token FALSE

(* Precedence & Associativity *)

(* N/A, I think... *)

(* Declare the starting point for parsing *)

%start <value> doc

%%

doc:
  | o = obj; EOF { o }
  ;

value:
  | n = NUM { Number n }
  | s = STRING { String s }
  | o = obj { o }
  | a = arr { a }
  | NULL { Null }
  | TRUE { True }
  | FALSE { False }
  ;

obj:
  | OPEN_CURLY; entries = obj_entries; CLOSE_CURLY { Object entries }
  | OPEN_CURLY; entries = obj_entries; COMMA; CLOSE_CURLY { Object entries }
  ;

obj_entries:
  | e = obj_entry { [e] }
  | l = obj_entries ; COMMA ; e = obj_entry { l @ [e] }
  ;

obj_entry:
  | k = STRING; COLON; v = value; { (k, v) }
  ;

arr:
  | OPEN_BRACKET ; e = value_list ; CLOSE_BRACKET { Array e }
  | OPEN_BRACKET ; e = value_list ; COMMA; CLOSE_BRACKET { Array e }
  ;

value_list:
  | (* empty *)                        { [] }
  | v = value                          { [v] }
    (* h @ [v] concatenates h and v *)
  | h = value_list ; COMMA ; v = value { h @ [v] }
  ;
