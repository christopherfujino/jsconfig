open Jsconfig.Ast
open Jsconfig.Main

let program = "{\
  \"Key\": \"Value\"
}"

let rec print_expr e = match e with
  | Object (k, v) -> "{\"" ^ k ^ "\": " ^ print_expr v ^ "}"
  | Array a -> (match a with
    | [] -> "[]"
    | h :: t -> "[" ^ print_expr h ^ ", " ^ print_expr (Array t) ^ "]")
  | String s -> "\"" ^ s ^ "\""
  | Number n -> string_of_float n

(* |> is reverse application, avoiding nesting of function invocations *)
let () = program |> parse |> print_expr |> print_endline
