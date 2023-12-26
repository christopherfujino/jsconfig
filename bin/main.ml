open Jsconfig.Ast
open Jsconfig.Main

let program = "{\"Key\": [1,2,3]\n}"

let rec print_list l =
  match l with [] -> "" | h :: t -> print_expr h ^ ", " ^ print_list t

and print_expr e =
  match e with
  | Object (k, v) -> "{\"" ^ k ^ "\": " ^ print_expr v ^ "}"
  | Array a -> ( match a with [] -> "[]" | l -> "[" ^ print_list l ^ "]")
  | String s -> "\"" ^ s ^ "\""
  | Number n -> string_of_float n

(* |> is reverse application, avoiding nesting of function invocations *)
let () = parse program |> print_expr |> print_endline
