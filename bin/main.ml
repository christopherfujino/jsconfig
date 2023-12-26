open Jsconfig.Ast
open Jsconfig.Main

let programs =
  [
    {|{"array": [1,2,3]}|};
    {|{"nested array": [[], [1], ["a"]]}|};
    {|{"null": null}|};
    {|{"booleans": [true, false]}|};
    {|{"multiple": null, "fields": null}|}
  ]

let rec print_list l =
  match l with [] -> "" | h :: t -> print_expr h ^ ", " ^ print_list t

and print_expr e =
  match e with
  | Object (k, v) -> "{\"" ^ k ^ "\": " ^ print_expr v ^ "}"
  | Array a -> ( match a with [] -> "[]" | l -> "[" ^ print_list l ^ "]")
  | String s -> "\"" ^ s ^ "\""
  (* TODO handle ints nicely *)
  | Number n -> string_of_float n
  | Null -> "null"
  | True -> "true"
  | False -> "false"

let rec print_programs = function
  | [] -> ()
  | h :: t ->
      try let value = parse h in
      (* |> is reverse application, avoiding nesting of function invocations *)
      print_expr value |> print_endline;
      print_programs t with Jsconfig.Parser.Error -> failwith h

let () = print_programs programs
