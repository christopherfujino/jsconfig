type value =
  (* TODO: should be a list of kvps *)
  | Object of string * value
  | Array of value list
  | Number of float
  | String of string

