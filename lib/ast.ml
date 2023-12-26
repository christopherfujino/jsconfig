type value =
  | Object of (string * value) list
  | Array of value list
  | Number of float
  | String of string
  | Null
  | True
  | False
