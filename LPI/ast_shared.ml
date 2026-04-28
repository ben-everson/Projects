exception InvalidInputException of string
exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let fresh = 
  let count = ref 0 in
  fun () -> count := !count + 1; !count

type op =
  | Add
  | Sub
  | Mult
  | Div
  | Greater
  | Less
  | GreaterEqual
  | LessEqual
  | Equal
  | NotEqual
  | Or
  | And
[@@deriving show { with_path = false }]

type var = string [@@deriving show { with_path = false }]