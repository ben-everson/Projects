open Ast_shared

type label = Lab of var [@@deriving show { with_path = false }]

type o_exptype =
  | TInt
  | TBool
  | TTick of int
  | TArrow of o_exptype * o_exptype
  | TRec of (label * o_exptype) list
  | TScheme of (int list * o_exptype)
[@@deriving show { with_path = false }]

type o_expr =
  | Int of int
  | Bool of bool
  | ID of var
  | Binop of op * o_expr * o_expr
  | Not of o_expr
  | If of o_expr * o_expr * o_expr
  | Fun of var * o_expr
  | App of o_expr * o_expr
  | Record of (label * o_expr) list
  | Select of label * o_expr
  | Let of var * bool * o_expr * o_expr
[@@deriving show { with_path = false }]

type context = (string * o_exptype) list

type constraints = (o_exptype * o_exptype) list
