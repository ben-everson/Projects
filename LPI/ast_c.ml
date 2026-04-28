open Ast_shared

type c_exptype =
  | Int_Type
  | Bool_Type
  | Unknown_Type of int
[@@deriving show { with_path = false }]

type c_expr =
  | Integer of int
  | Boolean of bool
  | VarID of var
  | OpBin of op * c_expr * c_expr
  | OpNot of c_expr
  | Value
[@@deriving show { with_path = false }]

type stmt =
  | NoOp                                  (* For parser termination *)
  | Seq of stmt * stmt                    (* True sequencing instead of lists *)
  | Assign of string * c_exptype * c_expr
  | If of c_expr * stmt * stmt              
  | For of string * c_expr * c_expr * stmt    
  | While of c_expr * stmt                  (* Guard is an expr, body is a stmt *)
  | Print of c_expr                         (* Print the result of an expression *)
[@@deriving show { with_path = false }]

type value =
  | Int_Val of int
  | Bool_Val of bool
  | Unknown_Val

type environment = (string * value) list

let c = ref 0

let fresh () = let r = !c in c:= !c + 1; r

let reset () = c:= 0

