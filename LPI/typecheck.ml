open Ast_shared
open Ast_c

(*Provided fresh counter in ast_shared*)

(*copied helper methods from evaluator to handle context*)

(* Adds mapping [x:t] to context [con], if already present do nothing*)
let rec extend con x t = match con with 
| [] -> [(x,t)]
| (a,b)::rest when (a,b) = (x,t) ->(a,b)::rest
| (a,b)::rest -> (a,b)::(extend rest x t)
(*type of x will never change, doesnt need to be mutable with reference*)

(*returns union of contexts con1 and con2*)
let rec union con1 con2 = match con1 with 
| [] -> con2
| (a,b)::t -> union t (extend con2 a b) 


(* Returns [t] if [x:t] is a mapping in [con];*)
let rec lookup con x =
  match con with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, typ) :: rest -> if x = var then typ else lookup rest x

(*if typ1 is UT then typ1<: typ2 otherwise not a subtype*)
let subtypes typ1 typ2 = match typ1 with Unknown_Type(a) -> true | _ -> typ1 = typ2


(*check types of an expr instead of stmt, context cannot be changed
resolves expr to a single type*)
let rec exprtyper expr context = match expr with 
| Integer (a) -> Int_Type
| Boolean (b) -> Bool_Type

(*return the type of the variable in the given context*)
| VarID (varname) -> (lookup context varname)

(*check that types match with operator and return correct type*)
| OpBin (operator, expr1, expr2) -> (match operator with 
  (*if operator is for ints, check types and return int type*)
    | Add | Sub | Mult | Div -> if ((subtypes (exprtyper expr1 context) Int_Type) && 
          (subtypes (exprtyper expr2 context) Int_Type)) then 
              Int_Type else 
              raise (TypeError "47 cant add bools fool")

    (*if operator is for bools, check types and return bool type*)
    | And | Or -> if (( subtypes (exprtyper expr1 context) Bool_Type) && 
          (subtypes (exprtyper expr2 context) Bool_Type)) then 
              Bool_Type else 
              raise (TypeError "52 6 and 7?!?!")
    (*if op compares, check types are equal, return bool type*)
    | Equal | NotEqual | Greater | GreaterEqual | Less | LessEqual -> 
      if ((subtypes (exprtyper expr1 context) (exprtyper expr2 context)) ||
          (subtypes (exprtyper expr2 context) (exprtyper expr1 context))) then 
              Bool_Type else 
              raise (TypeError "57 5 > true? dumbass")
      )

  (* !x, x must be a boolean*)
| OpNot (expr1) -> if ((subtypes (exprtyper expr1 context) Bool_Type)) then Bool_Type
     else raise (TypeError "43 not (not bool)")

(*any user inputted value is resolved to an unkown type*)
| Value -> Unknown_Type (fresh ())




(*helper with added context parameter,
 type checks stmt and returns true, context if stmt type checks and 
  raises an error if it does not*)
let rec typecheckc stmt context = match stmt with 
| NoOp -> true, context

(*evaluate the first expression and save (result,context), 
  evaluate second expr in new context and return expr1&&expr2, 
  and context updated from expr2*)
  (*expr1 and 2 should rlly be stmt 1 and 2 but im laxy*)
| Seq(expr1, expr2) -> let (expr1_checked, new_context) = (typecheckc expr1 context) in 
    let (expr2_checked, new_new_context) = (typecheckc expr2 new_context) in
    (expr1_checked && expr2_checked, new_new_context)

(*assign new var varname of type typ to value expr1*)
| Assign(varname, typ, expr2) -> 
  (*if expr2 type checks, and if tpy <: expr2 or expr2 <: typ*)
  let expr2_type = exprtyper expr2 context in 
  if ((subtypes typ expr2_type) || (subtypes expr2_type typ))
    then let new_context = (extend context varname expr2_type) in (true, new_context)
    else raise (TypeError "bool x cant = 5 idiot")

  (*ensures condition is a bool and evaluares stm1 and stm2 in 
  starting context then returns union of their contexts*)
| If (condition_expr, stmt1, stmt2) ->
    let (res1, con1) = (typecheckc stmt1 context) in 
    let (res2, con2) = (typecheckc stmt2 context) in 
    if ( (subtypes (exprtyper condition_expr context) Bool_Type) && (res1 && res2) ) 
        then (true, union con1 con2)
        else raise (TypeError "91 bad" )

      (*adds iter_var to context if not already, then checks increment and
        condition are both ints then returns result of type checking body*)
| For (iter_var, increment_expr, condition_expr, body) -> 
    let new_context = (extend context iter_var Int_Type) in 
    let increment_expr_typ = exprtyper increment_expr new_context in 
    let condition_expr_typ = exprtyper condition_expr new_context in 
    if ((subtypes (lookup new_context iter_var) Int_Type) && (subtypes increment_expr_typ Int_Type) &&
        (subtypes condition_expr_typ Int_Type)) 
      then typecheckc body new_context
      else raise (TypeError "110 bad for llop")

(*ensures condition is boolean then returns result of evaluating body*)
| While (condition, body) -> let cond_typ = exprtyper condition context in 
    if (subtypes cond_typ Bool_Type) then typecheckc body context 
      else raise (TypeError "110 While 3?!?")

(*print expr type checks to true and context is unchanged*)
| Print (expr) -> let expr_type = exprtyper expr context in (true, context)

      
       
      

let typecheck stmt = match typecheckc stmt [] with (res1, context) -> res1 | _ -> raise (TypeError "uuh oh")

(* let typecheck stmt = failwith "testing" *)
