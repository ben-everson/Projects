open Ast_shared
open Ast_o

(*Provided fresh counter in ast_shared*)

let rec string_typer type_expr = match type_expr with 
  | TInt -> "TInt"
  | TBool -> "TBool"
  | TTick(x) -> "TTick("^ (string_of_int x) ^")" (* this represents a polymorphic type: 'a, 'b, ... *)
  | TArrow(a,b)-> "TArrow("^(string_typer a)^ ", " ^ (string_typer b) ^ ")"  (* this represents a function: 'a -> 'b *)
  | TRec fields -> let field_strs = 
    List.map (fun (Lab(lbl), ty) -> "(" ^ lbl ^ ", " ^ string_typer ty ^ ")") fields in
      "TRec([" ^ String.concat "; " field_strs ^ "])"
  | _ -> "{I HATE SCHEMES}"

let string_constrainers constrs = "[" ^ (List.fold_left
    (fun acc (a, b) ->
       let s = "(" ^ string_typer a ^ " : " ^ string_typer b ^ ")" in
       if acc = "" then s else acc ^ ", " ^ s)
    "" constrs) ^ "]"



(*can once again use lookup function *)
(* Adds constraint a:b to con *)
let rec extend con x t = (x,t)::con

(* Returns [b] if [a:b] is a constraint in [con];*)
let rec lookup con x =
  match con with
  | [] -> failwith "not in context" 
  | (a, b) :: rest -> if x = a then b else lookup rest x

(* (*i dont trust list.fold when i have bugs
takes in a list (lbl, expr_type) and to_remove, new_value -replaces to_remove with new_value
in expr_type*)
let rec replace_in_rec *)

(*takes in some o_exptype, TTick(to_remove), new_value, replaces all occurences
of TTick(to_remove in cur_type with new_value)*)
let rec replace cur_type to_remove new_value = match cur_type with 
  | TInt -> TInt
  | TBool -> TBool
  | TTick(x) -> if x = to_remove then new_value else TTick(x)
  | TArrow(typ1, typ2) -> TArrow((replace typ1 to_remove new_value), (replace typ2 to_remove new_value))

  (*maps each label,type pair to itself if type<>to_remove or label, new_value if b=to_remove*)
  | TRec(label_expr_list) -> TRec(List.map (fun (a, b) -> (a, replace b to_remove new_value)) label_expr_list)
  | _ -> failwith "42 you schemey bastard"

(* searches through constraints and replaces all occurrences of TTick(x) with new_type *)
let update_constraints constraints x new_type =
  List.map (fun (a, b) -> (replace a x new_type, replace b x new_type)) constraints

(*checks if TTick(x) is in the given texpr CAUTION: true if texpr=TTick(x)*)
let rec check_occ x texpr = match texpr with 
  | TInt | TBool -> false
  | TTick(a) -> a = x
  | TArrow(a,b) -> check_occ x a || check_occ x b

  (*fold through list of label*value and return true if x is in value for any entry*)
  | TRec(ls) -> List.exists (fun (_, b) -> check_occ x b) ls
  | _ -> failwith "shcemer"


let rec unify constr = match constr with 
| (TTick(x), TTick(y))::t when x=y -> unify t 
| (TInt, TInt)::t | (TBool, TBool)::t -> unify t (*if a=b then constraint is not helpful *)
| (TInt, TBool)::t | (TBool, TInt)::t -> failwith "35 mismatched types"

(*if 'b <> 'a but 'b contains 'a*)
| (TTick(x), b)::t when (TTick(x) <> b  && (check_occ x b))-> failwith "49 infinite recursion a:(a->b)"
| (b, TTick(x))::t when (TTick(x) <> b  && (check_occ x b))-> failwith "49 infinite recursion a:(a->b)"

(*split up two functions a->b:c->d becomes a:c,b:d*)
| (TArrow(p1,b1), TArrow(p2,b2))::t ->  unify ((p1,p2)::(b1,b2)::t)

(*replace all occurences of TTick(x) in t with b*)
(*maps each texpr in t to itself with TTick(x)s replaced by b*)
| (TTick(x), b)::t -> (TTick(x), b) :: (unify (update_constraints t x b))
(*now backwards this time, try to preserve order for substitutions*)
| (b, TTick(x))::t -> (TTick(x), b) :: (unify (update_constraints t x b))
| (TRec(list1), TRec(list2))::t -> unify ((check_rec list1 list2) @ t)
| [] -> []
| _ -> failwith "i didnt think of it so def not possible"

and check_rec lista listb = match lista, listb with 
| (lbl1, exprtyp1)::t1, (lbl2,exprtyp2)::t2-> if lbl1 <> lbl2 
    then failwith "mismatched fields"
    else (exprtyp1,exprtyp2)::check_rec t1 t2
| [],[] -> []
| _ -> failwith "mismatched reckordsss"

(*iterates through constraints and replaces a with b in tentative type*)
let rec substitute constraints tentative_type = match constraints with 
| (TTick(a),b)::t -> substitute t (replace tentative_type a b)
| [] -> tentative_type
| _ -> failwith "bad constrain, unifier messed up?"


(*takes in record and label, returns (label, expr) that matches given label*)
let find_field rec_expr_type label = match rec_expr_type with 
(*returns the first instance in list where (a,b): a = label*)
| TRec (list) -> List.find (fun (a,b)-> a = label) list
| _ -> print_string(match label with Lab(str) -> str); failwith "77 I'm not a record :3"

let rec gen_constraints expr constraint_list context = match expr with 
| Int (x) as a -> (TInt, [], context)
| Bool (x) as a -> (TBool, [], context)
| ID (x) as a -> (lookup context x, [], context) (*primitive types dont add any constraints*)
| Binop (op, expr1, expr2) as a -> ( 
    match op with 
      | Add | Sub | Div | Mult -> 
        let (typ1, con1, ctxt1) = gen_constraints expr1 constraint_list context in 
        let (typ2, con2, ctxt2) = gen_constraints expr2 constraint_list context in
        let ret_typ = TTick(fresh ()) in 
        (*returns new type-a- generate constraints for each expr and t1:t2, t1:int, a:int*)
        (ret_typ, (typ1, typ2)::(typ1, TInt)::(ret_typ, TInt)::con1 @ con2, context)
      
        | And | Or ->
        let (typ1, con1, ctxt1) = gen_constraints expr1 constraint_list context in 
        let (typ2, con2, ctxt2) = gen_constraints expr2 constraint_list context in
        let ret_typ = TTick(fresh ()) in 
        (*returns new type-a- generate constraints for each expr and t1:t2, t1:int, a:int*)
        (ret_typ, (typ1, typ2)::(typ1, TBool)::(ret_typ, TBool) :: con1 @ con2, context)
      
        | Equal | NotEqual | Less | Greater | LessEqual | GreaterEqual ->
        let (typ1, con1, ctxt1) = gen_constraints expr1 constraint_list context in 
        let (typ2, con2, ctxt2) = gen_constraints expr2 constraint_list context in
        let ret_typ = TTick(fresh ()) in 
        (*returns new type-a- generate constraints for each expr and t1:t2, t1:int, a:bool*)
        (ret_typ, (typ1, typ2)::(ret_typ, TBool) :: con1 @ con2, context)
      )
| Not (expr) as a -> let (typ1,con1,ctxt1) = (gen_constraints expr constraint_list context) in 
        let ret_typ = TTick(fresh ()) in 
        (ret_typ, (typ1, TBool)::(ret_typ, TBool)::con1, context) 

(*cond_expr:bool, true_*)
| If (cond_expr, true_expr, false_expr) ->
    let (typ1, con1, ctxt1) = gen_constraints cond_expr constraint_list context in 
    let (typ2, con2, ctxt2) = gen_constraints true_expr constraint_list context in
    let (typ3, con3, ctxt3) = gen_constraints false_expr constraint_list context in
    let ret_typ = TTick(fresh ()) in 
    (*resulting type is ret_typ, add conditions typ1:bool, typ2:typ3*)
      (ret_typ, (ret_typ, typ2)::(typ1, TBool):: (typ2, typ3)::con1 @ con2 @ con3, context)


| Fun (var, expr) -> 
    (*create new context where var has type 'a*)
    let new_context = extend context var (TTick(fresh ())) in
    (*generate constraints in new context*)
    let (typ1, con1, ctxt1) = gen_constraints expr constraint_list new_context in 
    (*TODO resulting context from creating constraints doesnt matter?*)
    let ret_typ = TTick( fresh ()) in
    (*add constraint that the function is of type TArrow {type of x} -> {type of expr}*)
      (ret_typ, (ret_typ, TArrow(lookup new_context var, typ1))::con1, context) (*TODO should return new_context or context? is X defined in outside context*)

| App (func, arg) -> 
    let (typ1, con1, ctxt1) = gen_constraints func constraint_list context in 
    let (typ2, con2, ctxt2) = gen_constraints arg constraint_list context in 
    let ret_typ = TTick( fresh ()) in 
    (*typ1 must be a function of {typ2} -> ret_typ  typ1:(typ2->ret_typ)*)
    (ret_typ, ((typ1, TArrow(typ2, ret_typ)):: con1 @con2), context)

| Record (label_value_list) -> 
  (*process record to get result list of label:exptype and generated constraints*)
    (match (process_record label_value_list constraint_list context) with 
      | (result_list, constraint_list) -> (TRec(result_list), constraint_list, context)
    )

| Select (lab, rec_expr) ->
    let (typ1, con1, ctxt1) = gen_constraints rec_expr constraint_list context in 
    (*find field in the record_type entry from the rec_expr instead of rec_expr itself*)
( match find_field typ1 lab with (label, lab_type) -> 
      (lab_type, con1, context))


| Let (var, rec_bool, val_expr, res_expr) -> if rec_bool 

  then (*recursive let binding*)
  (*in recursive let binding, extend context first then add extra constraints*)
    let new_typ = TTick (fresh ()) in 
    let new_context = (extend context var new_typ) in
    let (typ1, con1, ctxt1) = gen_constraints val_expr constraint_list new_context in 
    let new_context = (extend context var typ1) in(*new context instead has var:t1, 
    could also maybe possibly keep var:tx and add constraint tx:t1? #TODO*)
    let (typ2, con2, ctxt2) = gen_constraints res_expr constraint_list new_context in 
    let ret_typ = TTick (fresh ()) in 
    (ret_typ, (ret_typ, typ2)::con1@con2, context)


  else (*non recursive let bindings*)
    let (typ1, con1, ctxt1) = gen_constraints val_expr constraint_list context in 
    (*evalute the result expr in context from first, with added var:typ1*)
    let new_context = extend context var typ1 in (*CHANGED from ctxt1 to context*)
    let (typ2, con2, ctxt2) = gen_constraints res_expr constraint_list new_context in 
    let ret_typ = TTick (fresh ()) in 
    (ret_typ, (ret_typ, typ2)::con1 @ con2, context)

(*#TODO prolly shit idk
takes in a list of (label,expr), list of constraints, context
returns a tuple of (label, expr_type) list, constraints list*)
and process_record list constraints context = match list with 
(*generate constraints for each entry in record*)
| (label,expr)::t -> let (typ1, con1, ctxt1) = gen_constraints expr constraints context in
  (match (process_record t constraints context) with 
  (*after processing rest of list, add corresponding label:type to ret_list and take union of constraints*)
    | (return_list, constraints_list) -> ((label, typ1)::return_list,con1 @ constraints_list)
  )
| [] -> ([], [])
    
(*generates constraints for expression t, then unifies constraints then substitutes them in tent_type*)
let infer t = let (tent_type, constrainers, context) = gen_constraints t [] [] in
  print_string ("tent_type: "^ (string_typer tent_type)^ "\n");
  print_string ((string_constrainers constrainers) ^ "\n");
  let unified = unify constrainers in
  print_string( string_constrainers unified);
  let substituted = substitute unified tent_type in substituted

(*(fun x -> fun y->let sum = x+y in fun z->{product=sum*z;is_large=sum>10}) 2 6*)