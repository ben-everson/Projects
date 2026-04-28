open Ast_o

val unify : constraints -> constraints

val substitute : constraints -> o_exptype -> o_exptype

val infer: o_expr -> o_exptype
