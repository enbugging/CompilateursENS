open Preprocessing.Ast

type ident = string

type p_const = constant

type p_expr = 
        | PConstant of p_const*typ
        | PVariable of int (*décalage par rapport à bp*)
	| PTypedExpression of p_expr * typ
	| PBinaryOperation of p_expr * binaryOperation * p_expr * typ
	| PConditional of p_expr * p_expr * p_expr * typ
        | PExplicitConstructor of ident * p_expr list * typ
	| FunctionCall of ident * p_expr list * typ
	| Do of p_expr list
	| Let of (ident * p_expr) list * p_expr * typ
	| Case of p_expr * (pattern * p_expr) list * typ

type p_def = PDefinition of ident * patarg list * p_expr * typ

type p_decl =
        | PTypeDeclaration of ident * typ list * typ list * typ list * p_def
	| PData of ident * typ list * constructor list
	| PClass of ident * typ list * p_decl list
	| PInstance of typ list * p_decl list

type p_file = PFile of p_decl list
