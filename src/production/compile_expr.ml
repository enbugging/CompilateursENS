incldue X86_64
open Past
let rec compile_expr = function
    (*| PConstant of p_const*typ
    | PVariable of int (*décalage par rapport à bp*)
	| PTypedExpression of p_expr * typ
	| PBinaryOperation of p_expr * binaryOperation * p_expr * typ
	| PConditional of p_expr * p_expr * p_expr * typ
    | PExplicitConstructor of int * p_expr list * typ
	| PFunctionCall of ident * p_expr list * typ
	| PDo of p_expr list
	| PLet of (int * p_expr) list * p_expr * typ
	| PCase of p_expr * (pattern * p_expr) list * typ*)

	| PConstant (Boolean b,_) -> pushq (imm (if b then 1 else 0))
	| Constant (Integer i,_) -> pushq (imm i)
	| Constant (String s,_) -> nop (*TODO*)
	| PVariable x -> pushq (ind ~ofs:x rbp)
	| PTypedExpression (e,t) -> nop (*TODO*)
	| PBinaryOperation (e1, (Plus | Minus | Times | Divide) as binop, e2, t) -> 
                        compile_expr e1 ++ 
                        compile_expr e2 ++
                        popq rbx ++ popq rax ++
                        (match binop with
                        | Plus -> addq !%rbx !%rax
                        | Minus -> subq !%rbx !%rax
                        | Times -> imulq !%rbx !%rax
                        | Divide -> cqto ++ idivq !%rbx ) ++ !%rax
	| PBinaryOperation(e1, Modulo, e2,t) -> 
					compile_expr e1 ++
					compile_expr e2 ++
					popq rbx ++ popq rax ++
					cqto ++ idivq !%rbx
					++ !%rdx
	| PFunctionCall ("log",[e],t) -> nop
									
	| PFunctionCall ("Show",[e],t) -> nop
	| _ -> nop