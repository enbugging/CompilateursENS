(******************************************************************************)
(* phase 2 : production de code *)

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

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
                        

let compile_stmt (codefun, codemain) = function


let compile_program p ofile =
  let p = alloc p in
  Format.eprintf "%a@." print p;
  let codefun, code = List.fold_left compile_stmt (nop, nop) p in
  let p =
    { text =
        globl "main" ++ label "main" ++
        movq !%rsp !%rbp ++
        code ++
        movq (imm 0) !%rax ++ (* exit *)
        ret ++
        label "print_int" ++
        movq !%rdi !%rsi ++
        movq (ilab ".Sprint_int") !%rdi ++
        movq (imm 0) !%rax ++
        call "printf" ++
        ret ++
        codefun;
      data =
        Hashtbl.fold (fun x _ l -> label x ++ dquad [1] ++ l) genv
          (label ".Sprint_int" ++ string "%d\n")
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f
