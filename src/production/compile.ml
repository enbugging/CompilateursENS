open Format
open X86_64
open Preprocessing.Ast
open Past

(* phase 1 : allocation des variables *)

exception VarUndef of string

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

let reg_size = 64

module Smap = Map.Make(String)

type local_env = ident Smap.t

let rec alloc_expr (env: local_env) (fpcur: int) = function
        | TConstant (c,t) -> PConstant (c,t), fpcur
	| TVariable (x,t) -> begin match Smap.find_opt x with
                                | Some(dec) -> PVariable dec, fpcur
                                | None -> raise (VarUndef x)
                                end

	| TTypedExpression (e, t) -> alloc_expr env fpcur e

	| TBinaryOperation (e1, binop, e2, t) -> 
                        let e1, fpmax1 = alloc_expr env fpcur e1 in
                        let e2, fpmax2 = alloc_expr env fpcur e2 in
                        PBinaryOperation (e1, binop, e2, t), max fpmax1 fpmax2

	| TConditional (e1, e2, e3, t) ->
                        let e1, fpmax1 = alloc_expr env fpcur e1 in
                        let e2, fpmax2 = alloc_expr env fpcur e2 in
                        let e3, fpmax3 = alloc_expr env fpcur e3 in
                        PConditional (e1,e2,e3,t), max (max fpmax1 fpmax2) fpmax3

        | TExplicitConstructor (x, e_list,t) -> let fpcur', e_list' = List.fold_left (fun (fpmax,l) e ->
                                                                let e', fpmax' = alloc_expr env fpcur e in (max fpmax fpmax', e'::l)) (fpcur,[]) e_list in PExplicitConstructor (x, e_list', t), reg_size+fpcur'

	| TFunctionCall (f, instances, e_list,t) -> let fpcur', e_list' = List.fold_left (fun (fpmax,l) e ->
                                                                let e', fpmax' = alloc_expr env fpcur e in (max fpmax fpmax', e'::l)) (fpcur,[]) e_list in PFunctionCall (x, instances, e_list', t), reg_size*(1 + List.length instances)+fpcur'


	| TDo (e_list) -> let fpcur', e_list' = List.fold_left (fun (fpmax,l) e ->
                        let e', fpmax' = alloc_expr env fpcur e in (max fpmax fpmax', e'::l)) (fpcur,[]) e_list in PDo (e_list'), fpcur' 

	| TLet (x_i_e_i_list, e,t) -> let dec = ref 0 in
                                let fpcur', e_list' = List.fold_left (fun (fpmax,l) e ->
                                                                let e', fpmax' = alloc_expr env fpcur e in (max fpmax fpmax', e'::l)) (fpcur,[]) e_list
                        PLet (x_i_e_i_list, e, t), fpcur (*TODO*)
	| TCase (e, p_i_e_i_list,t) -> PCase (e, p_i_e_i_list, t), fpcur (*TODO*)

let alloc_stmt = function
        | TTypeDeclaration (Name(name,start_p,end_p),
                        vars,instances,tau_list,TDefinition(_,p_list, e)) -> nop (*TODO*)
        | TData (ident, vars, constructors) -> nop (*TODO*)
        | TClass (ident, vars, decls) -> nop (*TODO*)
        | TInstance (tau_list, decls) -> nop (*TODO*)

let alloc = List.map alloc_stmt

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
