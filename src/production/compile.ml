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
        | TConstant (c, t) -> PConstant (c,t), fpcur
	| TVariable (x, t) -> begin match Smap.find_opt x with
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

        | TExplicitConstructor (x, e_list, t) -> let fpcur', e_list' = List.fold_left (fun (fpmax,l) e ->
                                                                let e', fpmax' = alloc_expr env fpcur e in (max fpmax fpmax', e'::l)) (fpcur,[]) e_list in PExplicitConstructor (x, e_list', t), reg_size+fpcur'

	| TFunctionCall (f, instances, e_list), t -> let fpcur', e_list' = List.fold_left (fun (fpmax,l) e ->
                                                                let e', fpmax' = alloc_expr env fpcur e in (max fpmax fpmax', e'::l)) (fpcur,[]) e_list in PFunctionCall (x, instances, e_list', t), reg_size*(1 + List.length instances)+fpcur'


	| TDo (e_list) -> let fpcur', e_list' = List.fold_left (fun (fpmax,l) e ->
                        let e', fpmax' = alloc_expr env fpcur e in (max fpmax fpmax', e'::l)) (fpcur,[]) e_list in PDo (e_list'), fpcur' 

	| TLet (x_i_e_i_list, e, t) -> PLet (x_i_e_i_list, e, t), fpcur (*TODO*)
	| TCase (e, p_i_e_i_list, t) -> PCase (e, p_i_e_i_list, t), fpcur (*TODO*)

let alloc_stmt = function
        | TypeDeclaration (Name(name,start_p,end_p),
                        vars,instances,tau_list), TDefinition(_,p_list, e) -> nop (*TODO*)
        | Data (ident, vars, constructors), NoDecl -> nop (*TODO*)
        | Class (ident, vars, decls), NoDecl -> nop (*TODO*)
        | Instance (tau_list, decls), NoDecl -> nop (*TODO*)

let alloc = List.map alloc_stmt

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

