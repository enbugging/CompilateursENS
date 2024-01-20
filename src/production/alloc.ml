open Format
open X86_64
open Preprocessing.Ast
open Typing.Tast
open Past

(* phase 1 : allocation des variables *)

exception VarUndef of string

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

let reg_size = 64

module Smap = Map.Make(String)

type local_env = int Smap.t

let rec alloc_expr (env: local_env) (fpcur: int) = function
        | TConstant (c, t) -> PConstant (c,t), fpcur
	| TVariable (x, t) -> begin match Smap.find_opt x env with
                                | Some(dec) -> PVariable (dec, t), fpcur
                                | None -> PConstr (x,t), fpcur
                                end

	| TTypedExpression (e,_, _) -> alloc_expr env fpcur e

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
                                                                let e', fpmax' = alloc_expr env fpcur e in (max fpmax fpmax', e'::l)) (fpcur,[]) e_list in PExplicitConstructor (0, e_list', t), reg_size+fpcur' (*TODO*)

	| TFunctionCall (f, instances, e_list, t) -> let fpcur', e_list' = List.fold_left (fun (fpmax,l) e ->
                                                                let e', fpmax' = alloc_expr env fpcur e in (max fpmax fpmax', e'::l)) (fpcur,[]) e_list in PFunctionCall (f, instances, e_list', t), reg_size*(1 + List.length instances)+fpcur'


	| TDo (e_list) -> let fpcur', e_list' = List.fold_left (fun (fpmax,l) e ->
                        let e', fpmax' = alloc_expr env fpcur e in (max fpmax fpmax', e'::l)) (fpcur,[]) e_list in PDo (e_list'), fpcur' 

	| TLet (x_i_e_i_list, e, t) -> PConstant (Boolean false, Tbool), fpcur (*TODO*)
	| TCase (e, l, t) -> PConstant (Boolean false, Tbool), fpcur (*TODO*)

let alloc_def = function
        | NoDecl -> PNoDecl
        | TDefinition (name,p_list,e) -> let env = List.fold_left (fun acc p -> match p with
                                                                                | PatargIdent x -> Smap.add x ((Smap.cardinal acc)*reg_size) acc) 
                                                        (Smap.add "unit" 0 Smap.empty) p_list in let e',_ = alloc_expr env 0 e in
                                                        PDefinition(name,p_list, e')
let rec alloc_stmt = function
        | TypeDeclaration (Name(name,start_p,end_p),
                        vars,instances,tau_list), definition -> 
                                PTypeDeclaration(name,vars,instances,tau_list,alloc_def definition)
        | TypeDeclaration (Name(name,start_p,end_p), vars,instances,tau_list), NoDecl -> 
                        PTypeDeclaration(name,vars,instances,tau_list,PNoDecl)
        | Data (Name(ident,_,_), vars, constructors), NoDecl -> 
                        PData(ident, vars, constructors) (*TODO*)
        | Class (Name(ident,_,_), vars, decls), NoDecl -> 
                        PClass(ident, vars, List.map alloc_stmt (List.map (fun d -> (d,NoDecl)) decls)) (*TODO*)
        | Instance (tau_list, defs), NoDecl -> 
                        PInstance(tau_list, []) (*TODO*)

let alloc = List.map alloc_stmt

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

