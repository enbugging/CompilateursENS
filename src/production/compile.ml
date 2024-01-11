open Format
open X86_64
open Preprocessing.Ast

(* phase 1 : allocation des variables *)

exception VarUndef of string

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

module Smap = Map.Make(String)

type local_env = ident Smap.t

let rec alloc_expr (env: local_env) (fpcur: int) = function
        | Constant (c) -> Constant c, fpcur
	| Variable (x) -> begin match Smap.find_opt x with
                                | Some(dec) -> Variable x, fpcur
                                | None -> raise (VarUndef x)
                                end

	| TypedExpression (e, t) -> alloc_expr env fpcur e

	| BinaryOperation (e1, binop, e2) -> 
                        let e1, fpmax1 = alloc_expr env fpcur e1 in
                        let e2, fpmax2 = alloc_expr env fpcur e2 in
                        TBinaryOperation (e1, binop, e2), max fpmax1 fpmax2

	| Conditional (e1, e2, e3) ->
                        let e1, fpmax1 = alloc_expr env fpcur e1 in
                        let e2, fpmax2 = alloc_expr env fpcur e2 in
                        let e3, fpmax3 = alloc_expr env fpcur e3 in
                        Conditional (e1,e2,e3), max (max fpmax1 fpmax2) fpmax3

        | ExplicitConstructor (x, e_list) -> begin match Smap.find_opt x with
                                                | Some(dec) -> List.fold_left (fun (fpmax,l) ->
                                                                let e, fpmax' = alloc_expr env fpcur in (max fpmax fpmax', e::l)) (fpcur,[]) e_list
                                                | None -> raise (VarUndef x)
                                                end
	| FunctionCall (f, e_list) -> FunctionCall (f, e_list), fpcur (*TODO*)
	| Do (e_list) -> Do (e_list), fpcur (*TODO*)
	| Let (x_i_e_i_list, e) -> Let (x_i_e_i_list, e), fpcur (*TODO*)
	| Case (e, p_i_e_i_list) -> Case (e, p_i_e_i_list) fpcur (*TODO*)

let alloc_stmt = function
        | TTypeDeclaration (Name(name,start_p,end_p),
                        vars,instances,tau_list,Definition(_,p_list, e)) -> nop
        
        (*
  | PSet (x, e) ->
                  let e', fpmax = alloc_expr Smap.empty 0 e in
                  Set (x, e', fpmax)

  | PFun (f, l, e) ->
                  let dec = ref 0 in
                  let e',fpmax = alloc_expr (List.fold_left (fun env x -> dec:= !dec +8; Smap.add x !dec env) Smap.empty l ) 0 e in
                  Fun(f, e', fpmax+8*(List.length l + 1))

  | PPrint e ->
    let e, fpmax = alloc_expr Smap.empty 0 e in
    Print (e, fpmax)
        *)

let alloc = List.map alloc_stmt

(******************************************************************************)
(* phase 2 : production de code *)

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

let rec compile_expr = function
        | Constant (Boolean b) -> pushq (imm (if b then 1 else 0))
        | Constant (Integer i) -> pushq (imm i)
        | Constant (String s) -> nop (*TODO*)
	| Variable x -> pushq (ind ~ofs:x rbp)
	| TypedExpression (e,t) -> nop (*TODO*)
	| BinaryOperation (e1, (Plus | Minus | Times | Divide) as binop, e2) -> 
                        compile_expr e1 ++ 
                        compile_expr e2 ++
                        popq rbx ++ popq rax ++
                        (match binop with
                        | Plus -> addq !%rbx !%rax
                        | Minus -> subq !%rbx !%rax
                        | Times -> imulq !%rbx !%rax
                        | Divide -> cqto ++ idivq !%rbx ) ++ !%rax

        | BinaryOperation(e1, Modulo, e2) -> 
                        compile_expr e1 ++
                        compile_expr e2 ++
                        popq rbx ++ popq rax ++
                        cqto ++ idivq !%rbx
                        ++ !%rdx

        | FunctionCall ("log",[e]) ->
                                        
        | FunctionCall ("Show",[e]) -> nop
        | _ -> nop
                        

let compile_stmt (codefun, codemain) = function
        (*
  | Set (x, e, fpmax) ->
    let code =
      pushn fpmax ++
      compile_expr e ++
      popq rax ++ movq !%rax (lab x) ++
      popn fpmax
    in
    codefun, codemain ++ code

  | Fun (f, e, fpmax) ->
    failwith "à compléter"

  | Print (e, fpmax) ->
    let code =
      pushn fpmax ++
      compile_expr e ++
      popq rdi ++
      popn fpmax ++
      call "print_int"
    in
    codefun, codemain ++ code
    *)

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
