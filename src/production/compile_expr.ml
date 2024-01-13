include X86_64
include Utils
open Past 
open Typing.Tast

(* Reminder of Past expressions :
| PConstant of p_const*typ
| PVariable of int (*décalage par rapport à bp*)
| PTypedExpression of p_expr * typ
| PBinaryOperation of p_expr * binaryOperation * p_expr * typ
| PConditional of p_expr * p_expr * p_expr * typ
| PExplicitConstructor of int * p_expr list * typ
| PFunctionCall of ident * p_expr list * typ
| PDo of p_expr list
| PLet of (int * p_expr) list * p_expr * typ
| PCase of p_expr * (pattern * p_expr) list * typ*)

(* exception *)
exception Bad_type of string * typ
exception Todo of string

(* type *)
let find_type = function
    | PConstant (_,t) -> t
    | PVariable (_,t) -> t
    | PTypedExpression (_,t) -> t
    | PBinaryOperation (_,_,_,t) -> t
    | PConditional (_,_,_,t) -> t
    | PExplicitConstructor (_,_,t) -> t
    | PFunctionCall (_,_,t) -> t
    | PLet (_,e,_) -> find_type e
    | PCase (_,_,t) -> t
    | PDo _ -> Tunit

let rec compile_constant = function
    | PConstant (Boolean b,_) -> 
        pushq (imm (if b then 1 else 0)) 
    | PConstant (Integer i,_) -> 
        pushq (imm i)
    | PConstant (String s,_) -> 
        pushq (lab s)
    | PVariable x -> 
        (* Not sure if this is right *)
        movq (ind ~ofs:x rbp) !%rax ++ 
        pushq !%rax
    | PTypedExpression (e,t) -> 
        compile_expr e
    | _ -> raise (Bad_type ("compile_constant", find_type e))

and compile_binop = function
    | PBinaryOperation (e1, (Plus | Minus | Times | Divide) as binop, e2, t) -> 
        compile_expr e1 ++ 
        compile_expr e2 ++
        popq rbx ++ popq rax ++
        (match binop with
        | Plus -> addq !%rbx !%rax
        | Minus -> subq !%rbx !%rax
        | Times -> imulq !%rbx !%rax
        | Divide -> cqto ++ idivq !%rbx ) ++ 
        pushq!%rax
    | PBinaryOperation(e1, Modulo, e2,t) -> 
        compile_expr e1 ++
        compile_expr e2 ++
        popq rbx ++ popq rax ++
        cqto ++ idivq !%rbx ++
        push !%rdx
    (* Comparison operations *)
    | PBinaryOperation (e1, (Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual) as binop, e2, t) ->
        compile_expr e1 ++
        compile_expr e2 ++
        popq rbx ++ popq rax ++
        match find_type e1 with
        | Tint | Tbool -> 
            cmpq !%rbx !%rax ++
            (match binop with
            | Equal -> sete !%al
            | NotEqual -> setne !%al
            | Less -> setl !%al
            | LessEqual -> setle !%al
            | Greater -> setg !%al
            | GreaterEqual -> setge !%al) ++
            movzbq !%al !%rax ++
            pushq !%rax
        | Tstring ->
            movq !%rax !%rdi ++ 
            movq !%rbx !%rsi ++
            call "strcmp" ++
            (match binop with
            | Equal -> sete !%al
            | NotEqual -> setne !%al
            | Less -> setl !%al
            | LessEqual -> setle !%al
            | Greater -> setg !%al
            | GreaterEqual -> setge !%al) ++
            movzbq !%al !%rax ++
            pushq !%rax
        | _  as t -> raise (Bad_type ("Comparison operations", t))
    | _ as e -> raise (Bad_type ("Binary operation", find_type e))

and compile_function_call = function 
    | PFunctionCall ("log",[e],t) ->
        match t with 
        | Tstring -> 
            compile_expr e ++
            call "log" ++ 
            pushq !%rax ++
            ret 
        | _ as t -> raise (Bad_type ("Function call : log", t))
    | PFunctionCall ("Show",[e],t) -> 
        match t with
        | Tbool ->
            compile_expr e ++
            call "show_bool" ++
            pushq !%rax ++
            ret
        | Tint ->
            compile_expr e ++
            call "show_int" ++
            pushq !%rax ++
            ret
        | Tstring ->
            compile_expr e
        | _ as t -> raise (Bad_type ("Function call : Show", t))
    | PFunctionCall (f, args, t) ->
        List.fold_right (fun e code -> compile_expr e ++ code) args nop ++
        call f ++
        pushq !%rax
    | _ as e -> raise (Bad_type ("Function call", find_type e))

and compile_conditional = function
    | PConditional (e1, e2, e3, t) ->
        let label_else = unique_label ~isUnique:true "else" in
        let label_end = unique_label ~isUnique:true "end" in
        compile_expr e1 ++ 
        popq rax ++
        cmpq (imm 0) !%rax ++
        je label_else ++
        compile_expr e2 ++
        jmp label_end ++
        label label_else ++
        compile_expr e3 ++
        label label_end
    | _ as e -> raise (Bad_type ("Conditional", find_type e))

and compile_expr = function
    | PConstant _ | PVariable _ | PTypedExpression _ as e -> compile_constant e
    | PBinaryOperation _ as e -> compile_binop e  
    | PFunctionCall _ as e -> compile_function_call e    
    | PConditional _ as e -> compile_conditional e
    | PDo l -> List.fold_right (fun e code -> compile_expr e ++ code) l nop
    | PLet (l,e,t) -> raise (Todo "Let")
    | PCase (e,l,t) -> raise (Todo "Case")
    | PExplicitConstructor (i,el,t) -> raise (Todo "Explicit constructor")
    | _ -> nop