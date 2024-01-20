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
let rec find_type = function
    | PConstant (_,t) -> t
    | PVariable (_, t) -> t
    | PTypedExpression (_,t) -> t
    | PBinaryOperation (_,_,_,t) -> t
    | PConditional (_,_,_,t) -> t
    | PExplicitConstructor (_,_,t) -> t
    | PFunctionCall (_,_,_,t) -> t
    | PLet (_,e,_) -> find_type e
    | PCase (_,_,t) -> t
    | PDo _ -> Tunit

let rec compile_constant env label_counter label_table (code, data) = function
    | PConstant (Boolean b,_) -> 
        let code = code ++
            pushq (imm (if b then 1 else 0)) 
        in (code, data)
    | PConstant (Integer i,_) -> 
        let code = code ++ 
            pushq (imm i)
        in (code, data)
    | PConstant (String s,_) -> 
        let label_s = unique_label ~isUnique:true "string" label_counter label_table in
        let data = data ++ 
            label label_s ++ 
            string s
        in 
        let code = code ++ 
            pushq (ilab label_s) 
        in (code, data)
    | PVariable (x, _) -> 
        let code = code ++ 
            (* Not sure if this is right *)
            movq (ind ~ofs:x rbp) !%rax ++ 
            pushq !%rax
        in (code, data)
    | PTypedExpression (e,t) -> 
        compile_constant env label_counter label_table (code, data) e
    | e -> raise (Bad_type ("compile_constant", find_type e))

and compile_binop env label_counter label_table (code, data) = function
        | PBinaryOperation (e1, binop, e2, t) when (List.mem binop [Plus ; Minus ; Times ; Divide])-> 
        let (code, data) = compile_expr env label_counter label_table (code, data) e1 in 
        let (code, data) = compile_expr env label_counter label_table (code, data) e2 in
        let code = code ++
            popq rbx ++ popq rax ++
            (match binop with
            | Plus -> addq !%rbx !%rax
            | Minus -> subq !%rbx !%rax
            | Times -> imulq !%rbx !%rax
            | Divide -> cqto ++ idivq !%rbx ) ++ 
            pushq!%rax
        in (code, data)
    | PBinaryOperation(e1, Modulo, e2,t) -> 
        let (code, data) = compile_expr env label_counter label_table (code, data) e1 in 
        let (code, data) = compile_expr env label_counter label_table (code, data) e2 in
        let code = code ++
            popq rbx ++ popq rax ++
            cqto ++ idivq !%rbx ++
            pushq !%rdx
        in (code, data)
    (* Comparison operations *)
    | PBinaryOperation (e1, binop, e2, t) when (List.mem binop [Equal ; NotEqual ; LessThan ; LessThanOrEqual ; GreaterThan ; GreaterThanOrEqual])->
        let (code, data) = compile_expr env label_counter label_table (code, data) e1 in 
        let (code, data) = compile_expr env label_counter label_table (code, data) e2 in
        let code = code ++
            popq rbx ++ popq rax ++
            match find_type e1 with
            | Tint | Tbool -> 
                cmpq !%rbx !%rax ++
                (match binop with
                | Equal -> sete !%al
                | NotEqual -> setne !%al
                | LessThan-> setl !%al
                | LessThanOrEqual -> setle !%al
                | GreaterThan -> setg !%al
                | GreaterThanOrEqual -> setge !%al) ++
                movzbq !%al rax ++
                pushq !%rax
            | Tstring ->
                movq !%rax !%rdi ++ 
                movq !%rbx !%rsi ++
                call "strcmp" ++
                (match binop with
                | Equal -> sete !%al
                | NotEqual -> setne !%al
                | LessThan-> setl !%al
                | LessThanOrEqual -> setle !%al
                | GreaterThan -> setg !%al
                | GreaterThanOrEqual -> setge !%al) ++
                movzbq !%al rax ++
                pushq !%rax
            | t -> raise (Bad_type ("Comparison operations", t))
        in (code, data)
    | e -> raise (Bad_type ("Binary operation", find_type e))

and compile_function_call env label_counter label_table (code, data) = function 
    | PFunctionCall ("log",_,[e],t) ->
        let (code, data) = compile_expr env label_counter label_table  (code, data) e in 
        let code = code ++ 
            call "log" 
        in (code, data)
    | PFunctionCall ("show",_,[e],t) -> 
        begin match find_type e with
        | Tbool ->
            let (code, data) = compile_expr env label_counter label_table (code, data) e in
            let code = code ++
                call "show_bool" ++
                pushq !%rax
            in (code, data)
        | Tint ->
            let (code, data) = compile_expr env label_counter label_table (code, data) e in
            let code = code ++
                call "show_int" ++ 
                pushq !%rax
            in (code, data)
        | Tstring ->
            compile_expr env label_counter label_table (code, data) e
        | t -> raise (Bad_type ("Function call : Show", t))
        end
    | PFunctionCall (f, _, args, t) ->
        let (code, data) = List.fold_left (fun (code, data) e -> compile_expr env label_counter label_table (code, data) e) (code, data) args in
        let code = code ++
            call f ++
            pushq !%rax
    (* TODO: remove arguments from stacks *)
        in (code, data)
    | e -> raise (Bad_type ("Function call", find_type e))
  
and compile_conditional env label_counter label_table (code, data) e = 
    match e with
    | PConditional (e1, e2, e3, t) ->
        let label_else = unique_label ~isUnique:true "else" label_counter label_table in
        let label_end = unique_label ~isUnique:true "end" label_counter label_table in
        let (code, data) = compile_expr env label_counter label_table (code, data) e1 in
        let code = code ++
            popq rax ++
            cmpq (imm 0) !%rax ++
            je label_else in 
        let (code, data) = compile_expr env label_counter label_table (code, data) e2 in
        let code = code ++
            jmp label_end ++
            label label_else in
        let (code, data) = compile_expr env label_counter label_table (code, data) e3 in
        let code = code ++
            label label_end
        in (code, data)
    | e -> raise (Bad_type ("Conditional", find_type e))

and compile_expr env label_counter label_table (code, data) = function
    | PConstant _ | PVariable _ | PTypedExpression _ as e -> compile_constant env label_counter label_table (code, data) e
    | PBinaryOperation _ as e -> compile_binop env label_counter label_table (code, data) e  
    | PFunctionCall _ as e -> compile_function_call env label_counter label_table (code, data) e    
    | PConditional _ as e -> compile_conditional env label_counter label_table (code, data) e
    | PDo l -> List.fold_left (fun (code, data) e -> compile_expr env label_counter label_table (code, data) e) (code, data) l
    | PLet (l,e,t) -> raise (Todo "Let")
    | PCase (e,l,t) -> raise (Todo "Case")
    | PExplicitConstructor (i,el,t) -> raise (Todo "Explicit constructor")
