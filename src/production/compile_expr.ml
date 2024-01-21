include X86_64
include Utils
open Typing.PrettyPrinterBeta
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
    | PConstant (Unit, _) -> (code,data) (*TODO*)
    | PVariable (x, _) -> 
        let code = code ++ 
            (* Not sure if this is right *)
            movq (ind ~ofs:x rbp) !%rax ++ 
            pushq !%rax
        in (code, data)
    | PTypedExpression (e,t) -> 
        compile_constant env label_counter label_table (code, data) e
    | PConstr (s,_) -> (*TODO not right but didn't know what to do*)
        let label_s = unique_label ~isUnique:true "string" label_counter label_table in
        let data = data ++ 
            label label_s ++ 
            string s
        in 
        let code = code ++ 
            pushq (ilab label_s) 
        in (code, data)

    | e -> raise (Bad_type ("compile_constant", find_type e))

and compile_binop env label_counter label_table (code, data) = function
    | PBinaryOperation (e1, binop, e2, t) when (List.mem binop [Plus ; Minus ; Times])-> 
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
    | PBinaryOperation(e1, Divide, e2,t) -> 
        let positive_dividend_label = unique_label ~isUnique:true "positive_dividend" label_counter label_table in
        let positive_divisor_label = unique_label ~isUnique:true "positive_divisor" label_counter label_table in
        let (code, data) = compile_expr env label_counter label_table (code, data) e1 in 
        let (code, data) = compile_expr env label_counter label_table (code, data) e2 in
        let code = code ++
            popq rbx ++ 
            popq rax ++
            movq !%rax !%rdi ++ (* Dividend *)
            cqto ++ 
            idivq !%rbx ++

            cmpq (imm 0) !%rdi ++ (* Check if dividend is positive *)
            jge positive_dividend_label ++ (* If it is negative, check if divisor is positive *)
            cmpq (imm 0) !%rbx ++
            jge positive_divisor_label ++ (* If it is negative, we need to add 1 to the quotient *)
            addq (imm 1) !%rax ++
            jmp positive_dividend_label ++
            
            label positive_divisor_label ++ (* If the divisor is positive, we need to substract 1 to the quotient *)
            subq (imm 1) !%rax ++
            jmp positive_dividend_label ++

            label positive_dividend_label ++
            pushq !%rax
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
            | t -> (*print_type t;*) raise (Bad_type ("Comparison operations", t))
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
        | Tbool | Tconstr("Boolean",[])->
            let (code, data) = compile_expr env label_counter label_table (code, data) e in
            let code = code ++
                call "show_bool" ++
                pushq !%rax
            in (code, data)
        | Tint | Tconstr("Int",[])->
            let (code, data) = compile_expr env label_counter label_table (code, data) e in
            let code = code ++
                call "show_int" ++ 
                pushq !%rax
            in (code, data)
        | Tstring | Tconstr("String",[])->
            compile_expr env label_counter label_table (code, data) e
        | t -> (*print_type t;*) raise (Bad_type ("Function call : Show", t))
        end
    | PFunctionCall (f, _, args, t) ->
        let code = code ++ 
            movq !%rsp !%r14 in (* Save rsp to r14, which we guarantee to not be modified by the function call *)
        let (code, data) = List.fold_left (fun (code, data) e -> compile_expr env label_counter label_table (code, data) e) (code, data) args in
        let code = code ++
            pushq !%r14 ++ (* Save r14 to stack *)
            call f ++
            popq rsp ++ (* Restore rsp *)
            pushq !%rax 
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
    | PConstr _ | PConstant _ | PVariable _ | PTypedExpression _ as e -> compile_constant env label_counter label_table (code, data) e
    | PBinaryOperation _ as e -> compile_binop env label_counter label_table (code, data) e  
    | PFunctionCall _ as e -> compile_function_call env label_counter label_table (code, data) e    
    | PConditional _ as e -> compile_conditional env label_counter label_table (code, data) e
    | PDo l -> List.fold_right (fun e (code, data) -> compile_expr env label_counter label_table (code, data) e) l (code, data)
    | PLet (l,e,t) -> (nop, nop) (*TODO*)
    | PCase (e,l,t) -> (nop, nop) (*Todo "Case"*)
    | PExplicitConstructor (i,el,t) -> (nop, nop) (*Todo "Explicit constructor"*)
