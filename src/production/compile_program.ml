open Past
open Alloc
open Compile_expr
open Utils
open Format

let env = Smap.empty

let rec compile_stmt (code, data) statement = 
  match statement with
  | PTypeDeclaration (_, _, _, list_of_types_of_args, def) -> 
    let hash_of_types_of_args = hash_of_list_of_types list_of_types_of_args in 
    let PDefinition (label, _, expr) = def in 
    let new_label = label ^ hash_of_types_of_args in
    compile_expr env (code, data) expr
  | PData _ -> (nop, nop) (* TODO *)
  | PClass _ -> (nop, nop) 
  | PInstance _ -> (nop, nop) (* TODO *)

let compile_program p ofile =
    let decl_list = alloc p in
    let (code,data) = log_code env (nop,nop) in
    let (code,data) = show_int_code env (code,data) in
    let (code,data) = show_bool_code env (code,data) in
    let (code, data) = List.fold_left compile_stmt (code, data) decl_list in
    let p = {text=code; data=data} in
    let f = open_out ofile in
    let fmt = formatter_of_out_channel f in
    X86_64.print_program fmt p;
    fprintf fmt "@?";
    close_out f
