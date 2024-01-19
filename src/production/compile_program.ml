open Past
open Alloc
open Compile_expr

let env = int Smap.t

let rec compile_stmt (code, data) statement = 
  match statement with
  | PTypeDeclaration (_, _, _, _, def) -> 
    let PDefinition (label, _, expr) = def in 
    compile_expr env (code, data) expr
  | PData _ -> () (* TODO *)
  | PClass _ -> () 
  | PInstance _ -> () (* TODO *)

let compile_program p ofile =
    let PFile decl_list = alloc env p in
    let p = List.fold_left compile_stmt (nop, nop) decl_list in
    let f = open_out ofile in
    let fmt = formatter_of_out_channel f in
    X86_64.print_program fmt p;
    fprintf fmt "@?";
    close_out f
