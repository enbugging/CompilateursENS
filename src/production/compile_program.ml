open Past
open Alloc
open Compile_expr
open Utils
open Format
open X86_64

let env = Smap.empty

let label_counter = ref 0
let label_table = ref StringSet.empty


let rec compile_stmt (code, data) statement = 
	match statement with
	| PTypeDeclaration (_, _, _, list_of_types_of_args, def) -> 
		let PDefinition (ident, _, expr) = def in 
		if ident = "main" then 
			compile_expr env label_counter label_table (code ++ globl "main" ++ label "main", data) expr
		else let new_label = ident in
			let (code, data) = compile_expr env label_counter label_table (
        code ++ 
        label new_label ++ 
        pushq !%rbp ++ 
        movq !%rsp !%rbp (* TODO: subq rsp to make room for local variable*)
        , data) expr
      in (
        code ++ 
        popq rax ++ 
        movq !%rbp !%rsp ++
        popq rbp ++ 
        ret, data)
	| PData _ -> (nop, nop) (* TODO *)
	| PClass _ -> (nop, nop) 
	| PInstance _ -> (nop, nop) (* TODO *)

let compile_program p ofile =
	let decl_list = alloc p in
	let (code,data) = log_code env (nop,nop) label_counter label_table in
	let (code,data) = show_int_code env (code,data) label_counter label_table in
	let (code,data) = show_bool_code env (code,data) label_counter label_table in
  let (code,data) = not_code env (code,data) label_counter label_table in 
  let (code,data) = mod_code env (code,data) label_counter label_table in
	let (code,data) = List.fold_left compile_stmt (code, data) decl_list in
	let p = {text=code; data=data} in
	let f = open_out ofile in
	let fmt = formatter_of_out_channel f in
	X86_64.print_program fmt p;
	fprintf fmt "@?";
	close_out f
