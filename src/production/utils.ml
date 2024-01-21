open Typing.Tast
open Preprocessing.Ast
open X86_64

module StringSet = Set.Make(String)

(* Modify a label if we need it to be unique, by set isUnique = true *)
let unique_label ?(isUnique = false) label label_counter label_table = 
	if not isUnique then 
		let _ = 
		  label_table := StringSet.add label !label_table
		in label
	else 
		begin
			while StringSet.mem (label ^ (string_of_int !label_counter)) !label_table do
				label_counter := !label_counter + 1;
			done;
			let new_label = label ^ (string_of_int !label_counter) in 
			let _ = 
			  label_table := StringSet.add new_label !label_table
			in new_label
		end

(* Log function: print a string to the output.
	Input: pointer p to a string
*)
let log_code env (text, data) label_counter label_table = 
	let log = unique_label "log" label_counter label_table in
	let string_format = unique_label "string_format" label_counter label_table in 
	let data = data ++ 
		label string_format ++ 
		string "%s\n"
	in
	let text = text ++
		label log ++
		popq rax ++ (* Pop the return address to the string to rax *)
		popq rsi ++ (* Move the pointer to the string to rsi *)
		pushq !%rax ++ (* Push the return address back to the stack *)
		movq (ilab string_format) !%rdi ++ (* Move the address of the string to rdi *)
		xorq !%rax !%rax ++ (* Set rax to 0 *)
		call "printf" ++ (* Call printf *)
		ret
	in (text, data) 

(* Show_int: taking an int and return a pointer to a string representing the int.
	In our cases, all integers are within long long range, so there can be at most 20 digits.
	Input: one value *)
let show_int_code env (text, data) label_counter label_table = 
	let show_int = unique_label "show_int" label_counter label_table in 
	let show_int_format = unique_label "show_int_format" label_counter label_table in
	let data = data ++
		label show_int_format ++ (* Label for the string *)
		string "%lld" (* The string *)
		(* TODO : align *)
	in 
	let text = text ++
		label show_int ++ 
		popq rax ++ (* Pop the return address to the int to rax *)
		popq r15 ++ (* Move the int to r15 *)
		pushq !%rax ++ (* Push the return address back to the stack *)

		(* Allocate 20 bytes for the string *)
		movq (imm 20) !%rdi ++ (* Move 20 to rdi *)
		call "malloc" ++ (* Allocate 20 bytes for the string *)

		movq !%r15 !%rdx ++ (* Move value back to rdi *)
		movq (ilab show_int_format) !%rsi ++ (* Move the address of the format string to rsi *)
		movq !%rax !%rdi ++ (* Move the address of the buffer (i.e., the result string) to rdi *)
		movq !%rax !%r15 ++ (* Move the address of the buffer to r15 *)
		xorq !%rax !%rax ++ (* Set rax to 0 *)
		call "sprintf" ++ 
		movq !%r15 !%rax ++ (* Move the address of the buffer to rax *)
		ret
	in (text, data)
		

(* Show_bool: taking a bool and return a pointer to a string representing the bool 
   Input: value in rdi *)
let show_bool_code env (text, data) label_counter label_table = 
	let show_bool = unique_label "show_bool" label_counter label_table in
	let true_label = unique_label "true" label_counter label_table in
	let false_label = unique_label "false" label_counter label_table in 
	let show_true = unique_label "show_true" label_counter label_table in 
	(* Add two labels in data, each containing a string, either "true" or "false".
		i.e. 
		.data
	true:
		.string "true"
	false:
		.string "false" *)
	let data = data ++
		label true_label ++ 
		string "true" ++ 
		label false_label ++
		string "false"
	in 
	(* Add code to check if the bool is true or false, and jump to the corresponding label 
		.text
	show_boolL
		testq rax, rax
		je [show_true]
		mov $(false), rax
		ret
	
	show_true:
		mov $(true), rax
		ret*)
	let text = text ++ 
		label show_bool ++ 
		popq rax ++ (* Pop the return address to the bool to rax *)
		popq rdi ++ (* Move the bool to rdi *)
		pushq !%rax ++ (* Push the return address back to the stack *)

		testq !%rdi !%rdi ++ (* Test if rax is true *)
		jne show_true ++ 
		movq (ilab false_label) !%rax ++ (* Push the address of false_label if rax is false *)
		ret ++

		label show_true ++ (* Label to jump to when rax is true*)
		movq (ilab true_label) !%rax ++ (* Push the address of true_label if rax is true *)
		ret
	in (text, data)

let not_code env (text, data) label_counter label_table = 
  let not_label = unique_label "not" label_counter label_table in
  let text = text ++
    label not_label ++
    popq r15 ++
    popq rax ++
    pushq !%r15 ++
    xorq (imm 1) !%rax ++
    ret
  in (text, data)

let mod_code env (text, data) label_counter label_table = 
  let mod_label = unique_label "mod" label_counter label_table in
  let mod_by_0_label = unique_label "mod_by_0" label_counter label_table in
  let mod_end_label = unique_label "mod_end" label_counter label_table in
  let add_to_res_label = unique_label "add_to_res" label_counter label_table in
  let text = text ++
    label mod_label ++
    
    (* Get the dividend and divisor *)
    popq r15 ++ (* Pop the return address to the divisor to r15 *)
    popq rax ++ (* Pop the return address to the dividend to rax *)
    popq rbx ++ (* Move the divisor to rbx *)
    pushq !%r15 ++ (* Push the return address back to the stack *)

    (* Check if the divisor is 0 *)
    cmpq (imm 0) !%rbx ++ (* Compare the divisor with 0 *)
    je mod_by_0_label ++ (* If divisor is 0, return 0 *)

    (* Use idivq to get remainder result *)
    cqto ++ (* Sign extend rax to rdx:rax *)
    idivq !%rbx ++ (* Divide rdx:rax by rbx *)

    (* Now we check the sign of the remainder, and correct if needed. *)
    (* If the result is negative, we need to add the divisor to the result. *)
    cmpq (imm 0) !%rdx ++ (* Compare the remainder with 0 *)
    jge mod_end_label ++ (* If remainder is non-negative, jump to end *)
    cmpq (imm 0) !%rbx ++ (* Compare the divisor with 0 *)
    jge add_to_res_label ++ (* If divisor is non-negative, add to the remaider *)
    subq !%rbx !%rdx ++ (* Subtract the divisor from the remainder *)
    jmp mod_end_label ++ (* Jump to end *)

    (* If the result is positive, we need to subtract the divisor from the result. *)
    label add_to_res_label ++
    addq !%rbx !%rdx ++ (* Add the divisor to the remainder *)
    jmp mod_end_label ++ (* Jump to end *)

    (* End of the function *)
    label mod_end_label ++
    movq !%rdx !%rax ++ (* Move the remainder to rax *)
    ret ++

    label mod_by_0_label ++
    movq (imm 0) !%rax ++ (* Move 0 to rax *)
    ret
  in (text, data)
