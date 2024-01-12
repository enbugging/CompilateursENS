(* Labels *)

let label_counter = ref 0
let label_table = Hashtbl.create 100

(* Modify a label if we need it to be unique, by set isUnique = true *)
let unique_label ?(isUnique : false) label = 
    if not isUnique then 
        let _ = Hashtbl.add label_table label in
        in label
    else 
        begin
            while Hashtbl.mem label_table label do
                label_counter := !label_counter + 1;
            done;
            let new_label = label ^ (string_of_int !label_counter) in 
            Hashtbl.add label_table new_label;
            new_label
        end

(* Log function: print a string to the output.
    Input: pointer p to a string
*)
let log_code env (text, data) = 
	let string_format = unique_label "string_format" in 
	let data = data ++ 
		label string_format ++ 
		string "%s\n"
	in
	let text = text++
		movq (ilab ~ofs:8 rdi) !%rsi ++ (* Move the pointer to rsi *)
		movq (ilabl string_format) !%rdi ++ (* Move the address of the string to rdi *)
		xorq !%rax !%rax ++ (* Set rax to 0 *)
		call "printf" ++ (* Call printf *)
		ret
	in (text, data) 

(* Show_int: taking an int and return a pointer to a string representing the int.
    In our cases, all integers are within long long range, so there can be at most 20 digits.
    Input: one value *)
let show_int_code env (text, data) = 
    let show_int_format = unique_label "show_int_format" in
    let data = data++
        label show_int_format ++ (* Label for the string *)
        string "%lld" (* The string *)
        (* TODO : align! *)
    in 
    let text = text++
        movq (ilab ~ofs:8 rsi) !%r15 ++ (* Move value to r15 *)
        movq (imm 20) !%rdi ++ (* Move 20 to rdi *)
        call "malloc" ++ (* Allocate 20 bytes for the string *)
        movq !%r15 !%rdi ++ (* Move value back to rdi *)

        movq (ilabl show_int_format) !%rsi ++ (* Move the address of the string to rsi *)
        xorq !%rax !%rax ++ (* Set rax to 0 *)
        call "sprintf"
        ret
    in (text, data)
        

(* Show_bool: taking a bool and return a pointer to a string representing the bool 
   Input: value in rdi *)
let show_bool_code env (text, data) = 
    let show_bool = unique_label "show_bool" in
    let true_label = unique_label "true" in
    let false_label = unique_label "false" in 
    let show_true = unique_label "show_true" in 
    (* Add two labels in data, each containing a string, either "true" or "false".
        i.e. 
        .data
    true:
        .string "true"
    false:
        .string "false" *)
    let data = data ++
    label true_label ++ 
    sring "true" ++ 
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
    let code = code ++ 
    label show_bool ++ 
	testq !%rax !%rax ++ (* Test if rax is true *)
    je (ilabl show_true)
    movq (ilab false_label) !%rax ++ (* Load the address of false_label if rax is false *)
    ret ++

    label show_true ++ (* Label to jump to when rax is true*)
    movq (ilab true_label) !%rax ++ (* Load the address of true_label if rax is true *)
    ret
    in (code, data)
