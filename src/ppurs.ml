(* Programme principal *)

open Format
open Lexing
open Preprocessing
open Typing
open Production

let usage = "usage: ppurs [options] file.purs"

let parse_only = ref false
let type_only = ref false

let set_file f s = f := s
let ofile = ref "" (* output file name *)

let spec =
	[
		"--parse-only", Arg.Set parse_only, "  stop after parsing";
		"--type-only", Arg.Set type_only, "  stop after typing";
    "-o", Arg.String (set_file ofile), "  set output file name";
	]

let file =
	let file = ref None in
	let set_file s =
		if not (Filename.check_suffix s ".purs") then
			raise (Arg.Bad "no .purs extension");
		file := Some s
	in
	Arg.parse spec set_file usage;
	match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let report (b,e) =
	let l = b.pos_lnum in
	let fc = b.pos_cnum - b.pos_bol + 1 in
	let lc = e.pos_cnum - b.pos_bol + 1 in
	eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
	let c = open_in file in
	let lb = Lexing.from_channel c in
	try
		(* Parsing *)
		let f = Parser.file Lexer.next_token lb in
		close_in c;
		if !parse_only then exit 0;

		(* Typing *)
    let t = Typer.type_file f in
		if !type_only then exit 0;

    (* Production *)
    if !ofile = "" then ofile := Filename.chop_suffix file ".purs" ^ ".s";
    let TFile p,_ = t in
    Production.Compile_program.compile_program p !ofile
	with
		| Lexer.Illegal_character s ->
			report (lexeme_start_p lb, lexeme_end_p lb);
			eprintf "Unable to parse module:\nUnexpected character: %c@." s;
			exit 1
		| Lexer.Illegal_character_in_gap '\n' ->
			report (lexeme_start_p lb, lexeme_end_p lb);
			eprintf "Unable to parse module:\nUnexpected line feed in string literal.";
			exit 1
		| Lexer.Illegal_character_in_gap s ->
			report (lexeme_start_p lb, lexeme_end_p lb);
			eprintf "Unable to parse module:\nUnexpected character %c@ in gap" s;
			exit 1
		| Lexer.Undetermined_string ->
			report (lexeme_start_p lb, lexeme_end_p lb);
			print_string "Unable to parse module:\nUndetermined string";
		| Lexer.Unterminated_comment ->
			report (lexeme_start_p lb, lexeme_end_p lb);
			print_string "Unable to parse module:\nBadly terminated comment";
		| Lexer.Bad_indentation ->
			report (lexeme_start_p lb, lexeme_end_p lb);
			print_string "Unable to parse module:\nBad identation";
			exit 1
		| Parser.Error ->
			report (lexeme_start_p lb, lexeme_end_p lb);
			eprintf "syntax error@.";
			exit 1
    | Tast.Error (start_e, end_e, msg_e)->
      report (start_e, end_e);
      print_string msg_e;
      exit 1
		| e ->
			eprintf "Anomaly: %s\n@." (Printexc.to_string e);
			exit 2

