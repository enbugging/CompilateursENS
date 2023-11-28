
(* Programme principal *)

open Format
open Lexing
open Parser

let usage = "usage: analyseur_syntaxique [options] file.purs"

let parse_only = ref false

let spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
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

let test lb =
        while true do
                let f = Lexer.next_token lb in
                print_endline "Le lexem a été lu";
                let _ = print_string (match f with
                | EQUAL -> "EQUAL"
                | CASE -> "CASE"
                | LIDENT s -> s
                | UIDENT s -> s
                | BRANCHING -> "->"
                | OF -> "OF"
                | EOF -> "\n"
                | MODULE -> "MODULE"
                | IMPORT -> "IMPORT"
                | WHERE -> "WHERE"
        ) in print_char ' '
        done

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
          print_endline "Commence";
          test lb;
          print_endline "fin";
          close_in c;
          if !parse_only then exit 0;
  with
    | _ -> print_string "Echec"; exit 1
