(* Lexical analyser for Little-Purescript *)

{
    open Lexing
    open Parser
    open Ast

    exception Illegal_character of char
    exception Undetermined_string
    exception Bad_indentation
    (* Handling of significant indentation *)
    type indentation = 
        | B of int (* block *)
        | M        (* marker *)

    let string_buffer = Buffer.create 1024

    (* Handling of keywords *)
    let keyword_hashtable = Hashtbl.create 32

    let _ = List.iter (fun (s, token) -> Hashtbl.add keyword_hashtable s token)
        ["case", CASE;
        "class", CLASS;
        "data", DATA;
        "do", DO;
        "else", ELSE;
        "false", FALSE;
        "forall", FORALL;
        "if", IF;
        "import", IMPORT;
        "in", IN;
        "instance", INSTANCE;
        "let", LET;
        "module", MODULE;
        "of", OF;
        "then", THEN;
        "true", TRUE;
        "where", WHERE]
}

let digit = ['0'-'9']
let lower = ['a'-'z'] | '_'
let upper = ['A'-'Z']
let other = lower | upper | digit | '\''
let lident = lower other*
let uident = upper (other | '.')*
let ident = digit | lower | upper | other | lident | uident
let integer = '0' | ['1'-'9'] digit*
let space = ' ' | '\t'
let line_comment = "--" [^ '\n']*

rule next_tokens = parse 
    | ('\n'|"\r\n") { new_line lexbuf; next_tokens lexbuf }
    | (space | line_comment)+
                    { next_tokens lexbuf }
    | lident as id  { LIDENT id }
    | uident as id  { UIDENT id }
    | '+'           { PLUS }
    | '-'           { MINUS }
    | '*'           { TIMES }
    | '/'           { DIVIDE }
    | '%'           { MODULO }
    | '='           { EQUAL }
    | "=="          { CMP Equal }
    | "!="          { CMP NotEqual }
    | "<"           { CMP LessThan }
    | "<="          { CMP LessThanOrEqual }
    | ">"           { CMP GreaterThan }
    | ">="          { CMP GreaterThanOrEqual }
    | "&&"          { AND }
    | "||"          { OR }
    | "!"           { NOT }
    | "=>"          { ARROW }
    | "->"          { BRANCHING }
    | '('           { LPAREN }
    | ')'           { RPAREN }
    | '{'           { LCURLY }
    | '}'           { RCURLY }
    | ','           { COMMA }
    | ':'           { COLON }
    | '.'           { DOT }
    | '|'           { VERTICAL_BAR }
    | ';'           { SEMICOLON }
    | integer as s  { CONSTANT (Integer (int_of_string s)) }
    | '"'           { CONSTANT (String (string lexbuf)) }
    | eof           { EOF }
    | _ as c        { raise (Illegal_character c) }

and string = parse 
    | '"'           { let s = Buffer.contents string_buffer in Buffer.reset string_buffer; s }
    | "\\n"         { Buffer.add_char string_buffer '\n'; string lexbuf }
    | "\\\""        { Buffer.add_char string_buffer '"'; string lexbuf }
    | _ as c        { Buffer.add_char string_buffer c; string lexbuf }
    | eof           { raise Undetermined_string }

{   
    let tokens = Queue.create () 
    let is_weak_mode = ref false (* weak indentation mode *)
    let about_weak_mode = ref false

    let stack = ref [] (* indentation stack *)
    let rec close c = 
        match !stack with
        | (B n) :: s when n > c -> stack := s; Queue.add RCURLY tokens; close c
        | (B n) :: s when n = c -> Queue.add SEMICOLON tokens;
        | _ -> ()
    let rec pop_until_marker () =
        match !stack with
        | M :: s -> stack := s
        | (B _) :: s -> Queue.add RCURLY tokens; stack := s; pop_until_marker ()
        | [] -> raise Bad_indentation
    let rec print_stack l = 
        match l with 
        | M :: s -> print_string "M|"; print_stack s
        | (B n) :: s -> print_string "B"; print_int n; print_string "|"; print_stack s
        | [] -> ()

    let next_token = 
        fun lexbuf ->
        if Queue.is_empty tokens then 
            begin
                let new_token = next_tokens lexbuf in
                let start_position = lexeme_start_p lexbuf in
                let column = start_position.pos_cnum - start_position.pos_bol in 
                let _ = if !about_weak_mode then
                    begin
                        (* print_string "about weak mode\n"; *)
                        close column;
                        stack := (B column) :: !stack;
                        about_weak_mode := false;
                        is_weak_mode := true 
                    end
                in match new_token with
                | LIDENT id ->
                    begin
                    try 
                        let token = Hashtbl.find keyword_hashtable id in
                        match token with 
                        | IF | LPAREN | CASE -> 
                            if !is_weak_mode then is_weak_mode := false else close column;
                            stack := M :: !stack;
                            Queue.add token tokens
                        | RPAREN | THEN | ELSE | IN -> 
                            pop_until_marker ();
                            if token = THEN then stack := M :: !stack;
                            Queue.add token tokens
                        | WHERE | DO | LET | OF -> 
                            if !is_weak_mode then is_weak_mode := false else close column;
                            if token = LET then stack := M :: !stack;
                            if token = OF then pop_until_marker ();
                            Queue.add token tokens;
                            Queue.add LCURLY tokens;
                            about_weak_mode := true
                        | _ -> 
                            begin
                                if !is_weak_mode then is_weak_mode := false else close column;
                                Queue.add token tokens
                            end
                    with 
                        | Not_found -> if !is_weak_mode then is_weak_mode := false else close column; Queue.add new_token tokens
                        | Bad_indentation -> raise Bad_indentation
                    end
                | EOF -> if !is_weak_mode then is_weak_mode := false else close (-1); Queue.add EOF tokens
                | _ -> if !is_weak_mode then is_weak_mode := false else close column; Queue.add new_token tokens
            end;
        (*
        print_string "stack: ";
        print_stack !stack;
        print_string "\n";
        let t = Queue.pop tokens
        in begin 
        match t with 
            | IMPORT -> print_string "Import\n"
            | SEMICOLON -> print_string "Semicolon\n"
            | UIDENT s -> print_string "Uident "; print_string s; print_string "\n"
            | LIDENT s -> print_string "Lident "; print_string s; print_string "\n"
            | CONSTANT (Integer i) -> print_string "Integer "; print_int i; print_string "\n"
            | CONSTANT (String s) -> print_string "String "; print_string s; print_string "\n"
            | CASE -> print_string "Case\n"
            | OF -> print_string "Of\n"
            | LCURLY -> print_string "Lcurly\n"
            | RCURLY -> print_string "Rcurly\n"
            | EQUAL -> print_string "Equal\n"
            | _ -> print_string "Other\n"
        end; t
        *)
        Queue.pop tokens
}
