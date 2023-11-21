(* Lexical analyser for Little-Purescript *)

{
    open Lexing
    open Parser
    open Ast

    exception Bad_ident
    exception Integer_overflow
    exception Illegal_character of char
    exception Undetermined_string
    exception Bad_indentation

    (* Handling of significant indentation *)
    type indentation = 
        | B of int (* block *)
        | M        (* marker *)

    let string_buffer = Buffer.create 1024

    let stack = ref [(B 0)] (* indentation stack *)
    let rec close c = 
        match !stack with
        | (B n) :: s when n > c -> stack := s; RCURLY :: (close n)
        | (B n) :: s when n = c -> stack := s; [SEMICOLON]
        | _ -> []

    (* Handling of keywords *)

    let keyword_hastable = Hashtbl.create 32 in
        List.iter (fun (s, token) -> Hashtbl.add keyword_hastable s token)
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

    let id_or_keyword s c = 
        try 
            let rec pop_until_marker = function
                | M :: s -> stack := s
                | _ :: s -> pop_until_marker s
                | [] -> raise Bad_identation
            in
            (* Algorithm to handle significant indentation (cf. Section 1.2 in sujet-v2.pdf) *)
            let token = Hashtbl.find keyword_hastable s in
                match token with
                | IF | LPAREN | CASE -> 
                    begin
                        let res = close c in
                        stack := (M :: !stack);
                        res @ [token]
                    end
                | RPAREN | THEN | ELSE | IN -> 
                    begin
                        let res = close c in
                        pop_until_marker !stack;
                        if token <> THEN then stack := (M :: !stack);
                        res @ [token]
                    end
                | WHERE | DO | LET | OF -> 
                    begin
                        let res = close cin 
                        if token <> LET then stack := (M :: !stack);
                        if token <> OF then pop_until_marker !stack;
                        res @ [token; LBRACKET]
                    end
                | _ -> [token] @ (close c)
        with Not_found -> LIDENT s
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
    | '\n'          { new_line lexbuf }
    | (space | line_comment)+
                    { next_tokens lexbuf }
    | lident as id   {
                        let start_position = lexeme_start_p lexbuf in
                        let column = start_position.pos_cnum - start_position.pos_bol in
                        id_or_keyword id column;
                        if id = "where" || id = "of" || id = "let" || id = "of" then weak_mode lexbuf
                    }
    | uident as id  { [UIDENT id] }
    | '+'           { [PLUS] }
    | '-'           { [MINUS] }
    | '*'           { [TIMES] }
    | '/'           { [DIVIDE] }
    | '%'           { [MODULO] }
    | '='           { [EQUAL] }
    | "=="          { [CMP Beq] }
    | "!="          { [CMP Bne] }
    | "<"           { [CMP Blt] }
    | "<="          { [CMP Ble] }
    | ">"           { [CMP Bgt] }
    | ">="          { [CMP Bge] }
    | "&&"          { [AND] }
    | "||"          { [OR] }
    | "!"           { [NOT] }
    | "=>"          { [ARROW] }
    | "->"          { [BRANCHING] }
    | '('           { [LPAREN] }
    | ')'           { [RPAREN] }
    | '{'           { [LCURLY] }
    | '}'           { [RCURLY] }
    | '['           { [LBRACKET] }
    | ']'           { [RBRACKET] }
    | ','           { [COMMA] }
    | ':'           { [COLON] }
    | '.'           { [DOT] }
    | '|'           { [VERTICAL_BAR] }
    | ';'           { [SEMICOLON] }
    | integer as s  { try [CONSTANT (Cint (int_of_string s))]
                        with _ -> raise Integer_overflow }
    | '"'           { string_buffer := ""; string lexbuf }
    | eof           { [EOF] }
    | _ as c        { raise (Illegal_character c) }

and weak_mode = parse 
    | lident as id   {
                        let start_position = lexeme_start_p lexbuf in
                        let cp = start_position.pos_cnum - start_position.pos_bol in
                        close cp;
                        stack := (B cp) :: !stack;
                        id_or_keyword id cp;
                        weak_mode lexbuf    
                    }
    | _             { next_tokens lexbuf }

and string = parse 
    | '"'           { let s = Buffer.contents string_buffer in Buffer.reset string_buffer; STRING s }
    | "\\n"         { Buffer.add_char string_buffer '\n'; string lexbuf }
    | "\\\""        { Buffer.add_char string_buffer '"'; string lexbuf }
    | _ as c        { Buffer.add_char string_buffer c; string lexbuf }
    | eof           { raise Undetermined_string }

{
    let next_token = 
        let tokens = Queue.create () in
        fun lexbuf ->
            if Queue.is_empty tokens then 
                begin
                    let new_tokens = next_tokens lexbuf in
                        List.iter (fun t -> Queue.add t new_tokens) new_tokens;
                end;
            Queue.pop tokens
}