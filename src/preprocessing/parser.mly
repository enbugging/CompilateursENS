(* Syntactical analyser for Little-Purescript *)

%{
    open Ast
%}

%token <Ast.constant_with_position> CONSTANT
%token <Ast.ident_with_position> LIDENT UIDENT STRING 
%token <Ast.binaryOperation_with_position> CMP
%token <int * int> CASE CLASS DATA DO ELSE FALSE FORALL IF IMPORT IN INSTANCE LET MODULE OF THEN TRUE WHERE
%token <int * int> EOF
%token <int * int> LPAREN RPAREN LSQUARE RSQUARE LCURLY RCURLY COMMA EQUAL COLON SEMICOLON ARROW BRANCHING NEWLINE DOT VERTICAL_BAR
%token <int * int> PLUS MINUS TIMES DIVIDE MODULO AND OR NOT

/* Priotity and associativity of tokens */
%left OR
%left AND
%nonassoc NOT
%nonassoc CMP
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%nonassoc unary_minus
%nonassoc LSQUARE

/* Start symbol */
%start file

/* Type of the file returned by the syntactical analyser */
%type <Ast.file> file

%%

file:
    | MODULE (UIDENT "Main") WHERE LCURLY imports decls SEMICOLON RCURLY EOF

imports:
    | IMPORT (UIDENT "Prelude") SEMICOLON 
      IMPORT (UIDENT "Effect") SEMICOLON 
      IMPORT (UIDENT "Prelude.Console") SEMICOLON 

decls:
    | decl=decl decls=decls
        { decl :: decls }
    | decl=decl
        { [decl] }

lident_star:
    | { [] }
    | lident=LIDENT lident_star=lident_star
        { lident :: lident_star }

decl:
    | defn=defn
        { defn }
    | tdecl
    | DATA u1 = uident ls1 = lident_star EQUAL uident atype_star uident_atype_star_star
        { Data (u1, ls1, u2, ) }
    | CLASS uident lident_star WHERE LCURLY tdecl_semicolon_star RCURLY
        {...}
    | INSTANCE instance WHERE LCURLY defn_semicolon_star RCURLY
        {...}

uident_atype_star_star:
    | { () }
    | VERTICAL_BAR uident atype_star uident_atype_star_star
        {...}

defn:
    | LIDENT patarg_star EQUAL expr

defn_semicolon_star:
    | { () }
    | defn SEMICOLON defn_star

tdecl:
    | LIDENT COLON COLON 
    | LIDENT COLON COLON FORALL lident lident_star DOT
        {...}
    | ntype_arrow_star type_branch_star type
        {...}

tdecl_semicolon_star:
    | { () }
    | tdecl SEMICOLON tdecl_star

ntype_arrow_star:
    | { () }
    | ntype ARROW ntype_arrow_star

type_branch_star:
    | { () }
    | type BRANCHING type_branch_star

ntype:
    | UIDENT s atype_star

ntype_star:
    | { () }
    | COLON ntype ntype_star

atype:
    | LIDENT
    | UIDENT

atype_star:
    | { () }
    | atype atype_star

type:
    | n = ntype
        { NType n }
    | atype

instance:
    | ntype
    | ntype ARROW ntyp
    | LPAREN ntype ntype_star RPAREN ARROW ntype

patarg:
    | CONSTANT
    | LIDENT
    | UIDENT
    | LPAREN patargs RPAREN

patarg_star:
    | { () }
    | patarg patarg_star

pattern:
    | patarg
    | UIDENT s patarg patarg_star

constant:
    | TRUE 
    | FALSE
    | c = CONSTANT
        { Integer(c) }
    | STRING


atom:
    | constant
    | LIDENT
    | UIDENT
    | LPAREN e = expr RPAREN
    | LIDENT COLON COLON type

atom_star:
    | { () }
    | atom atom_star

expr:
    | atom 
    | MINUS expr %prec unary_minus
    | expr binop expr
    | LPAREN (LIDENT | UIDENT) RPAREN atom atom_star
    | IF expr THEN expr ELSE expr
    | DO LCURLY expr_star RCURLY
    | LET LCURLY binding binding_star RCURLY IN expr
    | CASE expr OF LCURLY branch branch_star RCURLY

expr_star:
    | { () }
    | expr expr_star

binding:
    | LIDENT EQUAL expr

binding_star:
    | { () }
    | binding binding_star

branch:
    | pattern BRANCHING expr

branch_star:
    | { () }
    | branch branch_star

%inline binop:
| PLUS  { Badd }
| MINUS { Bsub }
| TIMES { Bmul }
| DIV   { Bdiv }
| MOD   { Bmod }
| c=CMP { c    }
| AND   { Band }
| OR    { Bor  }
;