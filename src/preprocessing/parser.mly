(* Syntactical analyser for Little-Purescript *)

%{
    open Ast
%}

%token <Ast.constant> CONSTANT
%token <string> LIDENT UIDENT STRING 
%token <Ast.binaryOperation> CMP
%token CASE CLASS DATA DO ELSE FALSE FORALL IF IMPORT IN INSTANCE LET MODULE OF THEN TRUE WHERE
%token EOF
%token LPAREN RPAREN LSQUARE RSQUARE LCURLY RCURLY COMMA EQUAL COLON SEMICOLON ARROW BRANCHING NEWLINE DOT VERTICAL_BAR
%token PLUS MINUS TIMES DIVIDE MODULO AND OR NOT

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

type_type_lident_star:
    | { [] }
    | type_lident=LIDENT type_lident_star=type_lident_star
        { (TypeIdent type_lident) :: type_lident_star }

decl:
    | defn=defn
        { defn }
    | tdecl=tdecl
        { tdecl }
    | DATA name=uident types=type_lident_star EQUAL constructor=constructor constructors=constructor_star
        { Data name types (constructor::constructors)}
    | CLASS name=uident types=type_lident_star WHERE LCURLY tdecl_list=tdecl_semicolon_star RCURLY
        { Class name types tdecl_list }
    | INSTANCE instance=instance WHERE LCURLY defn_list=defn_semicolon_star RCURLY
        { Instance instance defn_list }

decls:
    | decl=decl decls=decls
        { decl :: decls }
    | decl=decl
        { [decl] }

constructor:
    | u=uident ats=atype_star
        { Constructor u ats }
constructor_star:
    | { [] }
    | constructor=constructor constructor_star=constructor_star
        { constructor :: constructor_star }

defn:
    | li=LIDENT pts=patarg_star EQUAL e=expr
        { Definition li pts e }

defn_semicolon_star:
    | { [] }
    | defn=defn SEMICOLON defns=defn_semicolon_star
        { defn::defns }

tdecl:
    | name=LIDENT COLON COLON nts=ntype_arrow_star tys=type_branch_star ty=type
        { TypeDeclaration name [||] nts tys ty }
    | name=LIDENT COLON COLON FORALL li=lident type_lis=type_lident_star DOT nts=ntype_arrow_star tys=type_branch_star ty=type
        { TypeDeclaration name ((TypeIdent li)::type_lis) nts tys ty }

tdecl_semicolon_star:
    | { [] }
    | tdecl=tdecl SEMICOLON tdecls=tdecl_star
        { tdecl::tdecls }

ntype_arrow_star:
    | { [] }
    | nt=ntype ARROW nts=ntype_arrow_star
        { nt::nts }

type_branch_star:
    | { [] }
    | ty=type BRANCHING tys=type_branch_star
        { ty::tys }

ntype:
    | name=UIDENT ats=atype_star
        { TypeConstructor name ats }

ntype_star:
    | { [] }
    | COLON nt=ntype nts=ntype_star
        { nt::nts }

atype:
    | li=LIDENT
        { TypeIdent li }
    | ui=UIDENT
        { TypeIdent ui }

atype_star:
    | { [] }
    | at=atype ats=atype_star
        { at::ats }

type:
    | nt = ntype
        { TypeConstructor nt }
    | at=atype
        { TypeIdent at }

instance:
    | n=ntype
        { [n] }
    | n1=ntype ARROW n2=ntype
        { [n2; n1] }
    | LPAREN n1=ntype ns=ntype_star RPAREN ARROW n2=ntype
        { n2::(n1::ns) }

patarg:
    | c=CONSTANT
        { PatargConstant c }
    | li=LIDENT
        { PatargIdent li }
    | UIDENT
        { PatargIdent ui }
    | LPAREN pts=patargs RPAREN
        { Pattern pts }

patarg_star:
    | { [] }
    | pt=patarg pts=patarg_star
        { pt::pts }

pattern:
    | p=atarg
        { PatternArgument p }
    | s=UIDENT pt=patarg pts=patarg_star
        { PatternConstructor s (pt::pts) }

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