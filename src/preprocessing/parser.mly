(* Syntactical analyser for Little-Purescript *)

%{
    open Ast
%}

%token <Ast.constant> CONSTANT
%token <string> LIDENT UIDENT 
%token <Ast.binaryOperation> CMP
%token CASE CLASS DATA DO ELSE FALSE FORALL IF IMPORT IN INSTANCE LET MODULE OF THEN TRUE WHERE
%token EOF
%token LPAREN RPAREN LCURLY RCURLY COMMA EQUAL COLON SEMICOLON ARROW BRANCHING DOT VERTICAL_BAR
%token PLUS MINUS TIMES DIVIDE MODULO AND OR NOT

/* Priotity and associativity of tokens */
%left OR
%left AND
%nonassoc NOT
%nonassoc CMP
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%nonassoc unary_minus

/* Start symbol */
%start file

/* Type of the file returned by the syntactical analyser */
%type <Ast.file> file

%%

file:
    | MODULE UIDENT WHERE LCURLY i=imports d=separated_nonempty_list(SEMICOLON, decl) RCURLY EOF
    {File (i, d)}

imports:
    | IMPORT UIDENT SEMICOLON 
      IMPORT UIDENT SEMICOLON 
      IMPORT UIDENT SEMICOLON 
      {["Prelude";"Effect";"Prelude.Console"]}

type_lident:
    | type_lident=LIDENT
        { TypeIdent type_lident }

decl:
    | defn=defn
        { defn }
    | tdecl=tdecl
        { tdecl }
    | DATA name=UIDENT types=list(type_lident) EQUAL constructors=separated_nonempty_list(VERTICAL_BAR, constructor)
        { Data (Name name, types, constructors)}
    | CLASS name=UIDENT types=list(type_lident) WHERE LCURLY tdecls=separated_list(SEMICOLON, tdecl) RCURLY
        { Class (Name name, types, tdecls) }
    | INSTANCE instance=instance WHERE LCURLY defns=separated_list(SEMICOLON, defn) RCURLY
        { Instance (instance, defns) }

constructor:
    | u=UIDENT ats=list(atype)
        { Constructor (Name u, ats) }

defn:
    | li=LIDENT pts=list(patarg) EQUAL e=expr
        { Definition (li, pts, e) }

tdecl:
    | name=LIDENT COLON COLON nts=list(ntype_arrow) tys=separated_nonempty_list(BRANCHING, typed)
        { TypeDeclaration (name, [], nts, tys) }
    | name=LIDENT COLON COLON FORALL lis=nonempty_list(type_lident) DOT nts=list(ntype_arrow) tys=separated_nonempty_list(BRANCHING, typed)
        { TypeDeclaration (name, lis, nts, tys) }

ntype_arrow:
    | t=typed ARROW
        { t }

ntype:
    | name=UIDENT 
        { TypeConstructor (Name name, []) }
    | name=UIDENT ats=nonempty_list(atype)
        { TypeConstructor (Name name, ats) }

atype:
    | li=LIDENT
        { TypeIdent li }
    | ui=UIDENT
        { TypeIdent ui }
    | LPAREN t = typed RPAREN
        { t }

typed:
    | nt = ntype
        { nt }
    | at = atype
        { at }

instance:
    | n=ntype
        { [n] }
    | n1=ntype ARROW n2=ntype
        { [n2; n1] }
    | LPAREN ns=separated_nonempty_list(COMMA, ntype) RPAREN ARROW n=ntype
        { n::ns }

patarg:
    | c=CONSTANT
        { PatargConstant c }
    | li=LIDENT
        { PatargIdent li }
    | ui=UIDENT
        { PatargIdent ui }
    | LPAREN pts=pattern RPAREN
        { Pattern pts }

pattern:
    | p=patarg
        { PatternArgument p }
    | s=UIDENT pts=nonempty_list(patarg)
        { PatternConstructor (s, pts) }

constant:
    | TRUE 
    {Boolean true}
    | FALSE
    {Boolean false}
    | c = CONSTANT
        { c }

atom:
    | c = constant
    { Constant c }
    | lident=LIDENT
    { Variable lident }
    | uident=UIDENT
    { Variable uident }
    | LPAREN e = expr RPAREN
    { e }
    | lident = LIDENT COLON COLON t=typed
    {TypedExpression (Variable lident, t)}

expr:
    |a = atom 
    { a }
    | MINUS e = expr %prec unary_minus
    {BinaryOperation (Constant(Integer 0), Minus, e)}
    | NOT e = expr
    {UnaryOperation (Not, e)}
    | e1 = expr b = binop e2 = expr
    {BinaryOperation (e1,b,e2)}
    | LPAREN lident=LIDENT RPAREN a_s = nonempty_list(atom)
    { FunctionCall (lident, a_s) }
    | LPAREN uident=UIDENT RPAREN a_s = nonempty_list(atom)
    { FunctionCall (uident, a_s) }
    | IF e1 = expr THEN e2 = expr ELSE e3 =expr
    {Conditional (e1,e2,e3)}
    | DO LCURLY es = separated_nonempty_list(SEMICOLON, expr) RCURLY
    {Do es}
    | LET LCURLY bs = separated_nonempty_list(SEMICOLON, binding) RCURLY IN e = expr
    {Let (bs, e)}
    | CASE e = expr OF LCURLY bs = separated_nonempty_list(SEMICOLON, branch) RCURLY
    {Case (e, bs)}

binding:
    | lident=LIDENT EQUAL e = expr
    { (lident,e) }

branch:
    | p = pattern BRANCHING e = expr
    {(p,e)}

%inline binop:
| PLUS  { Plus }
| MINUS { Minus }
| TIMES { Times }
| DIVIDE   { Divide }
| MODULO   { Modulo }
| c=CMP { c    }
| AND   { And }
| OR    { Or  }
;