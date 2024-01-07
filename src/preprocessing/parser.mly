(* Syntactical analyser for Little-Purescript *)

%{
    open Ast
%}

%token <Ast.constant> CONSTANT
%token <string> LIDENT UIDENT 
%token <Ast.binaryOperation> CMP
%token CASE CLASS DATA DO ELSE FORALL IF IMPORT IN INSTANCE LET MODULE OF THEN WHERE
%token EOF
%token LPAREN RPAREN LCURLY RCURLY COMMA EQUAL COLON SEMICOLON ARROW BRANCHING DOT VERTICAL_BAR
%token PLUS MINUS TIMES DIVIDE MODULO AND OR NOT CONCATENATE 

/* Priotity and associativity of tokens */
%left OR
%left AND
%left CONCATENATE
%nonassoc NOT
%nonassoc CMP
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%nonassoc unary_minus
%nonassoc ELSE

/* Start symbol */
%start file

/* Type of the file returned by the syntactical analyser */
%type <Ast.file> file

%%

file:
    | MODULE UIDENT WHERE LCURLY i=delimited(IMPORT, UIDENT, SEMICOLON)* d=separated_nonempty_list(SEMICOLON, decl) RCURLY EOF
    {File (i, d)}

type_lident:
    | type_lident=LIDENT
        { TypeIdent (Name (type_lident,$startpos,$endpos)) }

decl:
    | defn=defn
        { defn }
    | tdecl=tdecl
        { tdecl }
    | DATA name=UIDENT types=type_lident* EQUAL constructors=separated_nonempty_list(VERTICAL_BAR, constructor)
        { Data (Name (name,$startpos,$endpos), types, constructors)}
    | CLASS name=UIDENT types=type_lident* WHERE LCURLY tdecls=separated_list(SEMICOLON, tdecl) RCURLY
        { Class (Name (name,$startpos,$endpos), types, tdecls) }
    | INSTANCE instance=instance WHERE LCURLY defns=separated_list(SEMICOLON, defn) RCURLY
        { Instance (instance, defns) }

constructor:
    | u=UIDENT ats=atype*
        { Constructor (Name (u,$startpos,$endpos), ats) }

defn:
    | li=LIDENT pts=patarg* EQUAL e=expression
        { Definition (Name (li,$startpos,$endpos), pts, e) }

tdecl:
    | name=LIDENT COLON COLON tys=separated_nonempty_list(BRANCHING, typed)
        { TypeDeclaration (Name (name,$startpos,$endpos), [], [], tys) }
    | name=LIDENT COLON COLON nts=terminated(typed, ARROW)* tys=separated_nonempty_list(BRANCHING, typed)
        { TypeDeclaration (Name (name,$startpos,$endpos), [], nts, tys) }
    | name=LIDENT COLON COLON FORALL lis=type_lident+ DOT tys=separated_nonempty_list(BRANCHING, typed)
        { TypeDeclaration (Name (name,$startpos,$endpos), lis, [], tys) }
    | name=LIDENT COLON COLON FORALL lis=type_lident+ DOT nts=terminated(typed, ARROW)* tys=separated_nonempty_list(BRANCHING, typed)
        { TypeDeclaration (Name (name,$startpos,$endpos), lis, nts, tys) }

ntype:
    | name=UIDENT 
        { TypeConstructor (Name (name,$startpos,$endpos), []) }
    | name=UIDENT ats=atype+
        { TypeConstructor (Name (name,$startpos,$endpos), ats) }


atype:
    | li=LIDENT
        { TypeIdent (Name (li,$startpos,$endpos)) }
    | ui=UIDENT
        { TypeIdent (Name (ui,$startpos,$endpos)) }
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
    | s=UIDENT pts=patarg+
        { PatternConstructor (s, pts) }

constant:
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
    | LPAREN lident = LIDENT COLON COLON t=typed RPAREN
    {TypedExpression ({
        e = Variable lident;
        location = $startpos, $endpos
    }, t)}

atom_with_pos:
    | a = atom
    { { e = a;
        location = $startpos, $endpos } }

expression:
    
    | e = expr
    { { e = e;
        location = $startpos, $endpos } }

expr:
    |a = atom
    { a }
    | MINUS e = expression %prec unary_minus
    { BinaryOperation ({ 
        e = Constant(Integer 0); 
        location = $startpos, $endpos
        }, Minus, e)}
    | NOT e = expression
    {FunctionCall ("not", [e])}
    | e1 = expression b = binop e2 = expression
    {BinaryOperation (e1,b,e2)}
    | lident=LIDENT a_s = atom_with_pos+
    { FunctionCall (lident, a_s) }
    | uident=UIDENT a_s = expression+
    { ExplicitConstructor (uident, a_s) }
    | IF e1 = expression THEN e2 = expression ELSE e3 =expression
    {Conditional (e1,e2,e3)}
    | DO LCURLY es = separated_nonempty_list(SEMICOLON, expression) RCURLY
    {Do es}
    | LET LCURLY bs = separated_nonempty_list(SEMICOLON, binding) RCURLY IN e = expression
    {Let (bs, e)}
    | CASE e = expression OF LCURLY bs = separated_nonempty_list(SEMICOLON, branch) RCURLY
    {Case (e, bs)}

binding:
    | lident=LIDENT EQUAL e = expression
    { (lident,e) }

branch:
    | p = pattern BRANCHING e = expression
    {(p,e)}

%inline binop:
| PLUS          { Plus }
| MINUS         { Minus }
| TIMES         { Times }
| DIVIDE        { Divide }
| MODULO        { Modulo }
| c=CMP         { c    }
| AND           { And }
| OR            { Or  }
| CONCATENATE   { Concatenate }
;
