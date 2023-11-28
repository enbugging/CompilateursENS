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
    | MODULE UIDENT WHERE LCURLY i=imports d=decls SEMICOLON RCURLY EOF
    {File (i, d)}

imports:
    | IMPORT UIDENT SEMICOLON 
      IMPORT UIDENT SEMICOLON 
      IMPORT UIDENT SEMICOLON 
      {["Prelude";"Effect";"Prelude.Console"]}

type_lident_star:
    | { [] }
    | type_lident=LIDENT type_lident_star=type_lident_star
        { (TypeIdent type_lident) :: type_lident_star }

decl:
    | defn=defn
        { defn }
    | tdecl=tdecl
        { tdecl }
    | DATA name=UIDENT types=type_lident_star EQUAL constructor=constructor
        { Data (Name name, types, [constructor])}
    | DATA name=UIDENT types=type_lident_star EQUAL constructor=constructor constructors=constructor_star
        { Data (Name name, types, (constructor::constructors))}
    | CLASS name=UIDENT types=type_lident_star WHERE LCURLY tdecl_list=tdecl_semicolon_star RCURLY
        { Class (Name name, types, tdecl_list) }
    | INSTANCE instance=instance WHERE LCURLY defn_list=defn_semicolon_star RCURLY
        { Instance (instance, defn_list) }

decls:
    | decl=decl decls=decls
        { decl :: decls }
    | decl=decl
        { [decl] }

constructor:
    | u=UIDENT ats=atype_star
        { Constructor (Name u, ats) }
constructor_star:
    | { [] }
    | VERTICAL_BAR constructor=constructor constructor_star=constructor_star
        { constructor :: constructor_star }

defn:
    | li=LIDENT pts=patarg_star EQUAL e=expr
        { Definition (li, pts, e) }

defn_semicolon_star:
    | { [] }
    | defn=defn SEMICOLON defns=defn_semicolon_star
        { defn::defns }

tdecl:
    | name=LIDENT COLON COLON nts=ntype_arrow_star tys=type_branch_star ty=typed
        { TypeDeclaration (name, [], nts, tys, ty) }
    | name=LIDENT COLON COLON FORALL li=LIDENT type_lis=type_lident_star DOT nts=ntype_arrow_star tys=type_branch_star ty=typed
        { TypeDeclaration (name, ((TypeIdent li)::type_lis), nts, tys, ty) }

tdecl_semicolon_star:
    | { [] }
    | tdecl=tdecl SEMICOLON tdecls=tdecl_semicolon_star
        { tdecl::tdecls }

ntype_arrow_star:
    | { [] }
    | t=typed ARROW ts=ntype_arrow_star
        { t::ts }

type_branch_star:
    | { [] }
    | ty=typed BRANCHING tys=type_branch_star
        { ty::tys }

ntype:
    | name=UIDENT ats=atype_star
        { (name, ats) }

ntype_star:
    | { [] }
    | COLON nt=ntype nts=ntype_star
        { nt::nts }

atype:
    | li=LIDENT
        { TypeIdent li }
    | ui=UIDENT
        { TypeIdent ui }
    | LPAREN t = typed RPAREN
        { t }

atype_star:
    | { [] }
    | at=atype ats=atype_star
        { at :: ats }

typed:
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
    | ui=UIDENT
        { PatargIdent ui }
    | LPAREN pts=pattern RPAREN
        { Pattern pts }

patarg_star:
    | { [] }
    | pt=patarg pts=patarg_star
        { pt::pts }

pattern:
    | p=patarg
        { PatternArgument p }
    | s=UIDENT pt=patarg pts=patarg_star
        { PatternConstructor (s, (pt::pts)) }

constant:
    | TRUE 
    {Boolean true}
    | FALSE
    {Boolean false}
    | c = CONSTANT
        { c }

atom:
    | c = constant
    { Constant c}
    | lident=LIDENT
    { Variable lident}
    | uident=UIDENT
    {TypeIdent = uident}
    | LPAREN e = expr RPAREN
    { e }
    | lident = LIDENT COLON COLON t=typed
    {TypedExpression (Variable lident, t)}

atom_star:
    | { () }
    | a = atom a_s = atom_star
    { a :: a_s }

expr:
    |a = atom 
    { a }
    | MINUS e = expr %prec unary_minus
    {BinaryOperation (Constant(Integer 0), Minus, e)}
    | e1 = expr b = binop e2 = expr
    {BinaryOperation (e1,b,e2)}
    | LPAREN lident=LIDENT RPAREN a = atom a_s = atom_star
    { FunctionCall (lident, a :: a_s) }
    | LPAREN uident=UIDENT RPAREN a = atom a_s = atom_star
    { FunctionCall (uident, a :: a_s) }
    | IF e1 = expr THEN e2 = expr ELSE e3 =expr
    {Conditional (e1,e2,e3)}
    | DO LCURLY e = expr_star RCURLY
    {Do e}
    | LET LCURLY b = binding b_s = binding_star RCURLY IN e = expr
    {Let (b :: b_s, e)}
    | CASE e = expr OF LCURLY b = branch b_s = branch_star RCURLY
    {Case (e, b::b_s)}

expr_star:
    | { [] }
    | e = expr e_s = expr_star
    { e :: e_s }

binding:
    | lident=LIDENT EQUAL e = expr
    { (lident,e) }

binding_star:
    | { [] }
    | b = binding b_s = binding_star
    {b :: b_s}

branch:
    | p = pattern BRANCHING e = expr
    {(p,e)}

branch_star:
    | { [] }
    | b = branch b_s = branch_star
    {b :: b_s}

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
