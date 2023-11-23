(* Abstract syntax tree of Little-PureScript *)

type ident = string

type typeVariable = TypeVariable of ident
type constructor = Constructor of typeVariable list
type typeDeclaration = ident

type typed = 
	| AType of ident
	| NType of ident * typed list

type constant = 
	| Boolean of bool
	| Integer of int
	| String of string
type binaryOperation = 
	| Plus
	| Minus
	| Times
	| Divide
	| Modulo
	| And 
	| Or
	| Equal
	| NotEqual
	| LessThan
	| LessThanOrEqual
	| GreaterThan
	| GreaterThanOrEqual

type pattern = 
	| PatternConstant of constant
	| PatternVariable of ident
	| PatternConstructor of ident * pattern list

type decl =
	(* Data (newTypename, types of arguments, cases) *)
	| Data of typeVariable * typeVariable list * constructor list
	(* Class (clasName, types of arguments, methods) *)
	| Class of typeVariable * typeVariable list * typeDeclaration list
	(* Instance (type of class, types of arguments, methods) *)
	| Instance of typeDeclaration * typeVariable list * typeDeclaration list

type expression = 
	| Constant of constant
	| Variable of ident
	| TypedExpression of expression * typed
	| BinaryOperation of expression * binaryOperation * expression
	| Conditional of expression * expression * expression
	| FunctionCall of ident * expression list
	| Do of expression list
	| Let of (ident * expression) list * expression
	| Case of expression * (pattern * expression) list

type import = ident
type file = File of import list * decl list

