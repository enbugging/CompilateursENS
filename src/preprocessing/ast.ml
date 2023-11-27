(* Abstract syntax tree of Little-PureScript *)

(* (identifier, number of line, number of column) *)
type ident = string

type typed = 
  | TypeConstructor of ntype
  | TypeIdent of ident
and ntype = ident * typed list

type name = Name of ident

(* Constructor (constructor name, list of arguments) *)
type constructor = Constructor of name * typed list

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
  | PatternArgument of patarg 
  | PatternConstructor of ident * patarg list
and patarg = 
  | PatargConstant of constant
  | PatargIdent of ident 
  | Pattern of pattern 

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


type decl =
  (* Definition *)
  | Definition of ident * patarg list * expression
  (* Type declaration *)
  | TypeDeclaration of ident * typed list * typed list * typed list * typed
	(* Data (newTypename, types of arguments, cases) *)
	| Data of name * typed list * constructor list
	(* Class (name of Class, types of arguments, methods) *)
	| Class of name * typed list * decl list
	(* Instance (name of class, types of arguments, methods) *)
	| Instance of ntype list * decl list

type import = ident
type file = File of import list * decl list
