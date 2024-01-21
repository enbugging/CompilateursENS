open Preprocessing.Ast 

module Smap = Map.Make(String)
module Sset = Set.Make(String)

type typ = 
	| Tint 
	| Tstring 
	| Tbool
	| Tunit 
        | Tvar of string
        | QuantifTvar of string
	| Teffect of typ
        | Tconstr of tconstr

and tconstr = string * typ list

type tdectype = string * typ
type tdecdata = string * string list * tconstr list
type tinstance = string * typ list
type tinstance_schema = tinstance list * tinstance
type tdecfun = string * string list * tinstance list * typ list
type tdecclass = string * string list * tdecfun list


and t_expr = 
	| TConstant of constant * typ
	| TVariable of ident * typ
	| TTypedExpression of t_expr * typed * typ
	| TBinaryOperation of t_expr * binaryOperation * t_expr * typ
	| TConditional of t_expr * t_expr * t_expr * typ
        | TExplicitConstructor of ident * t_expr list * typ
	| TFunctionCall of ident * tinstance list * t_expr list * typ
	| TDo of t_expr list
	| TLet of (ident * t_expr) list * t_expr * typ
	| TCase of t_expr * (pattern * t_expr) list * typ

type t_def =
        | TDefinition of string * patarg list * t_expr
  (* Type declaration *)
        | NoDecl (*Type ajouté pour pouvoir renvoyer des couples de déclarations, après le typage, ce qui ne sert que pour joindre une déclaration de fonction à son unique définition une fois la pattern matching remplacé par un case*)

let type_of_texpr = function	
        | TConstant (_,t) -> t
	| TVariable (_, t) -> t
	| TTypedExpression (_,_, t) -> t
	| TBinaryOperation (_,_,_, t) -> t
	| TConditional (_,_,_, t) -> t
        | TExplicitConstructor (_,_,t) -> t
	| TFunctionCall (_,_,_,t) -> t 
        | TDo _ -> Tconstr("Effect", [Tvar "Unit"]) 
	| TLet (_,_,t) -> t
	| TCase (_,_,t) -> t

let set_new_type t = function
        | TConstant (c,_) -> TConstant(c,t)
	| TVariable (x, _) -> TVariable(x,t)
	| TTypedExpression (e,t', _) -> TTypedExpression(e,t',t)
	| TBinaryOperation (e1,binop,e2, _) -> TBinaryOperation (e1,binop,e2, t)
	| TConditional (e1,e2,e3, _) -> TConditional (e1,e2,e3, t)
        | TExplicitConstructor (x,l,_) -> TExplicitConstructor (x,l,t)
	| TFunctionCall (f,i,e,_) -> TFunctionCall (f,i,e,t) 
        | TDo l -> TDo l
	| TLet (x,e,_) -> TLet(x,e,t)
	| TCase (p,l,_) -> TCase(p,l,t)



type tfile = TFile of (decl * t_def) list
type global_environment = {types : tdectype list;
                                datas : tdecdata list;
                                fonctions : tdecfun list;
                                classes : tdecclass list;
                                instances : tinstance list;
                                schemas : tinstance_schema list
        }

let init_g_env = {types = [("Unit",Tunit);("Boolean",Tbool);("Int",Tint);("String",Tstring)];
                                datas = [("_", ["a"], [("Effect", [Tvar "a"]); ("unit", [])])];
                                fonctions = [("not",[],[],[Tbool;Tbool]);
                                                ("mod",[],[],[Tint;Tint;Tint]);
                                                ("log",[],[],[Tstring;Teffect Tunit]);
                                                ("pure",["a"],[],[Tvar "a"; Teffect (Tvar "a")]);
                                                ("show",["a"],[],[Tvar "a"; Tstring])];
                                classes = [("Show",["a"], [("show",["a"],[],[Tvar "a"; Tstring])])];
                                instances = [("Show", [Tbool]);("Show", [Tint])];
                                schemas = []
        }


type env = {instances : tinstance list; vars : string list; vdecl : (string*typ) list }

let empty_env = {instances = []; 
                vars = ["Unit"; "Boolean"; "Int"; "String"]; vdecl = [("Unit",Tunit); ("Boolean",Tbool); ("Int",Tint); ("String",Tstring)]}

exception Error of (Lexing.position * Lexing.position * string)
let error l s =
        let start_pos, end_pos = l in
        raise (Error (start_pos, end_pos, s))
