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

type tvar = string
        (*
	{ 
		id : int; 
		mutable def : typ option; 
	}
*)

type tdectype = string * typ
type tdecdata = string * string list * tconstr list
type tinstance = string * typ list
type tinstance_schema = tinstance list * tinstance
type tdecfun = string * string list * tinstance list * typ list
type tdecclass = string * string list * tdecfun list

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
(*
module V = struct
  type t = tvar
  let compare v1 v2 = Stdlib.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id
  let create = let r = ref 0 in fun () -> incr r; { id = !r; def = None }
end

module Vset = Set.Make(V)
module Smap = Map.Make(String)
module Vmap = Map.Make(V)
*)


(*type env = { instances : tinstance list; bindings : schema Smap.t; fvars : Vset.t }*)
type env = {instances : tinstance list; vars : string list; vdecl : (string*typ) list }

let empty_env = {instances = []; 
                vars = ["Unit"; "Boolean"; "Int"; "String"]; vdecl = [("Unit",Tunit); ("Boolean",Tbool); ("Int",Tint); ("String",Tstring)]}

exception Error of (Lexing.position * Lexing.position * string)
let error l s =
        let start_pos, end_pos = l in
        raise (Error (start_pos, end_pos, s))
