open Preprocessing.Ast 

type typ = 
	| Tint 
	| Tstring 
	| Tbool
	| Tunit 
        | Tvar of tvar
	| Teffect of typ
        | Tconstr of tconstr

and tvar = string 
	(*{ 
		id : int; 
		mutable def : typ option; 
	}*)

and tconstr = string * typ list

type tdectype = string * typ
type tdecdata = string * string list * tconstr list
type tinstance = string * typ list
type tinstance_schema = tinstance list * tinstance
type tdecfun = string * string list * tinstance list * typ list
type tdecclass = string * tvar list * tdecfun list

type global_environment = {types : tdectype list;
                                datas : tdecdata list;
                                fonctions : tdecfun list;
                                classes : tdecclass list;
                                instances : tinstance list;
                                schemas : tinstance_schema list
        }

let init_g_env = {types = [];
                                datas = [];
                                fonctions = [];
                                classes = [];
                                instances = [];
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

type schema = { vars : Vset.t; typ : typ }
*)


(*type env = { instances : tinstance list; bindings : schema Smap.t; fvars : Vset.t }*)
type env = {instances : tinstance list; vars : string list; vdecl : (string*typ) list }

let empty_env = {instances = []; vars = ["Unit"; "Boolean"; "Int"; "String"]; vdecl = [("Unit",Tunit); ("Boolean",Tbool); ("Int",Tint); ("String",Tstring)]}

(*Fonctions de gestion des environnements*)
let ajoute_l_env_var v e =
        {instances=e.instances;
        vars = v::e.vars;
        vdecl = (v, Tvar v)::e.vdecl}

let ajoute_l_env_assoc v tau e =
        {instances=e.instances;
        vars = v::(List.filter (fun s -> s<>v) e.vars);
        vdecl = (v, tau)::(List.filter (fun (s,_) -> s<>v) e.vdecl)}

let ajoute_g_env_data nom vars c_list g_env = 
        {
        types = g_env.types;
        datas = (nom,vars,c_list)::g_env.datas;
        fonctions = g_env.fonctions;
        classes = g_env.classes;
        instances = g_env.instances;
        schemas = g_env.schemas;
        }

let ajoute_g_env_fonction nom vars i_list t_list g_env = 
        {
        types = g_env.types;
        datas = g_env.datas;
        fonctions = (nom, vars, i_list, t_list)::g_env.fonctions;
        classes = g_env.classes;
        instances = g_env.instances;
        schemas = g_env.schemas;
        }

let substitution assoc_list l = 
        List.map (fun v -> match List.assoc_opt v assoc_list with
                        | None -> Tvar v
                        | Some t -> t) l

exception Error of (Lexing.position * Lexing.position)

let type_of_var_l_env x start_p end_p env =
        begin
        try List.assoc x env.vdecl 
        with Not_found -> raise (Error (start_p,end_p))
        end

let trouve_g_env_constructeur x g_env start_p end_p =
        let rec find = function
                | [] -> let _ = print_string "Constructeur inexistant\n" in raise (Error (start_p,end_p))
                | (data_name, vars, constructeurs) :: q -> 
                                begin match List.assoc_opt x constructeurs with
                                | None -> find q
                                | Some tau_list -> (data_name, vars, tau_list)
                                end

        in find g_env.datas


