open Tast
open PrettyPrinterBeta
open Preprocessing.Ast

(*Fonctions de gestion des environnements et misc*)
let ajoute_l_env_var v e =
	{	
		instances=e.instances;
        vars = v::(List.filter (fun s -> s<>v) e.vars);
        vdecl = (v, Tvar v)::(List.filter (fun (s,_) -> s<>v) e.vdecl)
	}

let ajoute_l_env_assoc v tau e =
	{
		instances=e.instances;
        vars = v::List.filter (fun s -> s<>v) e.vars;
        vdecl = (v, tau)::List.filter (fun (s,_) -> s<>v) e.vdecl
	}

let ajoute_l_env_instance (i_name, l) e =
        {instances = (i_name, l)::(List.filter (fun (s,_) -> s<>i_name) e.instances);
        vars = e.vars;
        vdecl = e.vdecl
        }

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

let ajoute_g_env_class nom vars fonctions g_env =
        {
                types = g_env.types;
                datas = g_env.datas;
                fonctions = g_env.fonctions;
                classes = (nom, vars, fonctions)::g_env.classes;
                instances = g_env.instances;
                schemas = g_env.schemas;
        }

let ajoute_g_env_instance instances i g_env =
        {
                types = g_env.types;
                datas = g_env.datas;
                fonctions = g_env.fonctions;
                classes = g_env.classes;
                instances = i::g_env.instances;
                schemas = (instances, i)::g_env.schemas;
        }

let ajoute_g_env_schema instances i g_env =
        {
                types = g_env.types;
                datas = g_env.datas;
                fonctions = g_env.fonctions;
                classes = g_env.classes;
                instances = g_env.instances;
                schemas = (instances, i)::g_env.schemas;
        }

let substitution assoc_list l = 
        List.map (fun v -> 
			match List.assoc_opt v assoc_list with
			| None -> Tvar v
			| Some t -> t
		) l


let rec substitution_type assoc_list = function
        | Tvar tvar -> 
			begin 
				match List.assoc_opt tvar assoc_list with
				| None -> Tvar tvar
				| Some t -> t
        	end
		| Teffect t -> Teffect (substitution_type assoc_list t)
        | Tconstr (s,t_list) -> Tconstr(s, List.map (substitution_type assoc_list) t_list)
        | t -> t


let type_of_var_l_env x start_p end_p env =
    begin
        try List.assoc x env.vdecl 
        with Not_found -> raise (Error (start_p,end_p, "Variable "^x^" not found in environment"))
    end

let rec plus_precis env = function
        | _,Tvar _ -> true
        | Tvar _, _ -> true
        | Tint, Tint -> true
        | Tstring, Tstring -> true
		| Tbool, Tbool -> true
		| Tunit, Tunit -> true
		| Teffect t, Teffect t' -> plus_precis env (t,t')
        | Tconstr (s,t_list), Tconstr (s',t_list') -> s=s' && List.for_all (plus_precis env) (List.combine t_list t_list')
        | tau1,tau2 -> false

let trouve_l_env_instance env i =
        let i_name, tau_list = i in
        let rec find = function
                | [] -> false
                | (i_name', tau_list') :: q -> 
					(i_name=i_name' && List.for_all (fun (t,t') ->
							plus_precis env (t, t')
					) (List.combine tau_list' tau_list))
					|| find q

        in find env.instances

let trouve_g_env_instance (env:global_environment) i =
        let i_name, tau_list = i in
        let rec find = function
                | [] -> false
                | (i_name', tau_list') :: q -> 
                                (i_name=i_name' && List.for_all (fun (t,t') ->
                                        plus_precis env (t, t')
                                ) (List.combine tau_list tau_list'))
                                || find q

        in find env.instances

let trouve_g_env_schema_pour env start_p end_p i =
        let i_name, tau_list = i in
        let rec find = function
                | [] ->  raise (Error (start_p, end_p, "On est là: Nonexistent schema for "^i_name^" \n"))
                | (i_list, (i_name',tau_list')) :: q ->
					if i_name=i_name' && List.for_all (plus_precis env) (List.combine tau_list tau_list')
					then i_list
					else find q

        in find env.schemas

let trouve_g_env_constructeur x g_env start_p end_p =
        let rec find =
			function
			| [] -> raise (Error (start_p, end_p, "Nonexistent constructor "^x^"\n"))
			| (data_name, vars, constructeurs) :: q -> 
				begin match List.assoc_opt x constructeurs with
				| None -> find q
				| Some tau_list -> (data_name, vars, tau_list)
				end

        in find g_env.datas

let trouve_g_env_fonction f g_env start_p end_p =
        let rec find =
			function
			| [] -> raise (Error (start_p, end_p, "Nonexistent fonction "^f^"\n"))
			| (nom, vars, instances, tau_list ) :: q -> 
							if nom=f then (f,vars,instances, tau_list)
							else find q
        in find g_env.fonctions

let rec trouve_g_env_data s g_env start_p end_p = 
        let rec find = function
        | [] -> raise (Error (start_p, end_p, "Nonexistent data "^s^" "))
        | (data_name, vars, constructors) as res :: q when data_name = s -> res
        | _ :: q -> find q
        in find g_env.datas
        
let rec trouve_g_env_classe c g_env start_p end_p =
        let rec find = function
                | [] -> raise (Error (start_p, end_p, "Nonexistent classe "^c^" !"))
                | (s, vars, funs) as res :: q when s=c -> res
                | _ :: q -> find q
        in find g_env.classes

let rec existe_g_env_classe s g_env =
        let rec find = function
                | [] -> false
                | (class_name,_,_) :: q -> class_name = s || find q
        in find g_env.classes

let rec existe_g_env_fonction s g_env =
        let rec find = function
                | [] -> false
                | (f,_,_,_) :: q -> f=s || find q
        in find g_env.fonctions

let rec existe_g_env_data s g_env =
        let rec find = function
                | [] -> false
                | (data_name,_,_) :: q -> data_name = s || find q
        in find g_env.datas

let rec existe_g_env_constructor s g_env =
        let rec find = function
                | [] -> false
                | (_,_,l) :: q -> List.exists (fun (x,_) -> x=s) l || find q
        in find g_env.datas

let rec existe_g_env_type s g_env =
        let rec find = function
                | [] -> false
                | (x,_) :: q -> s=x || find q
        in find g_env.types

let existe_g_env_without_error x_i g_env = existe_g_env_type x_i g_env || existe_g_env_classe x_i g_env || existe_g_env_fonction x_i g_env || existe_g_env_data x_i g_env || existe_g_env_constructor x_i g_env

let existe_g_env x_i g_env start_p end_p =
        if existe_g_env_type x_i g_env then
                                        raise (Error (start_p, end_p, "Le type "^x_i^" existe deja"))
                                else
                                        if existe_g_env_classe x_i g_env then
                                        raise (Error (start_p, end_p, "La classe "^x_i^" existe deja"))
                                        else
                                                if existe_g_env_fonction x_i g_env then
                                        raise (Error (start_p, end_p, "La fonction "^x_i^" existe deja"))
                                                else
                                                        if existe_g_env_data x_i g_env then
                                        raise (Error (start_p, end_p, "Le type "^x_i^" existe deja"))
                                                else
                                                        if existe_g_env_constructor x_i g_env then
                                        raise (Error (start_p, end_p, "Le constructeur "^x_i^" existe deja"))
                                                        else ()
 
let existe_l_env x l_env =
        match List.assoc_opt x l_env.vdecl with
        | Some _ -> true
        | None -> match List.assoc_opt x l_env.instances with
                        | Some _ -> true
                        | None -> false

let pop_premier = function
        | [] -> failwith "pop_premier d'une liste vide"
        | x :: q -> (x,q)

let rec list_of_premiers = function
        | [[]] -> failwith "list_of_premiers d'une liste de listes vides"
        | [x] -> let p,suite = pop_premier x in ([p], [x])
        | x :: q -> let premiers, suites = list_of_premiers q in
                        let p,suite = pop_premier x in
                        (p::premiers, suite::suites)
        | _ -> failwith "format de liste improbable pour list_of_premiers"

let rec pop_dernier = function
       
	| [] -> failwith "Poping an empty list"
	| [x] -> ([],x)
	| x :: q -> let l,d = pop_dernier q in (x::l,d)

let rec etend_l_env_typed env = function
        | TypeConstructor (Name(c,_,_),l) -> ajoute_l_env_var c (List.fold_left etend_l_env_typed env l)
        | TypeIdent (Name(c,_,_)) -> ajoute_l_env_var c env

let rec etend_l_env env = 
	function
	| PatternArgument p ->
		begin
			match p with
			| PatargConstant _ -> env
			| PatargIdent i -> if List.mem i env.vars then env else ajoute_l_env_var i env
			| Pattern p' -> etend_l_env env p'
		end
	| PatternConstructor (_,l) -> List.fold_left etend_l_env env (List.map (fun p -> PatternArgument p) l)

module Vset = Set.Make(String)

let rec contient_deux_fois_la_meme_var start_p end_p motif = 
	let rec aux vset = 
		function
		| PatternArgument p -> 
			begin 
				match p with
				| PatargConstant _ -> vset
				| PatargIdent i -> 
					if Vset.mem i vset then 
						raise (Error (start_p, end_p, "Multiple occurrences of the same variable in pattern\n")) 
					else vset
				| Pattern p' -> aux vset p'
			end
			| PatternConstructor (_,l) -> List.fold_left aux vset (List.map (fun p -> PatternArgument p) l)
	in aux Vset.empty motif

