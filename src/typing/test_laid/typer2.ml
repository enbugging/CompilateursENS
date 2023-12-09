open Tast

let g_env = {types = [];
                datas = [];
                fonctions=[];
                classes=[];
                instances=[];
                schemas=[]
        }

let empty = { instances = []; vars = []; vdecl = [] }

(*Petites fonctions auxiliaires*)
let trouve_type_of_var_lenv x env =
        let rec aux = function
                | [] -> failwith "Pas d'association pour cette variable dans l'environnement local"
                | (y,t)::q -> if y=x then t else aux q
        in aux env.vdecl

let trouve_explicit_constructor s =
        let rec aux = function
                | [] -> failwith "Constructeur non déclaré"
                | (nom,vars,constructors) :: q -> begin match List.assoc_opt s constructors with
                                                | None -> aux q
                                                | Some(t_list) -> (nom,vars,t_list)
                end

        in aux g_env.datas


let trouve_decfun_g_env s =
        let rec aux = function
                | [] -> failwith "fonction non déclarée"
                | (s',_,_,_) as res :: q -> if s=s' then res else aux q
        in aux g_env.fonctions

let rec resoud_instance env sigma (nom_i, t_list) = ()
        (*on trouve une instance plus générale dans l'environnement local*)
        (*Ou on trouve une instance plus générale dans l'environnement global*)
        (*Ou on trouve un schema d'instance qui donne une instance plus générale que la notre*)

let rec pop_dernier = function
        | [] -> failwith "Pop dernier d'une liste vite..."
        | [x] -> ([],x)
        | x :: q -> let l,dernier = pop_dernier q in (x::l,dernier)

let rec applique_sub assoc_list tau = match tau with
        | Tvar tvar -> begin match List.assoc_opt tvar assoc_list with
                                | None -> Tvar tvar
                                | Some t -> t
        end
        | Teffect t -> Teffect (applique_sub assoc_list t)
        | Tconstr (s, t_list) -> Tconstr (s, List.map (applique_sub assoc_list) t_list)
        | _ -> tau

let ajoute_contexte_decl s tau env =
        {instances=env.instances; vars=env.vars; vdecl=(s,tau)::env.vdecl}

(*vérifie que les variables d'une liste de variables ont des noms distincts*)
let rec noms_distincts_vars = function
        | [] -> true
        | s :: q -> not (List.mem s q) && noms_distincts_vars q

(*vérifie que les constructeurs d'une liste de constructeurs ont des noms distincts*)
let noms_distincts_constructors constructor_list = 
        noms_distincts_vars (List.map (fun (Ast.Constructor (Name s,_ )) -> s) constructor_list)

let noms_distincts_et_nouveaux_fun defs =
        let defs' = List.map (fun (Ast.Definition (s,_,_)) -> s) defs in
        noms_distincts_vars defs'
        && (let existants = List.map (fun (s,_,_,_) -> s) g_env.fonctions in 
        List.for_all (fun s -> not (List.mem s existants)) defs')

(*Fonction utilisée pour trouver des substitution de variables de type de dans le cas des expressions de la forme (e::tau)*)
let rec unifie_sub env sigma = function
        | Tvar tvar, tau -> sigma := List.map (fun (v,t) -> if v<>tvar then (v,t) else (v, tau)) !sigma
        | tau, Tvar tvar -> begin
                        try 
                                (let tau' = trouve_type_of_var_lenv tvar env in
                                unifie_sub env sigma (tau, tau'))
                        with _ -> ()
        end
        | Tint, Tint -> ()
        | Tstring, Tstring -> ()
        | Tbool, Tbool -> ()
        | Tunit, Tunit -> ()
        | Teffect t1, Teffect t2 -> unifie_sub env sigma (t1,t2)
        | Tconstr (_,l1), Tconstr (_,l2) -> List.iter (unifie_sub env sigma) (List.combine l1 l2)
        | _,_ -> failwith "Impossible d'unifier les types"


(*Fonction qui renvoie le type d'un type suggéré bien formé et plante sinon*)
let rec bf env = function
        | Ast.TypeIdent "Unit" -> Tunit
        | Ast.TypeIdent "Boolean" -> Tbool
        | Ast.TypeIdent "Int" -> Tint
        | Ast.TypeIdent "String" -> Tstring
        | Ast.TypeIdent s -> trouve_type_of_var_lenv s env
        | Ast.TypeConstructor (Name s, t_list) -> 
                        begin match trouve_type_of_var_lenv s env with 
                                |Tconstr (_,verif_arite) ->
                                                if List.length t_list = List.length verif_arite then
                                                        Tconstr (s, List.map (bf env) t_list) 
                else
                        failwith "Nombre de types incompatible avec l'arité du constructeur"
                                | _ -> failwith "Le compilateur s'attendait à trouver un constructeur"
                        end


(*Fonction qui renvoie le type d'un expression*)
let rec type_expr env = function
        | Ast.Constant c -> begin match c with
                | Ast.Boolean b -> Tbool
                | Ast.Integer i -> Tint
                | Ast.String s -> Tstring
  end

        | Ast.Variable x -> trouve_type_of_var_lenv x env
        | Ast.UnaryOperation (Not, e) -> if type_expr env e = Tbool then Tbool else failwith "Not appliqué à une expression non booléenne"

        | Ast.TypedExpression (e, tau) -> 
                  let res = type_expr env e in 
                  let tau' = bf env tau in
                  if res=tau' then
                        res 
                  else
                          failwith "Type suggéré mal formé"
  
        | Ast.BinaryOperation (e1, binop, e2) -> 
                        begin match type_expr env e1, binop, type_expr env e2 with
                        | Tint, Plus, Tint | Tint, Minus, Tint | Tint, Times, Tint | Tint, Divide, Tint -> Tint 
                        | Tint, Equal, Tint | Tint, NotEqual, Tint | Tint, LessThan, Tint | Tint, LessThanOrEqual, Tint | Tint, GreaterThan, Tint | Tint, GreaterThanOrEqual, Tint -> Tbool 
                        | Tbool, Equal, Tbool | Tbool, NotEqual, Tbool | Tstring, Equal, Tstring | Tstring, NotEqual, Tstring -> Tbool
                        | Tbool, And, Tbool | Tbool, Or, Tbool -> Tbool
                        | Tstring, Concatenate, Tstring -> Tstring
                        | _,_,_ -> failwith "Opérandes ou opération invalide pour une opération binaire"
  end
        | Ast.Conditional (e1, e2, e3) -> if type_expr env e1 = Tbool then
                let res = type_expr env e2 in
                if type_expr env e3 = res then 
                        res
                else 
                        failwith "Type de retour différents dans un if"
        else
                failwith "Opérande non booléenne dans un if"

        | Ast.Do e_list -> 
                  if List.for_all (fun e -> type_expr env e = Teffect Tunit) e_list then 
                          Teffect Tunit
                  else
                          failwith "Les expressions du Do doivent être de type Effect Unit"

        | Ast.Let (l, e) -> let env' = ref env in
  List.iter (fun (s,exp) -> let tau = type_expr !env' exp in env' := ajoute_contexte_decl s tau !env') l; type_expr !env' e

        | Ast.ExplicitConstructor (s, expr_l) (*ident * expression list*) -> 
                        let nom, vars, t_list = trouve_explicit_constructor s in
                        let sigma = ref (List.map (fun v -> (v, Tvar v)) vars) in
                        List.iter (fun (e_i,tau_i) -> unifie_sub env sigma (tau_i, type_expr env e_i) ) (List.combine expr_l t_list);
                        Tconstr (nom, List.map (fun (v,t) -> t) !sigma)

	| Ast.FunctionCall (s, expr_l) (*ident * expression list*) ->
                        let s,vars,instances,t_list = trouve_decfun_g_env s in
                        let sigma = ref (List.map (fun v -> (v, Tvar v)) vars) in
                        let t_list', dernier = pop_dernier t_list in
                        List.iter (fun (e_i,tau_i) -> unifie_sub env sigma (tau_i, type_expr env e_i) ) (List.combine expr_l t_list');
                        List.iter (fun i -> resoud_instance env sigma i) instances;
                        applique_sub !sigma dernier

	| Ast.Case (e, p_e_list) (*expression * (pattern * expression) list*) -> Tunit


(*Fonction appelé pour enregistrer la declaration d'un type dans l'environnement global*)
let declaration_de_type = function
        | Ast.Data (Name nom, vars, constructors) ->
                        let vars' = List.map (fun v -> match v with
                                                        |Ast.TypeIdent s -> s
                                                        | _ -> "Un constructeur n'était pas attendu à cet endroit") vars in
                        if noms_distincts_vars vars' then
                                if noms_distincts_constructors constructors then
                                        g_env.datas <- (nom, vars', List.map (fun (Ast.Constructor (Name c_name, t_list)) -> (c_name, List.map (bf {instances = []; vars = vars'; vdecl = List.map (fun v -> (v, Tvar v)) vars'}) t_list)) constructors) :: g_env.datas
                                else
                                        failwith "Il existe un conflit de noms sur les constructeurs de la declaration de type"
                        else
                                        failwith "Il existe un conflit de noms sur les variables de la declaration de type"


        | _ -> failwith "Mauvais argument pour declaration_de_type"


let declaration_de_fonction = function
        | Ast.TypeDeclaration (name,vars,instances,types) -> ()
        | Definition (s, p_list, e) -> ()


let declaration_de_class = function
        | Ast.Class (Name name,vars,decls) ->
                let vars' = List.map (fun v -> match v with
                                                        |Ast.TypeIdent s -> s
                                                        | _ -> "Un constructeur n'était pas attendu à cet endroit") vars in
                if noms_distincts_vars vars' then
                        if noms_distincts_et_nouveaux_fun decls then
                                let env = {instances = []; vars = vars'; vdecl = List.map (fun v -> (v, Tvar v)) vars'} in
                                let res = ref [] in
                                List.iter (
                                        fun (Ast.TypeDeclaration (f,_,_,t_list)) -> 
                                                let to_add = (f,vars',[(name, List.map (fun v -> Tvar v) vars')], List.map (bf env) t_list) in
                                                g_env.fonctions <- to_add::g_env.fonctions;
                                                res := to_add::!res
                                ) decls;
                                g_env.classes <- (name,vars',!res)::g_env.classes
                        else
                                failwith "Il existe un conflit de noms sur les noms de fonctions de la declaration de class"
                else
                        failwith "Il existe un conflit de noms sur les variables de la declaration de class"
        | _ -> failwith "La fonction declaration de class a reçu autre chose qu'une class en argument"

let trouve_class_g_env c =
        let rec aux = function
                | [] -> failwith "La classe instanciée n'existe pas"
                | (c',vars,decls) as res :: q -> if c=c' then res else aux q
        in aux g_env.classes


let meme_ensemble_de_fonctions decls defs =
        let defs_names = List.map (fun (Ast.Definition (s,_,_)) -> s) defs in
        let decls_names = List.map (fun (s,_,_,_) -> s) decls in
        List.for_all (fun s -> List.mem s decls_names) defs_names 
        && List.for_all (fun s -> List.mem s defs_names) decls_names


let rec equations_successives = function
        | [] -> true
        | [x] -> true
        | (s,_,_,_) :: (s',v,i,t) :: q ->
                        if s=s' then 
                                equations_successives ((s',v,i,t)::q)
                else
                        (not (List.exists (fun (s1,_,_,_) -> s=s1) q)) && equations_successives ((s',v,i,t)::q)


let definitions_conformes = function
        | x :: qdecls, y :: qdefs -> true

let declaration_d_instance = function
        (*Le premier élément de t_list est la class de l'instance*)
        | Ast.Instance (t_list, defs) -> 
                        let Ast.TypeConstructor (Name c_name, tau_list) = List.hd t_list in
                        let instances = (match t_list with
                                        | [] -> []
                                        | x :: q -> q
                        ) in
                                let c,vars,decls = trouve_class_g_env c_name in
                                if meme_ensemble_de_fonctions decls defs then
                                        if equations_successives decls then
                                                if definitions_conformes (decls, defs) then
                                                        ()
                                                        (*Vérifier qu'aucune instance ne peut être unifiée avec celle-ci*)
                                                else
                                                        failwith "Une des équations n'est pas conforme"
                                        else
                                                failwith "Il existe une fonctions dont les équations de sont pas successives"
                                else
                                        failwith "Les equations ne definissent pas le meme ensemble de fonctions que celui de la classe"

let rec separe_defs_suite name l = match l with
        | Ast.Definition (s,_,_) as d :: q -> 
                        if s=name then 
                                let l1, l2 = separe_defs_suite name q in (d::l1,l2)
                        else
                                ([],l)

        | _ -> failwith "La fonction separe_defs_suite attends une liste de definitions"
                                
let rec type_declarations l = match l with
        | [] -> ()
        | Ast.Data _ as d :: q -> declaration_de_type d; type_declarations q
        | Ast.TypeDeclaration (name,_,_,_) as td :: q -> 
                        declaration_de_fonction td;
                        let definitions, suite_du_fichier = separe_defs_suite name q in 
                        List.iter declaration_de_fonction definitions;
                        type_declarations suite_du_fichier
        | Ast.Class _ as c :: q -> declaration_de_class c; type_declarations q
        | Ast.Instance _ as i :: q -> declaration_d_instance i; type_declarations q
        | Ast.Definition _  :: q -> failwith "Il ne devrait pas y avoir de definition situé en dehors d'une declaration de fonction ou d'instance"
        
let type_fichier = function
        | Ast.File _,l -> type_declarations l

let rec print_type = function
	| Tint -> print_string " Tint "
	| Tstring -> print_string " Tstring "
	| Tbool -> print_string " Tbool "
	| Tunit -> print_string " Tunit "
        | Tvar tvar -> print_char ' '; print_string tvar; print_char ' '
        | Teffect t -> print_string " Teffect("; print_type t; print_string ") "
        | Tconstr (s, l) -> print_string s; List.iter print_type l

let _ = declaration_de_type (Ast.Data (Name "Foo", [TypeIdent "a"; TypeIdent "b"], [Constructor (Name "Nil", []); Constructor (Name "Foo", [TypeIdent "a"])]))

let _ = print_type (type_expr empty (Ast.ExplicitConstructor ("Foo", [Ast.Constant (Ast.String "toto")])))
