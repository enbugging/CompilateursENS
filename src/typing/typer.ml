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

let rec plus_general assoc = function
        | _, Tvar tvar -> true
        | Tconstr (s, tau_list), Tconstr (s', tau_list') -> s=s' && List.for_all (plus_general assoc) (List.combine tau_list tau_list')
        | Teffect t, Teffect t' -> plus_general assoc (t,t')
        | Tvar tvar, t -> 
                        begin match List.assoc_opt tvar assoc with
                        | None -> false
                        | Some (t') -> plus_general assoc (t',t)
                        end
        | Tint, Tint 
        | Tstring, Tstring 
	| Tbool, Tbool
	| Tunit, Tunit -> true
        | _,_ -> false
  

let rec resoud_instance_local_ou_global assoc (nom_i, t_list) = function
        | [] -> false
        | (nom_i', t_list') :: q -> 
                                plus_general assoc (Tconstr (nom_i, t_list), Tconstr (nom_i', t_list')) || resoud_instance_local_ou_global assoc (nom_i, t_list) q
        
let rec resoud_instance env (nom_i, t_list) = 
       (*on trouve une instance plus générale dans l'environnement local*)
      (*Ou on trouve une instance plus générale dans l'environnement global*)
        (*Ou on trouve un schema d'instance qui donne une instance plus générale que la notre*)
  if resoud_instance_local_ou_global env.vdecl (nom_i, t_list) env.instances 
           || resoud_instance_local_ou_global env.vdecl (nom_i, t_list) g_env.instances 
           || resoud_instance_schemas env (nom_i, t_list) g_env.schemas then
                   true
        else
                failwith "Impossible de resoudre l'instance"

and resoud_instance_schemas env (nom_i, t_list) = function
        | [] -> false
        | (i_list, (nom_i', t_list')) :: q -> if nom_i=nom_i' then
                List.for_all (resoud_instance env) i_list && t_list' = t_list
        else resoud_instance_schemas env (nom_i, t_list) q

       
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

let rec retire_assoc s = function
        | [] -> []
        | (s',x) :: q -> if s=s' then q else (s',x)::(retire_assoc s q)

let ajoute_contexte_decl s tau env =
        {instances=env.instances; vars=env.vars; vdecl=(s,tau)::(retire_assoc s env.vdecl)}

let rec ajoute_contexte_motif env = function
        | Preprocessing.Ast.PatternArgument p -> 
                        begin match p with
                        | PatargConstant _ -> env
                        | PatargIdent tvar ->{instances=env.instances; vars=tvar::env.vars; vdecl=env.vdecl}
                        | Pattern p' -> ajoute_contexte_motif env p'
                        end
        | Preprocessing.Ast.PatternConstructor (s, t_list) -> List.fold_left ajoute_contexte_motif env (List.map (fun p' -> Preprocessing.Ast.PatternArgument p') t_list)

(*vérifie que les variables d'une liste de variables ont des noms distincts*)
let rec noms_distincts_vars = function
        | [] -> true
        | s :: q -> not (List.mem s q) && noms_distincts_vars q

(*vérifie que les constructeurs d'une liste de constructeurs ont des noms distincts*)
let noms_distincts_constructors constructor_list = 
        noms_distincts_vars (List.map (fun (Preprocessing.Ast.Constructor (Name s,_ )) -> s) constructor_list)

let noms_distincts_et_nouveaux_fun defs =
        let defs' = List.map (fun (Preprocessing.Ast.Definition (s,_,_)) -> s) defs in
        noms_distincts_vars defs'
        && (let existants = List.map (fun (s,_,_,_) -> s) g_env.fonctions in 
        List.for_all (fun s -> not (List.mem s existants)) defs')

let rec sans_repetition_de_var = function
                | Preprocessing.Ast.PatternArgument p ->
                        begin match p with
                        | PatargConstant _ -> (true, [])
                        | PatargIdent i -> (true, [i])
                        | Pattern p' -> sans_repetition_de_var p'
                        end
        | Preprocessing.Ast.PatternConstructor (s, p_list) -> 
                        if List.length p_list = 0 then (true, [])
                        else
                                let tete, suite = (match p_list with
                                                        | x :: q -> (x,q))
                                in
                                let sans_repet, l = sans_repetition_de_var (PatternArgument tete) in
                                if sans_repet then
                                        let sans_repet', l' = sans_repetition_de_var (Preprocessing.Ast.PatternConstructor (s, suite)) in
                                        if sans_repet' then 
                                                (List.for_all (fun v -> not (List.mem v l')) l, l@l') 
                                        else
                                                (false, [])
                                else
                                        (false, [])

let rec est_exhaustif = function	
        | [], _ -> false
        | _, Tvar tvar -> false

        | Preprocessing.Ast.PatternArgument (PatargIdent i) :: q, _ -> true

        | PatternArgument (Pattern p) :: q, t -> est_exhaustif (p::q,t)

        | l, Teffect t -> est_exhaustif (l, Tconstr("Effect", [t]))

        | l, Tconstr (s',t_list) -> 
                        List.exists (fun p' -> match p' with
                                                | Preprocessing.Ast.PatternArgument (PatargIdent i) -> true
                                                | _ -> false) l
                        || (let pattern_s = List.filter (fun p' -> match p' with
                                                        | Preprocessing.Ast.PatternConstructor (s, p_list) -> s=s'
                                                        | _ -> false) l
                        in est_exhaustif_constr (pattern_s, t_list)
                        )
        | _,_ -> false

and est_exhaustif_constr = function
        | Preprocessing.Ast.PatternConstructor(_, []) :: q, [] -> List.for_all (fun (Preprocessing.Ast.PatternConstructor (s,l)) -> List.length l = 0) q
        | PatternConstructor(_, p::p_list) :: q as l, t::t_list -> 
                        est_exhaustif ((List.map (fun (Preprocessing.Ast.PatternConstructor(_,p'::p_list')) -> Preprocessing.Ast.PatternArgument p') l),t)
                        && est_exhaustif_constr (List.map (fun (Preprocessing.Ast.PatternConstructor(s,p'::p_list')) -> Preprocessing.Ast.PatternConstructor (s,p_list')) l, t_list)
        | _,_ -> false
                

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
        | Preprocessing.Ast.TypeIdent "Unit" -> Tunit
        | Preprocessing.Ast.TypeIdent "Boolean" -> Tbool
        | Preprocessing.Ast.TypeIdent "Int" -> Tint
        | Preprocessing.Ast.TypeIdent "String" -> Tstring
        | Preprocessing.Ast.TypeIdent s -> trouve_type_of_var_lenv s env
        | Preprocessing.Ast.TypeConstructor (Name "Unit", _) -> Tunit
        | Preprocessing.Ast.TypeConstructor (Name "Boolean", _) -> Tbool
        | Preprocessing.Ast.TypeConstructor (Name "Int", _) -> Tint
        | Preprocessing.Ast.TypeConstructor (Name "String", _) -> Tstring
        | Preprocessing.Ast.TypeConstructor (Name "Effect", [t]) -> Teffect (bf env t)
        | Preprocessing.Ast.TypeConstructor (Name s, t_list) -> 
                        begin match trouve_type_of_var_lenv s env with 
                                |Tconstr (_,verif_arite) ->
                                                if List.length t_list = List.length verif_arite then
                                                        Tconstr (s, List.map (bf env) t_list) 
                else
                        failwith "Nombre de types incompatible avec l'arité du constructeur"
                                | _ -> failwith "Le compilateur s'attendait à trouver un constructeur"
                        end

let rec type_motif env p = 
        let rec get_type = function
	| Preprocessing.Ast.PatternArgument p -> 
                        begin match p with
                        | PatargConstant (Boolean _) -> Tbool
                        | PatargConstant (Integer _) -> Tint
                        | PatargConstant (String _) -> Tstring
                        | PatargIdent i -> Tvar i
                        | Pattern p -> type_motif env p
                        end
        | Preprocessing.Ast.PatternConstructor (s, p_list) -> let nom, vars, t_list = trouve_explicit_constructor s in
                        let sigma = ref (List.map (fun v -> (v, Tvar v)) vars) in
                        List.iter (fun (p_i,tau_i) -> unifie_sub env sigma (tau_i, type_motif env p_i) ) (List.combine (List.map (fun p -> Preprocessing.Ast.PatternArgument p) p_list) t_list);
                        Tconstr (nom, List.map (fun (v,t) -> t) !sigma)
        in let sans_repet,_ = sans_repetition_de_var p in
        if sans_repet then get_type p
        else failwith "Repetition de variable dans le motif"


(*Fonction qui renvoie le type d'un expression*)
let rec type_expr env = function
        | Preprocessing.Ast.Constant c -> begin match c with
                | Preprocessing.Ast.Boolean b -> Tbool
                | Preprocessing.Ast.Integer i -> Tint
                | Preprocessing.Ast.String s -> Tstring
  end

        | Preprocessing.Ast.Variable x -> trouve_type_of_var_lenv x env
        | Preprocessing.Ast.UnaryOperation (Not, e) -> if type_expr env e = Tbool then Tbool else failwith "Not appliqué à une expression non booléenne"

        | Preprocessing.Ast.TypedExpression (e, tau) -> 
                  let res = type_expr env e in 
                  let tau' = bf env tau in
                  if res=tau' then
                        res 
                  else
                          failwith "Type suggéré mal formé"
  
        | Preprocessing.Ast.BinaryOperation (e1, binop, e2) -> 
                        begin match type_expr env e1, binop, type_expr env e2 with
                        | Tint, Plus, Tint | Tint, Minus, Tint | Tint, Times, Tint | Tint, Divide, Tint -> Tint 
                        | Tint, Equal, Tint | Tint, NotEqual, Tint | Tint, LessThan, Tint | Tint, LessThanOrEqual, Tint | Tint, GreaterThan, Tint | Tint, GreaterThanOrEqual, Tint -> Tbool 
                        | Tbool, Equal, Tbool | Tbool, NotEqual, Tbool | Tstring, Equal, Tstring | Tstring, NotEqual, Tstring -> Tbool
                        | Tbool, And, Tbool | Tbool, Or, Tbool -> Tbool
                        | Tstring, Concatenate, Tstring -> Tstring
                        | _,_,_ -> failwith "Opérandes ou opération invalide pour une opération binaire"
  end
        | Preprocessing.Ast.Conditional (e1, e2, e3) -> if type_expr env e1 = Tbool then
                let res = type_expr env e2 in
                if type_expr env e3 = res then 
                        res
                else 
                        failwith "Type de retour différents dans un if"
        else
                failwith "Opérande non booléenne dans un if"

        | Preprocessing.Ast.Do e_list -> 
                  if List.for_all (fun e -> type_expr env e = Teffect Tunit) e_list then 
                          Teffect Tunit
                  else
                          failwith "Les expressions du Do doivent être de type Effect Unit"

        | Preprocessing.Ast.Let (l, e) -> let env' = ref env in
  List.iter (fun (s,exp) -> let tau = type_expr !env' exp in env' := ajoute_contexte_decl s tau !env') l; type_expr !env' e

        | Preprocessing.Ast.ExplicitConstructor (s, expr_l) (*ident * expression list*) -> 
                        let nom, vars, t_list = trouve_explicit_constructor s in
                        let sigma = ref (List.map (fun v -> (v, Tvar v)) vars) in
                        List.iter (fun (e_i,tau_i) -> unifie_sub env sigma (tau_i, type_expr env e_i)) (List.combine expr_l t_list);
                        Tconstr (nom, List.map (fun (v,t) -> t) !sigma)

	| Preprocessing.Ast.FunctionCall (s, expr_l) (*ident * expression list*) ->
                        let s,vars,instances,t_list = trouve_decfun_g_env s in
                        let sigma = ref (List.map (fun v -> (v, Tvar v)) vars) in
                        let t_list', dernier = pop_dernier t_list in
                        List.iter (fun (e_i,tau_i) -> unifie_sub env sigma (tau_i, type_expr env e_i) ) (List.combine expr_l t_list');
                        List.iter (fun i -> let _ = resoud_instance env i in ()) instances;
                        applique_sub !sigma dernier

	| Preprocessing.Ast.Case (e, p_e_list) (*expression * (pattern * expression) list*) ->
                        let tau = type_expr env e in
                        let p_list = List.map (fun (p,_) -> p) p_e_list in
                        if List.for_all (fun p ->  type_motif env p = tau) p_list && est_exhaustif (p_list, tau) then
                                let p_1,e_1 = List.hd p_e_list in
                                let tau' = type_expr (ajoute_contexte_motif env p_1) e_1 in
                                if List.for_all (fun (p_i,e_i) -> type_expr (ajoute_contexte_motif env p_i) e_i = tau') p_e_list then
                                        tau'
                                else
                                        failwith "Une des expression n'est pas du bon type"
                        else
                                failwith "Un des motif n'est pas du bon type"

(*Fonction appelé pour enregistrer la declaration d'un type dans l'environnement global*)
let declaration_de_type = function
        | Preprocessing.Ast.Data (Name nom, vars, constructors) ->
                        let vars' = List.map (fun v -> match v with
                                                        |Preprocessing.Ast.TypeIdent s -> s
                                                        | _ -> "Un constructeur n'était pas attendu à cet endroit") vars in
                        if noms_distincts_vars vars' then
                                if noms_distincts_constructors constructors then
                                        g_env.datas <- (nom, vars', List.map (fun (Preprocessing.Ast.Constructor (Name c_name, t_list)) -> (c_name, List.map (bf {instances = []; vars = vars'; vdecl = List.map (fun v -> (v, Tast.Tvar v)) vars'}) t_list)) constructors) :: g_env.datas
                                else
                                        failwith "Il existe un conflit de noms sur les constructeurs de la declaration de type"
                        else
                                        failwith "Il existe un conflit de noms sur les variables de la declaration de type"


        | _ -> failwith "Mauvais argument pour declaration_de_type"


let declaration_de_class = function
        | Preprocessing.Ast.Class (Name name,vars,decls) ->
                let vars' = List.map (fun v -> match v with
                                                        |Preprocessing.Ast.TypeIdent s -> s
                                                        | _ -> "Un constructeur n'était pas attendu à cet endroit") vars in
                if noms_distincts_vars vars' then
                        if noms_distincts_et_nouveaux_fun decls then
                                let (env:Tast.env) = {instances = []; vars = vars'; vdecl = List.map (fun v -> (v, Tast.Tvar v)) vars'} in
                                let res = ref [] in
                                List.iter (
                                        fun (Preprocessing.Ast.TypeDeclaration (f,_,_,t_list)) -> 
                                                let to_add = (f,vars',[(name, List.map (fun v -> Tast.Tvar v) vars')], List.map (bf env) t_list) in
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
        let defs_names = List.map (fun (Preprocessing.Ast.Definition (s,_,_)) -> s) defs in
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
        | Preprocessing.Ast.Instance (t_list, defs) -> 
                        let Preprocessing.Ast.TypeConstructor (Name c_name, tau_list) = List.hd t_list in
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

let declaration_de_fonction = function
        | Preprocessing.Ast.TypeDeclaration (name,vars,instances,types) ->
                        let vars' = List.map (fun (Preprocessing.Ast.TypeIdent s) -> s) vars in
                        let typed_instances = List.map (fun (Tast.Tconstr x) -> x) (List.map (bf {instances=[]; vars=vars'; vdecl=(List.map (fun v -> (v, Tvar v)) vars')}) instances) in
                        let t_list = List.map (bf {instances=typed_instances; vars=vars'; vdecl=(List.map (fun v -> (v, Tvar v)) vars')}) types in
                        print_string name;
                        g_env.fonctions <- (name,vars',typed_instances,t_list)::g_env.fonctions;
                        ({instances=typed_instances; vars=vars'; vdecl=(List.map (fun v -> (v, Tvar v)) vars')},t_list)
        | _ -> failwith "On attendait ici une declaration de fonction"

let rec separe_defs_suite name l = match l with
        | [] -> ([],[])
        | Preprocessing.Ast.Definition (s,_,_) as d :: q when s=name -> 
                                let l1, l2 = separe_defs_suite name q in (d::l1,l2)

        | _ -> ([], l)

let rec definition_bf env tau_list = function
        | Preprocessing.Ast.Definition (s,p_list,e) ->
                        let p_list' = List.map (fun p -> type_motif env (PatternArgument p)) p_list in
                        let tau_list',tau = pop_dernier tau_list in
                        if List.for_all (plus_general env.vdecl) (List.combine p_list' tau_list') then
                                let env' = List.fold_left ajoute_contexte_motif env 
                                (List.map (fun p -> Preprocessing.Ast.PatternArgument p) p_list) in
                                (*On en profite pour vérifier que les motifs introduisent des variables distinctes*)
                                
                                if plus_general env'.vdecl (type_expr env' e,tau) then
                                        if noms_distincts_vars env'.vars then
                                                ()
                                        else
                                                failwith "Il existe un conflit entre les noms de variables introduites par les motifs"
                                else
                                        failwith "type de retour de la definition incorrecte"
                        else
                                failwith "Un des motifs de la definition est incorrecte"
                        
        | _ -> failwith "La fonction definition_bf attends une definition de fonction"

let rec type_declarations l = match l with
        | [] -> ()

        | Preprocessing.Ast.Data _ as d :: q -> declaration_de_type d; type_declarations q


        | Preprocessing.Ast.TypeDeclaration (name,_,_,_) as td :: q -> 
                        let env, tau_list = declaration_de_fonction td in
                        let definitions, suite_du_fichier = separe_defs_suite name q in 
                        (*Ici on devrait vérifier que
                        tous les motifs sont des variables, à l’exception d’une colonne au plus, qui est un filtrage exhaustif*)
                        (*let c = unique_non_variable definitions in*)
                        List.iter (definition_bf env tau_list) definitions;
                        type_declarations suite_du_fichier

        | Preprocessing.Ast.Class _ as c :: q -> declaration_de_class c; type_declarations q

        | Preprocessing.Ast.Instance _ as i :: q -> declaration_d_instance i; type_declarations q

        | Preprocessing.Ast.Definition _  :: q -> failwith "Il ne devrait pas y avoir de definition situé en dehors d'une declaration de fonction ou d'instance"

        
let file = function
        | Preprocessing.Ast.File (_,l) -> type_declarations l

(*
let rec print_type = function
	| Tint -> print_string " Tint "
	| Tstring -> print_string " Tstring "
	| Tbool -> print_string " Tbool "
	| Tunit -> print_string " Tunit "
        | Tvar tvar -> print_char ' '; print_string tvar; print_char ' '
        | Teffect t -> print_string " Teffect("; print_type t; print_string ") "
        | Tconstr (s, l) -> print_string s; List.iter print_type l

let _ = declaration_de_type (Ast.Data (Name "Foo", [TypeIdent "a"; TypeIdent "b"], [Constructor (Name "Nil", []); Constructor (Name "Foo", [TypeIdent "a"])]))
o*)
