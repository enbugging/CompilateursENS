open Tast
open Preprocessing.Ast
open PrettyPrinterBeta
open GestionEnv

let rec unpack g_env l_env tau = match tau with
        | Tvar tvar -> begin match List.assoc_opt tvar l_env.vdecl with
                                | Some(Tvar s) when s=tvar -> tau 
                                | Some(x) -> unpack g_env l_env x
                                | None -> tau
        end
        | Tconstr(tvar,[]) -> begin match List.assoc_opt tvar l_env.vdecl with
                                | Some(Tvar s) when s=tvar -> tau 
                                | Some(x) -> unpack g_env l_env x
                                | None -> tau
        end
        | _ -> tau

let rec unifie_sub sigma (start_p, end_p) (tau1,tau2) = 
        match (tau1,tau2) with
        | Tvar tvar, _ -> begin match List.assoc_opt tvar !sigma with
                        | Some(Tvar tvar) -> sigma := (tvar, tau2)::(List.filter (fun (a,b) -> a<>tvar) !sigma)
                        | Some(x) -> unifie_sub sigma (start_p, end_p) (x,tau2)
                        | None -> sigma := (tvar, tau2)::(List.filter (fun (a,b) -> a<>tvar) !sigma)
        end
        | Tconstr (tvar,[]), _ -> begin match List.assoc_opt tvar !sigma with
                        | Some(Tvar tvar) -> sigma := (tvar, tau2)::(List.filter (fun (a,b) -> a<>tvar) !sigma)
                        | Some(x) -> unifie_sub sigma (start_p, end_p) (x,tau2)
                        | None -> sigma := (tvar, tau2)::(List.filter (fun (a,b) -> a<>tvar) !sigma)
        end
        | QuantifTvar _, _ -> ()
        | _, QuantifTvar tvar -> raise (Error (start_p, end_p, "Impossible d'unifier la variable de type universellement quantifiée "^tvar^" !"))
        | _, Tvar tvar -> begin match List.assoc_opt tvar !sigma with
                        | Some(Tvar tvar) -> sigma := (tvar, tau1)::(List.filter (fun (a,b) -> a<>tvar) !sigma)
                        | Some(t') -> unifie_sub sigma (start_p, end_p) (tau1, t')
                        | None -> ()
        end
        | _, Tconstr (tvar,[]) -> begin match List.assoc_opt tvar !sigma with
                        | Some(Tvar tvar) -> sigma := (tvar, tau1)::(List.filter (fun (a,b) -> a<>tvar) !sigma)
                        | Some(t') -> unifie_sub sigma (start_p, end_p) (tau1, t')
                        | None -> ()
        end

        | Tint, Tint -> ()
        | Tstring, Tstring -> ()
        | Tbool, Tbool -> ()
        | Tunit, Tunit -> ()
        | Teffect t, Teffect t' -> unifie_sub sigma (start_p, end_p) (t,t')
        | Tconstr ("Effect", [t]), Teffect t' -> unifie_sub sigma (start_p, end_p) (t,t')
        | Teffect t, Tconstr ("Effect", [t']) -> unifie_sub sigma (start_p, end_p) (t,t')
        | Tconstr (s,t_list), Tconstr (s',t_list') when s=s' -> List.iter (unifie_sub sigma (start_p, end_p)) (try List.combine t_list t_list' with _ -> raise (Error (start_p, end_p, "Arite differente sur deux constructeurs")))
        | _,_ -> raise (Error (start_p, end_p, "Expression du mauvais type\n"))

let rec bf g_env env = function
        | TypeIdent (Name (s,start_p,end_p)) -> 
                        begin
                        try 
                                let res = type_of_var_l_env s start_p end_p env in
                                res
                        with _ ->
                                        let res,_,_ = trouve_g_env_data s g_env start_p end_p in (Tvar res)
                        end
        | TypeConstructor (Name (name,start_p,end_p), t_list) ->
                        try let _,_,t_l = trouve_g_env_constructeur name g_env start_p end_p in
                        if List.length t_l = List.length t_list then
                                try Tconstr (name, List.map (bf g_env env) t_list)
                                with _ -> raise (Error (start_p, end_p, "Unable to type "^name^" !"))
                        else
                                raise Not_found (*l'exception Not_found n'est pas appropriée mais il en fallait bien une*)
                        with    | Not_found -> raise (Error (start_p, end_p, "Bad arity for "^name^" !"))
                                | _ -> try Tconstr (name, List.map (bf g_env env) t_list)
                                        with _ -> raise (Error (start_p, end_p, "Unable to type "^name^" !"))

let rec resoud_instance g_env l_env start_p end_p i =
        (*TODO Il faudrait peut-être utiliser sigma ?*)
        let i_name, tau_list = i in
        print_string ("On cherche à résoudre l'instance "^i_name);
        if trouve_l_env_instance l_env i || trouve_g_env_instance g_env i then
                ()
        else
                let i_list = trouve_g_env_schema_pour g_env start_p end_p i in
                List.iter (resoud_instance g_env l_env start_p end_p) i_list

let rec type_motif g_env l_env start_p end_p = function
        | PatternArgument p -> begin match p with
                                | PatargConstant (Integer _) -> Tint
                                | PatargConstant (String _) -> Tstring
                                | PatargConstant (Boolean _) -> Tbool
                                | PatargIdent i -> Tvar i
                                | Pattern p' -> type_motif g_env l_env start_p end_p p'
                                end
        | PatternConstructor (name, p_list) -> 
                        let p_list' = List.map (fun p -> PatternArgument p) p_list in
                        let _ = List.iter (fun motif -> contient_deux_fois_la_meme_var start_p end_p motif; ()) p_list' in
                        let data_name, vars, tau_list = trouve_g_env_constructeur name g_env start_p end_p in
                        let sigma = ref (List.map (fun v -> (v, Tvar v)) vars) in

                        sigma := List.append !sigma (List.filter (fun (s,t) -> List.assoc_opt s !sigma = None) l_env.vdecl);
                        List.iter (fun (p_i,tau_i) -> unifie_sub sigma (start_p, end_p) ((type_motif g_env l_env start_p end_p p_i), tau_i)) (try List.combine p_list' tau_list with _ -> raise (Error (start_p, end_p, "Mauvais nombre d'arguments")));
                        Tconstr (data_name, substitution !sigma vars)

let rec filtrage_exhaustif l_env = function
        | _, [] -> false
        | Teffect t, l -> 
                        List.exists (fun x -> match x with
                                                | PatternArgument (PatargIdent _) -> true
                                                | _ -> false
                                                ) l 
                        || filtrage_exhaustif l_env (t, 
                                                        List.map (fun (PatternConstructor("Effect",[t'])) -> PatternArgument t') (List.filter (fun x -> match x with
                                                                                                                | PatternConstructor("Effect",_) -> true
                                                                                                                | _ -> false) l)
                                                        )
        | Tconstr (name, t_list), l -> 
                        List.exists (fun x -> match x with
                                                | PatternArgument (PatargIdent _) -> true
                                                | _ -> false
                                                ) l 
                        || (let candidats = ref (List.map (fun (PatternConstructor(_,p_list)) -> p_list) (List.filter (fun x -> begin match x with
                                                                | PatternConstructor(name, p_list) -> true
                                                                | _ -> false
                                                                end) l)) in
                                let t_list_ref = ref t_list in
                                let res = ref true in
                                for i=1 to List.length t_list do
                                        let premiers, liste_des_restes = list_of_premiers !candidats in
                                        let premier, reste = pop_premier !t_list_ref in
                                        t_list_ref := reste;
                                        candidats := liste_des_restes;
                                        if not (filtrage_exhaustif l_env (premier, List.map (fun p -> PatternArgument p) premiers)) then
                                                res := false;
                                done;
                                !res
                        )
        | _,l -> List.exists (fun x -> match x with
                                                | PatternArgument (PatargIdent _) -> true
                                                | _ -> false
                                                ) l 

let rec type_expression g_env l_env = function
        | {e=Constant c; location=(start_p,end_p)} -> 
                        begin match c with
                        | Integer _ -> Tint
                        | Boolean _ -> Tbool
                        | String _ -> Tstring
                        end

        | {e=Variable x; location=(start_p,end_p)} ->
                        begin
                        try
                                type_of_var_l_env x start_p end_p l_env
                        with _ -> let res,_,_ = trouve_g_env_constructeur x g_env start_p end_p 
                                in (Tvar res)
                        end

	| {e=TypedExpression (exp, tau); location=(start_p,end_p)} ->
                        let tau' = bf g_env l_env tau in
                        if type_expression g_env l_env exp = tau' then
                                tau'
                        else
                                raise (Error (start_p, end_p, "Le type suggéré ne correspond pas\n"));

	| {e=BinaryOperation (e1, binop, e2); location=(start_p,end_p)} ->
                        let tau1 = unpack g_env l_env (type_expression g_env l_env e1) in
                        let tau2 = unpack g_env l_env (type_expression g_env l_env e2) in
                        begin match tau1, binop, tau2 with
                        | Tint, Plus, Tint | Tint, Minus, Tint | Tint, Times, Tint | Tint, Divide, Tint -> Tint 
                        | Tint, Equal, Tint | Tint, NotEqual, Tint | Tint, LessThan, Tint | Tint, LessThanOrEqual, Tint | Tint, GreaterThan, Tint | Tint, GreaterThanOrEqual, Tint -> Tbool 
                        | _,Equal,_ | _,NotEqual,_ when tau1 = tau2 -> Tbool
                        | Tbool, And, Tbool | Tbool, Or, Tbool -> Tbool
                        | Tstring, Concatenate, Tstring -> Tstring
                        | _,_,_ -> raise (Error (start_p, end_p, "Opérandes invalide pour cette opération binaire"))
                        end

	| {e=Conditional (e1, e2, e3); location=(start_p,end_p)} ->
                        if type_expression g_env l_env e1 = Tbool then
                                let tau = type_expression g_env l_env e2 in
                                let sub = ref [] in
                                unifie_sub sub (start_p,end_p) (type_expression g_env l_env e3, tau);
                                tau
                        else
                                raise (Error (start_p, end_p, "La première opérande du If n'est pas de type Boolean"))

        | {e=Do e_list; location=(start_p,end_p)} ->
                        let sub = ref [] in
                        List.iter (fun exp -> 
                                unifie_sub sub (start_p, end_p) (type_expression g_env l_env exp, Teffect Tunit)
                        ) e_list;
                        Teffect Tunit

	| {e=Let (assoc_list, exp); location=(start_p,end_p)} ->
                        let l_env' = ref l_env in
                        List.iter (fun (x_i,e_i) -> 
                                existe_g_env x_i g_env start_p end_p;
                                let tau_i = type_expression g_env !l_env' e_i in
                                l_env' := ajoute_l_env_assoc x_i tau_i !l_env') assoc_list;
                        type_expression g_env !l_env' exp

        | {e=ExplicitConstructor (x, e_list); location=(start_p,end_p)} ->
                        let data_name, vars, tau_list = trouve_g_env_constructeur x g_env start_p end_p in
                        let sigma = ref (List.map (fun v -> (v, Tvar v)) vars) in
                        sigma := List.append !sigma (List.filter (fun (s,t) -> List.assoc_opt s !sigma = None) l_env.vdecl);
                        List.iter (fun (e_i,tau_i) -> unifie_sub sigma e_i.location ((type_expression g_env l_env e_i), tau_i)) (try List.combine e_list tau_list with _ -> raise (Error (start_p, end_p, "Mauvais nombre d'arguments")));

                        Tconstr (data_name, substitution !sigma vars)

	| {e=FunctionCall (f, e_list); location=(start_p,end_p)} ->
                        let f,vars, instances, tau_list = trouve_g_env_fonction f g_env start_p end_p in
                        let tau_list, ret_type = pop_dernier tau_list in
                        let sigma = ref (List.map (fun v -> (v, Tvar v)) vars) in
                        sigma := List.append !sigma (List.filter (fun (s,t) -> List.assoc_opt s !sigma = None) l_env.vdecl);
                        List.iter (fun (e_i,tau_i) -> unifie_sub sigma e_i.location ((type_expression g_env l_env e_i), tau_i)) (try List.combine e_list tau_list with _ -> raise (Error (start_p, end_p, "Mauvais nombre d'arguments pour la fonction "^f^" !")));
                        List.iter (resoud_instance g_env l_env start_p end_p) instances;
                        substitution_type !sigma ret_type

	| {e=Case (exp, case_list); location=(start_p,end_p)} ->
                        let tau = type_expression g_env l_env exp in
                        let p_1,e_1 = List.hd case_list in
                        let tau' = type_expression g_env (etend_l_env l_env p_1) e_1 in
                        List.iter (fun (p_i,e_i) ->
                                let sigma = ref [] in
                                sigma := List.append !sigma (List.filter (fun (s,t) -> List.assoc_opt s !sigma = None) l_env.vdecl);
                                unifie_sub sigma e_i.location (type_motif g_env l_env start_p end_p p_1, tau);
                                unifie_sub sigma e_i.location (type_expression g_env (etend_l_env l_env p_i) e_i, tau')) case_list;
                                if filtrage_exhaustif l_env (tau, List.map (fun (p_i,_) -> p_i) case_list) then
                                        tau'
                                else
                                        raise (Error (start_p, end_p, "filtrage non exhaustif"))
