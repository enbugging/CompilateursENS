open Tast
open Preprocessing.Ast
open PrettyPrinterBeta
open GestionEnv

let rec unifie_sub sigma (start_p, end_p) = function
        | Tvar tvar, t -> sigma := (tvar, t)::(List.filter (fun (a,b) -> a<>tvar) !sigma)
        | _, Tvar _ -> ()
        | Tint, Tint -> ()
        | Tstring, Tstring -> ()
        | Tbool, Tbool -> ()
        | Tunit, Tunit -> ()
        | Teffect t, Teffect t' -> unifie_sub sigma (start_p, end_p) (t,t')
        | Tconstr (_,t_list), Tconstr (_,t_list') -> List.iter (unifie_sub sigma (start_p, end_p)) (List.combine t_list t_list')
        | _,_ -> print_string "Expression du mauvais type\n"; raise (Error (start_p, end_p))

let rec bf env = function
        | TypeIdent (Name (s,start_p,end_p)) -> 
                        type_of_var_l_env s start_p end_p env
        | TypeConstructor (Name (name,start_p,end_p), t_list) ->
                        try Tconstr (name, List.map (bf env) t_list)
                        with _ -> raise (Error (start_p, end_p))

let rec resoud_instance g_env l_env start_p end_p i =
        let i_name, tau_list = i in
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
                        (*TODO Vérifier qu'aucun motif ne contient deux fois la même variable*)
                        let p_list' = List.map (fun p -> PatternArgument p) p_list in
                        let _ = List.iter (fun motif -> contient_deux_fois_la_meme_var start_p end_p motif; ()) p_list' in
                        (*Trouver une data declaration dont un des constructeur est sigma compatible pour un certain sigma*)
                                let data_name, vars, tau_list = trouve_g_env_constructeur name g_env start_p end_p in
                                let sigma = ref (List.map (fun v -> (v, Tvar v)) vars) in
                                List.iter (fun (p_i,tau_i) -> unifie_sub sigma (start_p, end_p) ((type_motif g_env l_env start_p end_p p_i), tau_i)) (List.combine p_list' tau_list);
                                Tconstr (data_name, substitution !sigma vars)

let rec filtrage_exhaustif l_env =true
        (*function
        | _, [] -> false
        | Tvar t, l -> filtrage_exhaustif l_env (type_of_var_l_env t, l)
        | Teffect t, l -> filtrage_exhaustif l_env (type_of_var_l_env t, 
                                                        List.map (fun (Teffect t') -> t') (List.filter (fun x -> match x with
                                                                                                                | Tconstr _ -> true
                                                                                                                | _ -> false) l)
                                                        )
        | Tconstr (name, t_list), l -> true (*TODO*)
        | _,_ -> false
        *)

let rec type_expression g_env l_env = function
        | {e=Constant c; location=(start_p,end_p)} -> 
                        begin match c with
                        | Integer _ -> Tint
                        | Boolean _ -> Tbool
                        | String _ -> Tstring
                        end

        | {e=Variable x; location=(start_p,end_p)} ->
                        type_of_var_l_env x start_p end_p l_env

	| {e=TypedExpression (exp, tau); location=(start_p,end_p)} ->
                        let tau' = bf l_env tau in
                        if type_expression g_env l_env exp = tau' then
                                tau'
                        else
                                let _ = print_string "Le type suggéré ne correspond pas\n" in
                                raise (Error (start_p, end_p));

	| {e=BinaryOperation (e1, binop, e2); location=(start_p,end_p)} ->
                        begin match type_expression g_env l_env e1, binop, type_expression g_env l_env e2 with
                        | Tint, Plus, Tint | Tint, Minus, Tint | Tint, Times, Tint | Tint, Divide, Tint -> Tint 
                        | Tint, Equal, Tint | Tint, NotEqual, Tint | Tint, LessThan, Tint | Tint, LessThanOrEqual, Tint | Tint, GreaterThan, Tint | Tint, GreaterThanOrEqual, Tint -> Tbool 
                        | Tbool, Equal, Tbool | Tbool, NotEqual, Tbool | Tstring, Equal, Tstring | Tstring, NotEqual, Tstring -> Tbool
                        | Tbool, And, Tbool | Tbool, Or, Tbool -> Tbool
                        | Tstring, Concatenate, Tstring -> Tstring
                        | _,_,_ -> let _ = print_string "Opérandes invalide pour cette opération binaire" in raise (Error (start_p, end_p))
                        end

	| {e=Conditional (e1, e2, e3); location=(start_p,end_p)} ->
                        if type_expression g_env l_env e1 = Tbool then
                                let tau = type_expression g_env l_env e2 in
                                if type_expression g_env l_env e3 = tau then
                                        tau
                                else
                                        let _ = print_string "Le deux expressions de retour du If doivent etre de meme type" in
                                        raise (Error (start_p,end_p))
                        else
                                let _ = print_string "La première opérande du If n'est pas de type Boolean" in
                                raise (Error (start_p, end_p))

        | {e=Do e_list; location=(start_p,end_p)} ->
                        List.iter (fun exp -> 
                                if type_expression g_env l_env exp = Teffect Tunit then
                                        ()
                                else
                                        let _ = print_string "L'expression devrait avoir le type Effect Unit" in
                                        raise (Error exp.location)
                        ) e_list;
                        Teffect Tunit

	| {e=Let (assoc_list, exp); location=(start_p,end_p)} ->
                        let l_env' = ref l_env in
                        List.iter (fun (x_i,e_i) -> 
                                let tau_i = type_expression g_env !l_env' e_i in
                                l_env' := ajoute_l_env_assoc x_i tau_i !l_env') assoc_list;
                        type_expression g_env !l_env' exp

        | {e=ExplicitConstructor (x, e_list); location=(start_p,end_p)} ->
                        let data_name, vars, tau_list = trouve_g_env_constructeur x g_env start_p end_p in
                        let sigma = ref (List.map (fun v -> (v, Tvar v)) vars) in
                        List.iter (fun (e_i,tau_i) -> unifie_sub sigma e_i.location ((type_expression g_env l_env e_i), tau_i)) (List.combine e_list tau_list);
                        Tconstr (data_name, substitution !sigma vars)

	| {e=FunctionCall (f, e_list); location=(start_p,end_p)} ->
                        let f,vars, instances, tau_list = trouve_g_env_fonction f g_env start_p end_p in
                        let tau_list, ret_type = pop_dernier tau_list in
                        let sigma = ref (List.map (fun v -> (v, Tvar v)) vars) in
                        List.iter (fun (e_i,tau_i) -> unifie_sub sigma e_i.location ((type_expression g_env l_env e_i), tau_i)) (List.combine e_list tau_list);
                        List.iter (resoud_instance g_env l_env start_p end_p) instances;
                        substitution_type !sigma ret_type

	| {e=Case (exp, case_list); location=(start_p,end_p)} ->
                        let tau = type_expression g_env l_env exp in
                        let p_1,e_1 = List.hd case_list in
                        let tau' = type_expression g_env (etend_l_env l_env p_1) e_1 in
                        if List.for_all (fun (p_i,e_i) ->
                                type_motif g_env l_env start_p end_p p_1 = tau && type_expression g_env (etend_l_env l_env p_i) e_i = tau') case_list then
                                if filtrage_exhaustif (case_list,tau) then
                                        tau'
                                else
                                        let _ = print_string "filtrage non exhaustif" in
                                        raise (Error (start_p, end_p))
                        else
                                let _ = print_string "Une des expressions du filtrage n'est pas du bon type" in
                                raise (Error (start_p, end_p))

