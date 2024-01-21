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
        begin match (tau1,tau2) with
        | Tvar "_", _ -> ()
        | Tvar tvar, _ -> begin match List.assoc_opt tvar !sigma with
                        | Some(Tvar tvar') -> let l' = List.filter (fun (a,b) -> a<>tvar) !sigma in sigma := (tvar, tau2)::l'
                        | Some(Tconstr(tvar',_)) -> ()
                        | Some(x) -> unifie_sub sigma (start_p, end_p) (x,tau2)
                        | None -> let l' = List.filter (fun (a,b) -> a<>tvar) !sigma in sigma := (tvar, tau2)::l'         
        end
        | Tconstr (tvar,[]), _ -> begin match List.assoc_opt tvar !sigma with
                        | Some(Tvar tvar') -> let l' = List.filter (fun (a,b) -> a<>tvar) !sigma in sigma := (tvar, tau2)::l'
                        | Some(Tconstr(tvar',[])) -> ()
                        | Some(x) -> unifie_sub sigma (start_p, end_p) (x,tau2)
                        | None -> let l' = List.filter (fun (a,b) -> a<>tvar) !sigma in sigma := (tvar, tau2)::l'
        end
        | QuantifTvar _, _ -> ()
        | _, QuantifTvar tvar -> raise (Error (start_p, end_p, "Impossible d'unifier la variable de type universellement quantifiée "^tvar^" !"))
        | _, Tvar tvar -> begin match List.assoc_opt tvar !sigma with
                        | Some(Tvar tvar') -> let l' = List.filter (fun (a,b) -> a<>tvar) !sigma in sigma := (tvar, tau1)::l'
                        | Some(Tconstr(tvar',[])) -> ()
                        | Some(t') -> unifie_sub sigma (start_p, end_p) (tau1, t')
                        | None -> ()
        end
        | _, Tconstr (tvar,[]) -> begin match List.assoc_opt tvar !sigma with
                        | Some(Tvar tvar') -> let l' = List.filter (fun (a,b) -> a<>tvar) !sigma in sigma := (tvar, tau1)::l'
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

        | Tconstr (s,t_list), Tconstr (s',t_list') when s=s' -> List.iter (fun x -> try unifie_sub sigma (start_p, end_p) x with _ -> ()) (try List.combine t_list t_list' with _ -> raise (Error (start_p, end_p, "Arite differente sur deux constructeurs")))

        | _,_ -> raise (Error (start_p, end_p, "Expression du mauvais type\n"))
        end

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
        if trouve_l_env_instance l_env i || trouve_g_env_instance g_env i then
                ()
        else
                let i_list = trouve_g_env_schema_pour g_env start_p end_p i in
                if List.length i_list > 0 then
                        List.iter (resoud_instance g_env l_env start_p end_p) i_list
                else
                        raise (Error (start_p, end_p, "Impossible de résoudre l'instance "^i_name^" !"))

let rec type_motif g_env l_env start_p end_p = function
        | PatternArgument p -> begin match p with
                                | PatargConstant (Integer _) -> Tint
                                | PatargConstant (String _) -> Tstring
                                | PatargConstant (Boolean _) -> Tbool
                                | PatargIdent i -> begin match List.assoc_opt i l_env.vdecl with
                                                        | Some(t) -> t
                                                        | None -> if i.[0]<>'_' && Char.uppercase_ascii i.[0] = i.[0] then Tconstr(i, []) else Tvar i
                                end
                                | Pattern p' -> type_motif g_env l_env start_p end_p p'
                                end
        | PatternConstructor (name, p_list) -> 
                        let p_list' = List.map (fun p -> PatternArgument p) p_list in
                        let _ = List.iter (fun motif -> let _ = contient_deux_fois_la_meme_var start_p end_p motif in ()) p_list' in
                        let data_name, vars, tau_list = trouve_g_env_constructeur name g_env start_p end_p in
                        let sigma = ref (List.map (fun v -> (v, Tvar v)) vars) in

                        sigma := List.append !sigma (List.filter (fun (s,t) -> List.assoc_opt s !sigma = None) l_env.vdecl);
                        List.iter (fun (p_i,tau_i) -> unifie_sub sigma (start_p, end_p) ((type_motif g_env l_env start_p end_p p_i), tau_i)) (try List.combine p_list' tau_list with _ -> raise (Error (start_p, end_p, "Mauvais nombre d'arguments")));
                        Tconstr (data_name, substitution !sigma vars)


let rec unpack_pattern p = match p with
        | PatternArgument (Pattern p') -> unpack_pattern p'
        | _ -> p

        (*Les constructeurs d'arité 0 sont parsés comme des PatargIdent...*)
let is_ident_constr y = Char.uppercase_ascii y.[0] = y.[0]

let rec filtrage_exhaustif g_env l_env start_p end_p (tau, lst) = 
        match (tau, lst) with
        | _, [] -> false
        | Teffect t, l -> 
                        List.exists (fun x -> match x with
                                                | PatternArgument (PatargIdent y) -> not (is_ident_constr y) 
                                                | _ -> false
                                                ) l 
                        || filtrage_exhaustif g_env l_env start_p end_p (t, 
                                                        List.map (fun p -> match p with 
                                                                                | PatternConstructor("Effect",[t']) -> PatternArgument t'
                                                                                | _ -> failwith "Seuls des constructeurs effects peuvent apparaître dans cette liste") (List.filter (fun x -> match x with
                                                                                                                | PatternConstructor("Effect",_) -> true
                                                                                                                | _ -> false) l)
                                                        )
        | Tconstr (name, t_list), l -> 
                        List.exists (fun x -> match x with
                                                | PatternArgument (PatargIdent y) -> not (is_ident_constr y)
                                                | _ -> false
                                                ) l 
                        ||(existe_g_env_data name g_env && let data_name, vars, constructors = trouve_g_env_data name g_env start_p end_p in
                        (*On vérifie que tous les constructeurs apparaissent*)
                        let l = List.map unpack_pattern l in
                        let constructors_in_patterns = List.map (fun x -> match x with PatternConstructor (c,_) -> c | PatternArgument (PatargIdent x) -> if is_ident_constr x then x else "_" | _ -> "_") l in
                        List.for_all (fun y -> List.mem y constructors_in_patterns) (List.map (fun (x,_) -> x) constructors) 
                        (*On vérifie que pour chaque constructeur, pour chacun de ses arguments, il est filtré de manière exhaustive*)
                        && List.for_all (fun (x,t_lst) -> 
                                let col = ref 0 in
                                let filter = List.filter (fun p -> match p with PatternConstructor(c,_) -> c=x | (*PatternArgument (PatargIdent y) -> y=x |*) _ -> false) l in
                                List.for_all (fun t_col -> 
                                        let l' = List.map (fun (PatternConstructor(_,args)) -> PatternArgument (List.nth args !col)) filter in
                                        if filtrage_exhaustif g_env l_env start_p end_p (t_col, l') then 
                                                let _ = incr col in true 
                                        else false) t_lst) constructors
                        )
        | Tbool, l -> List.mem (PatternArgument (PatargConstant (Boolean false))) l && List.mem (PatternArgument (PatargConstant (Boolean true))) l
        | _,l -> List.exists (fun x -> match x with
                                                | PatternArgument (PatargIdent _) -> true
                                                | _ -> false
                                                ) l 

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

let rec type_expression g_env l_env expression = match expression with
        | {e=Constant c; location=(start_p,end_p)} -> 
                        begin match c with
                        | Integer _ -> TConstant (c, Tint)
                        | Boolean _ -> TConstant (c, Tbool)
                        | String _ -> TConstant (c, Tstring)
                        | Unit -> TConstant (Unit, Tunit)
                        end

        | {e=Variable x; location=(start_p,end_p)} ->
                        begin
                        try
                                TVariable (x, type_of_var_l_env x start_p end_p l_env)
                        with _ -> let res,_,_ = trouve_g_env_constructeur x g_env start_p end_p 
                                in TVariable (x, Tvar res)
                        end

	| {e=TypedExpression (exp, tau); location=(start_p,end_p)} ->
                        let tau' = bf g_env l_env tau in
                        let t_exp = type_expression g_env l_env exp in
                        if type_of_texpr t_exp = tau' then
                                TTypedExpression(t_exp, tau, tau')
                        else
                                raise (Error (start_p, end_p, "Le type suggéré ne correspond pas\n"));

	| {e=BinaryOperation (e1, binop, e2); location=(start_p,end_p)} ->
                        let t_e1 = type_expression g_env l_env e1 in
                        let t_e2 = type_expression g_env l_env e2 in
                        let tau1 = unpack g_env l_env (type_of_texpr t_e1) in
                        let tau2 = unpack g_env l_env (type_of_texpr t_e2) in
                        let res = 
                        begin match tau1, binop, tau2 with
                        | Tint, Plus, Tint | Tint, Minus, Tint | Tint, Times, Tint | Tint, Divide, Tint -> Tint 
                        | Tint, Equal, Tint | Tint, NotEqual, Tint | Tint, LessThan, Tint | Tint, LessThanOrEqual, Tint | Tint, GreaterThan, Tint | Tint, GreaterThanOrEqual, Tint -> Tbool 
                        | _,Equal,_ | _,NotEqual,_ when tau1 = tau2 -> Tbool
                        | Tbool, And, Tbool | Tbool, Or, Tbool -> Tbool
                        | Tstring, Concatenate, Tstring -> Tstring
                        | _,_,_ -> raise (Error (start_p, end_p, "Opérandes invalide pour cette opération binaire"))
                        end
                        in if tau1 = Tunit then TConstant (Boolean (binop = Equal), Tbool)
                        else
                                TBinaryOperation(set_new_type tau1 t_e1,binop,set_new_type tau2 t_e2, res)

	| {e=Conditional (e1, e2, e3); location=(start_p,end_p)} ->
                        let t_e1 = type_expression g_env l_env e1 in
                        let sub = ref [] in
                        unifie_sub sub (start_p,end_p) (type_of_texpr t_e1, Tbool);
                        let t_e2 = type_expression g_env l_env e2 in
                        let t_e3 = type_expression g_env l_env e3 in
                        unifie_sub sub (start_p,end_p) (type_of_texpr t_e3, type_of_texpr t_e2);
                        TConditional (t_e1, t_e2, t_e3, type_of_texpr t_e2)

        | {e=Do e_list; location=(start_p,end_p)} ->
                        let t_e_list = List.map (type_expression g_env l_env) e_list in
                        let sub = ref [] in
                        List.iter (fun expr -> 
                                unifie_sub sub (start_p, end_p) (type_of_texpr expr, Teffect Tunit)
                        ) t_e_list;
                        TDo t_e_list

	| {e=Let (assoc_list, exp); location=(start_p,end_p)} ->
                        let l_env' = ref l_env in
                        let assoc_list' = List.map (fun (x_i,e_i) -> 
                                existe_g_env x_i g_env start_p end_p;
                                let t_e_i = type_expression g_env !l_env' e_i in
                                let tau_i = type_of_texpr t_e_i in
                                l_env' := ajoute_l_env_assoc x_i tau_i !l_env';
                                (x_i,t_e_i)) assoc_list in
                        let res = type_expression g_env !l_env' exp in 
                        TLet (assoc_list', res, type_of_texpr res) 

        | {e=ExplicitConstructor (x, e_list); location=(start_p,end_p)} ->
                        let data_name, vars, tau_list = trouve_g_env_constructeur x g_env start_p end_p in
                        let sigma = ref (List.map (fun v -> (v, Tvar v)) vars) in
                        sigma := List.append !sigma (List.filter (fun (s,t) -> List.assoc_opt s !sigma = None) l_env.vdecl);
                        let t_e_list = List.map (fun (e_i,tau_i) -> let t_e_i = type_expression g_env l_env e_i in unifie_sub sigma e_i.location (type_of_texpr t_e_i, tau_i); t_e_i) (try List.combine e_list tau_list with _ -> raise (Error (start_p, end_p, "Mauvais nombre d'arguments"))) in

                        TExplicitConstructor(x,t_e_list, Tconstr (data_name, substitution !sigma vars))
                        
	| {e=FunctionCall (f, e_list); location=(start_p,end_p)} ->
                        let f,vars, instances, tau_list = trouve_g_env_fonction f g_env start_p end_p in
                        let tau_list, ret_type = pop_dernier tau_list in
                        let sigma = ref (List.map (fun v -> (v, Tvar v)) vars) in
                        sigma := List.append !sigma (List.filter (fun (s,t) -> List.assoc_opt s !sigma = None) l_env.vdecl);
                        let t_e_list = List.map (fun (e_i,tau_i) -> let t_e_i = type_expression g_env l_env e_i in unifie_sub sigma e_i.location (type_of_texpr t_e_i, tau_i); t_e_i) (try List.combine e_list tau_list with _ -> raise (Error (start_p, end_p, "Mauvais nombre d'arguments pour la fonction "^f^" !"))) in
                        List.iter (resoud_instance g_env l_env start_p end_p) instances;
                        TFunctionCall(f, instances, t_e_list, substitution_type !sigma ret_type)

	| {e=Case (exp, case_list); location=(start_p,end_p)} ->
                        let t_exp = type_expression g_env l_env exp in
                        let tau = type_of_texpr t_exp in
                        let p_1,e_1 = List.hd case_list in
                        let t_exp' = type_expression g_env (etend_l_env l_env p_1) e_1 in
                        let tau' = type_of_texpr t_exp' in
                        let t_case_list = List.map (fun (p_i,e_i) ->
                                let sigma = ref [] in
                                sigma := List.append !sigma (List.filter (fun (s,t) -> List.assoc_opt s !sigma = None) l_env.vdecl);
                                unifie_sub sigma e_i.location (type_motif g_env l_env start_p end_p p_1, tau);
                                let t_e_i = type_expression g_env (etend_l_env l_env p_i) e_i in
                                unifie_sub sigma e_i.location (type_of_texpr t_e_i, tau');
                                (p_i,t_e_i)) case_list in
                                if filtrage_exhaustif g_env l_env start_p end_p (tau, List.map (fun (p_i,_) -> p_i) case_list) then
                                        TCase (t_exp, t_case_list, tau')
                                else
                                        raise (Error (start_p, end_p, "filtrage non exhaustif"))
