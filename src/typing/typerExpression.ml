open Tast
open Preprocessing.Ast

let rec bf env = function
        | TypeIdent (Name (s,start_p,end_p)) -> 
                        type_of_var_l_env s start_p end_p env
        | TypeConstructor (Name (name,start_p,end_p), t_list) ->
                        try Tconstr (name, List.map (bf env) t_list)
                        with _ -> raise (Error (start_p, end_p))

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
                        Tunit
        | {e=ExplicitConstructor (x, e_list); location=(start_p,end_p)} -> Tunit
	| {e=FunctionCall (f, e_list); location=(start_p,end_p)} -> Tunit
	| {e=Case (exp, case_list); location=(start_p,end_p)} -> Tunit

 
