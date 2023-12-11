open Tast
open Preprocessing
open TyperExpression

let rec noms_distincts = function
        | [] -> true
        | s :: q -> (not (List.mem s q)) && noms_distincts q

let declaration_de_classe g_env = function
        | Ast.Class (Name (c_name, start_p, end_p), vars, fonctions) ->
                let vars = List.map (fun (Ast.TypeIdent (Name (s,_,_))) -> s) vars in
                if not (noms_distincts vars) then
                                let _ = print_string "Les variables n'ont pas des noms distincts\n" in raise (Error (start_p,end_p))
                else
                        let noms_fonctions = List.map (fun (Ast.TypeDeclaration (Name (f,_,_),_,_,_)) -> f) fonctions in
                        if not (noms_distincts noms_fonctions
                        
                        ) then
                                let _ = print_string "Les fonctions n'ont pas des noms distincts\n" in raise (Error (start_p,end_p))
                        else
                                (*Il reste à vérifier que les noms des fonctions ne sont pas déjà utilisés*)
                                if List.exists (fun s -> 
                                        List.mem s (List.map (fun (f,_,_,_) -> f) g_env.fonctions)) 
                                noms_fonctions then
                                        let _ = print_string "Les fonctions n'ont pas des noms distincts\n" in raise (Error (start_p,end_p))
                                else
                                        let l_env = List.fold_left (fun env v -> ajoute_l_env_var v env) empty_env vars in
                                        List.fold_left (
                                                fun g_env_acc (Ast.TypeDeclaration (Name (f,start_p,end_p), [], [], t_list)) ->
                                                        ajoute_g_env_fonction f vars [(c_name, List.map (fun v -> Tvar v) vars)] (List.map (bf l_env) t_list) g_env_acc
                                                        
                                        ) g_env fonctions
        | _ -> failwith "Mauvais argument passé à declaration_de_classe"
