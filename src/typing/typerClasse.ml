open Tast
open Preprocessing
open TyperExpression
open GestionEnv

let rec noms_distincts = function
	| [] -> true
	| s :: q -> (not (List.mem s q)) && noms_distincts q

let declaration_de_classe g_env = function
	| Ast.Class (Name (c_name, start_p, end_p), vars, fonctions) ->
                        if existe_g_env_classe c_name g_env then
                                raise (Error (start_p, end_p, "La classe "^c_name^" existe deja"))
                        else
		let vars = List.map (fun p -> match p with Ast.TypeIdent (Name (s,_,_)) -> s | _ -> failwith "Cas impossible") vars in
		if not (noms_distincts vars) then
			raise (Error (start_p, end_p, "Les variables n'ont pas des noms distincts\n"))
		else
			let noms_fonctions = List.map (fun p -> match p with Ast.TypeDeclaration (Name (f,_,_),_,_,_) -> f | _ -> failwith "Cas impossible") fonctions in
			if not (noms_distincts noms_fonctions) then
				raise (Error (start_p,end_p, "Les fonctions n'ont pas des noms distincts\n"))
			else
				if List.exists (fun s ->
					List.mem s (List.map (fun (f,_,_,_) -> f) g_env.fonctions)) 
					noms_fonctions then
						raise (Error (start_p,end_p, "Les fonctions n'ont pas des noms nouveaux\n"))
				else
					let l_env = 
						if List.length vars > 0 then
							List.fold_left (fun env v -> ajoute_l_env_var v env) empty_env vars
						else empty_env in

                                        let g_env = ajoute_g_env_class c_name vars (List.map (fun p -> match p with Ast.TypeDeclaration (Name (f,_,_), [], [], t_list) -> (f, vars, [(c_name, List.map (fun v -> Tvar v) vars)], List.map (bf g_env l_env) t_list) | _ -> failwith "Cas impossible") fonctions) g_env in
				        let g_env = List.fold_left (
							fun g_env_acc p -> match p with Ast.TypeDeclaration (Name (f,start_p,end_p), [], [], t_list) ->
								ajoute_g_env_fonction f vars [(c_name, List.map (fun v -> Tvar v) vars)] (List.map (bf g_env l_env) t_list) g_env_acc | _ -> failwith "Cas impossible"
						) g_env fonctions
                                        in g_env
                                        (*in ajoute_g_env_instance [] (c_name, List.map (fun v -> Tvar v) vars) g_env*)
    | _ -> failwith "Mauvais argument passé à declaration_de_classe"
