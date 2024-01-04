open Tast
open Preprocessing
open TyperExpression
open GestionEnv

let rec noms_distincts = 
	function
	| [] -> true
	| s :: q -> (not (List.mem s q)) && noms_distincts q

let declaration_de_type g_env = function
                | Ast.Data (Name (name, start_p, end_p), vars, c_list) -> 
                                if existe_g_env_data name g_env then
                                        raise (Error (start_p, end_p, "Le type "^name^" existe deja"))
                                else
		let vars = List.map (fun (Ast.TypeIdent (Name (s,_,_))) -> s) vars in
		if not (noms_distincts vars) then raise (Error (start_p, end_p, "Variables cannot have the same name\n"))
		else
			if not (noms_distincts (List.map (fun (Ast.Constructor (Name (x,_,_),_)) -> x) c_list)) then
				raise (Error (start_p, end_p, "Constructions cannot have the same name\n"))
		else
			(*On construit la liste des constructeurs typés*)
			let to_add = ref [] in
			let l_env = ajoute_l_env_var name (List.fold_left (fun env v -> ajoute_l_env_var v env) empty_env vars) in
			List.iter (fun (Ast.Constructor (Name (c_name,start_p,end_p), t_list)) -> 
                                if existe_g_env_constructor c_name g_env then
                                        raise (Error (start_p, end_p, "Le constructeur "^c_name^" existe deja"))
                                else
				to_add := (c_name, List.map (bf g_env l_env) t_list) :: !to_add
				) c_list;
                        ajoute_g_env_data name vars !to_add g_env
	| _ -> failwith "Typer error : declaration_de_type"
