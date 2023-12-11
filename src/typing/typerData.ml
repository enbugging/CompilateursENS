open Tast
open Preprocessing
open TyperExpression

let rec noms_distincts = function
        | [] -> true
        | s :: q -> (not (List.mem s q)) && noms_distincts q

let declaration_de_type g_env = function
        | Ast.Data (Name (name,start_p,end_p), vars, c_list) ->
                        let vars = List.map (fun (Ast.TypeIdent (Name (s,_,_))) -> s) vars in
                        if not (noms_distincts vars) then
                                let _ = print_string "Les variables n'ont pas des noms distincts\n" in raise (Error (start_p,end_p))
                        else
                                if not (noms_distincts (List.map (fun (Ast.Constructor (Name (x,_,_),_)) -> x) c_list)) then
                                        let _ = print_string "Les constructeurs n'ont pas des noms distincts\n" in raise (Error (start_p,end_p))
                        else
                                (*On construit la liste des constructeurs typés*)
                                let to_add = ref [] in
                                let l_env = List.fold_left (fun env v -> ajoute_l_env_var v env) empty_env vars in
                                List.iter (fun (Ast.Constructor (Name (c_name,start_p,end_p), t_list)) -> 
                                        to_add := (c_name, List.map (bf l_env) t_list) :: !to_add
                                        ) c_list;
                        ajoute_g_env_data name vars !to_add g_env

        | _ -> failwith "Erreur de comportement du typer"