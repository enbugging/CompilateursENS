open Tast
open Preprocessing
open TyperExpression
open PrettyPrinterBeta
open GestionEnv

let colonne_de_filtrage g_env l_env definitions =
        let rec scan_horizontal start_p end_p col = function
                | [] -> (false,-1)
                | p :: q -> begin match type_motif g_env l_env start_p end_p p with
                                | Tvar _ -> scan_horizontal start_p end_p (col+1) q
                                | _ -> 
                                                let b,_ = scan_horizontal start_p end_p (col+1) q in 
                                if b then
                                        raise (Error (start_p, end_p, "Plusieurs colonnes de filtrage dans une même ligne"))
                                else (true, col)
                                end
                | _ -> (false,-1)
                                
         in let rec scan_vertical = function
                | [] -> (false,-1)
                | Ast.Definition (_, p_list,e) :: q -> 
                                let p_list = List.map (fun p -> Ast.PatternArgument p) p_list in
                                let start_p, end_p = e.location in
                                let b,c = scan_horizontal start_p end_p 0 p_list in
                                if b then
                                        if List.for_all ( fun (Ast.Definition(_,p_list',_)) ->
                                                let p_list' = List.map (fun p -> Ast.PatternArgument p) p_list' in
                                                let b',c' = scan_horizontal start_p end_p 0 p_list' in
                                        (not b') || c'=c) q then
                                               (true, c) 
                                        else
                                                raise (Error (start_p, end_p, "Plusieurs colonnes de filtrage"))
                                else
                                        scan_vertical q
         in scan_vertical definitions

let filtrage_exhaustif definitions col tau = 
        let filtrage = List.map (fun (Ast.Definition (_, p_list,_)) -> List.nth p_list col) definitions in true


let rec pattern_distincts start_p end_p = function
        | [] -> ()
        | Ast.PatternArgument (PatargIdent x) as p :: q when x<>"_"-> if List.mem p q then raise (Error (start_p, end_p, "La variable de filtrage "^x^" apparait plusieurs fois")) else pattern_distincts start_p end_p q
        | _ :: q -> pattern_distincts start_p end_p q

let verification_definition g_env l_env vars tau_list tau_ret = function
        | Ast.Definition (Name(name, start_pos, end_pos), p_list, e) ->
                        let p_list = List.map (fun p -> Ast.PatternArgument p) p_list in
                        pattern_distincts start_pos end_pos p_list;
                        let sub = ref (List.map (fun x -> (x, QuantifTvar x)) vars) in
                        sub := List.append !sub (List.filter (fun (s,t) -> List.assoc_opt s !sub = None) l_env.vdecl);
                        List.iter (fun (p_i, tau_i) -> unifie_sub sub (start_pos, end_pos) ((type_motif g_env l_env start_pos end_pos p_i), tau_i)) (try List.combine p_list tau_list with _ -> raise (Error (start_pos, end_pos, "Mauvais nombre d'arguments")));

                        let l_env = {instances=l_env.instances; vars=l_env.vars; vdecl = !sub} in
                        unifie_sub sub (start_pos, end_pos) (type_expression g_env l_env e, tau_ret)

        | _ -> failwith "On attendait ici une definition de fonction"

let declaration_de_fonction g_env declaration definitions =
        match declaration with
        | Ast.TypeDeclaration (Name(name,start_pos,end_pos), vars, instances, tau_list) ->
                        if List.length definitions < 1 then
                                raise (Error (start_pos, end_pos, "La fonction "^name^" ne possède pas de definition"))
                        else
                                if existe_g_env_fonction name g_env then
                                        raise (Error (start_pos, end_pos, "La fonction "^name^" existe deja"))
                                else
                        let vars = List.map (fun (Ast.TypeIdent (Name(s,_,_))) -> s) vars in
                        let l_env = List.fold_left (fun env v -> ajoute_l_env_var v env) empty_env vars
                        in let instances' = List.map (fun i -> match i with
                                                        | Tvar i_name -> (i_name,[])
                                                        | Tconstr x -> x
                                                        ) (List.map (bf g_env l_env) instances) in
                        let tau_list' = List.map (bf g_env l_env) tau_list in
                        let l_env = List.fold_left (fun env i -> ajoute_l_env_instance i env) l_env instances' in
                        List.iter (fun t -> match t with 
                                                | Tconstr (x,_) -> if existe_g_env_without_error x g_env || existe_l_env x l_env then () else raise (Error (start_pos, end_pos, "Le type "^x^" devrait exister")) 
                                                | _ -> ()) tau_list';
                        let tau_list2, tau_ret = pop_dernier tau_list' in
                        List.iter (verification_definition (ajoute_g_env_fonction name vars instances' tau_list' g_env) l_env vars tau_list2 tau_ret) definitions;
                        let b, col = colonne_de_filtrage g_env l_env definitions in
                        if b then
                                if filtrage_exhaustif definitions col (List.nth tau_list col) then
                                        ajoute_g_env_fonction name vars instances' tau_list' g_env
                                else
                                        raise (Error (start_pos, end_pos, "Filtrage non exhaustif de la colonne "^(string_of_int col)))
                        else
                                ajoute_g_env_fonction name vars instances' tau_list' g_env
        | _ -> failwith "Il devrait s'agire d'une TypeDeclaration"
