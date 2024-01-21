open Tast
open Preprocessing
open TyperExpression
open PrettyPrinterBeta
open GestionEnv

(*Fonction pour détecter la présence d'une colonne de filtrage et la renvoyer le cas échéant*)
let colonne_de_filtrage g_env l_env definitions =
        let rec scan_horizontal start_p end_p col = function
                | [] -> (false,-1)
                | p :: q -> begin match type_motif g_env l_env start_p end_p p with
                                | Tvar t -> 
                                        let b,c = scan_horizontal start_p end_p (col+1) q in
                                        if existe_g_env_constructor t g_env then
                                                if b then
                                                        raise (Error (start_p, end_p, "Plusieurs colonnes de filtrage dans une même ligne"))
                                                else (true, col)
                                        else
                                                (b,c) 

                                | _ -> 
                                                let b,_ = scan_horizontal start_p end_p (col+1) q in 
                                if b then
                                        raise (Error (start_p, end_p, "Plusieurs colonnes de filtrage dans une même ligne"))
                                else (true, col)
                                end
                                
         in let rec scan_vertical = function
                | [] -> (false,-1)
                | Ast.Definition (_, p_list,e) :: q -> 
                                let p_list = List.map (fun p -> Ast.PatternArgument p) p_list in
                                let start_p, end_p = e.location in
                                let b,c = scan_horizontal start_p end_p 0 p_list in
                                if b then
                                        if List.for_all ( fun p -> match p with Ast.Definition(_,p_list',_) ->
                                                let p_list' = List.map (fun p -> Ast.PatternArgument p) p_list' in
                                                let b',c' = scan_horizontal start_p end_p 0 p_list' in
                                        (not b') || c'=c | _ -> failwith "Cas impossible") q then
                                               (true, c) 
                                        else
                                                raise (Error (start_p, end_p, "Plusieurs colonnes de filtrage"))
                                else
                                        scan_vertical q
                | _ -> failwith "Cas impossible"
         in scan_vertical definitions


(*Fonction pour vérifier que les motifs introduisent des variables différentes*)
let rec pattern_distincts start_p end_p p_list = 
        let vars = ref [] in
        let rec ajoute_idents_pattern = function
                | Ast.PatternArgument p -> ajoute_idents_patarg p 
                | PatternConstructor (_,l) -> List.iter ajoute_idents_patarg l

        and ajoute_idents_patarg = function
                | PatargConstant _ -> ()
                | PatargIdent "_" -> ()
                | PatargIdent x -> vars := x::!vars
                | Pattern p -> ajoute_idents_pattern p

        in List.iter ajoute_idents_pattern p_list;
        if TyperData.noms_distincts !vars then ()
        else raise (Error (start_p, end_p, "Une variable de filtrage apparait plusieurs fois"))


let rec etend_l_env_unified sub g_env l_env start_p end_p = function
        | Ast.PatternArgument p -> begin match p with
                                | PatargConstant (Integer _) -> Tint
                                | PatargConstant (String _) -> Tstring
                                | PatargConstant (Boolean _) -> Tbool
                                | PatargIdent i -> Tvar i
                                | Pattern p' -> etend_l_env_unified sub g_env l_env start_p end_p p'
                                end
        | PatternConstructor (name, p_list) -> 
                        let p_list' = List.map (fun p -> Ast.PatternArgument p) p_list in
                        let _ = List.iter (fun motif -> let _ = contient_deux_fois_la_meme_var start_p end_p motif in ()) p_list' in
                        let data_name, vars, tau_list = trouve_g_env_constructeur name g_env start_p end_p in
                        let sigma = ref (List.map (fun v -> (v, Tvar v)) vars) in

                        sigma := List.append !sigma (List.filter (fun (s,t) -> List.assoc_opt s !sigma = None) l_env.vdecl);
                        List.iter (fun (p_i,tau_i) -> unifie_sub sigma (start_p, end_p) ((etend_l_env_unified sub g_env l_env start_p end_p p_i), tau_i)) (try List.combine p_list' tau_list with _ -> raise (Error (start_p, end_p, "Mauvais nombre d'arguments")));
                        List.iter (fun (a,b) -> let l' = List.filter (fun (c,d) -> c<>a) !sub in sub :=(a,b)::l') !sigma;
                        Tconstr (data_name, substitution !sigma vars)

(*Fonction pour vérifier qu'une définition est correcte*)
let verification_definition g_env l_env vars tau_list tau_ret = function
        | Ast.Definition (Name(name, start_pos, end_pos), p_list, e) ->
                        let p_list = List.map (fun p -> Ast.PatternArgument p) p_list in
                        pattern_distincts start_pos end_pos p_list;

                        let sub = ref (List.map (fun x -> (x, QuantifTvar x)) vars) in
                        sub := List.append !sub (List.filter (fun (s,t) -> List.assoc_opt s !sub = None) l_env.vdecl);

                        let combined_list = try List.combine p_list tau_list with _ -> raise (Error (start_pos, end_pos, "Mauvais nombre d'arguments")) in
                        List.iter (fun (p_i, tau_i) -> let tau_p_i = type_motif g_env l_env start_pos end_pos p_i in unifie_sub sub (start_pos, end_pos) (tau_p_i, tau_i)) combined_list;

                        let l_env = {instances=l_env.instances; vars=l_env.vars; vdecl = !sub} in
                        let sub2 = ref [] in
                        List.iter (fun (p_i,t_i) -> let _ = etend_l_env_unified sub2 g_env l_env start_pos end_pos p_i in ()) combined_list;
                        sub2 := List.filter (fun (a,b) -> not (existe_l_env a l_env)) !sub2;

                        let l_env = List.fold_left (fun env (a,b) -> ajoute_l_env_assoc a b env) l_env !sub2 in
                        let t_e = type_expression g_env l_env e in
                        let _ = unifie_sub sub (start_pos, end_pos) (type_of_texpr t_e, tau_ret) in
                        t_e

        | _ -> failwith "On attendait ici une definition de fonction"

let declaration_de_fonction g_env declaration definitions =
        match declaration with
        | Ast.TypeDeclaration (Name(name,start_pos,end_pos), vars, instances, tau_list) as decl ->
                        if List.length definitions < 1 then
                                raise (Error (start_pos, end_pos, "La fonction "^name^" ne possède pas de definition"))
                        else
                                if existe_g_env_fonction name g_env then
                                        raise (Error (start_pos, end_pos, "La fonction "^name^" existe deja"))
                        else
                                let vars = List.map (fun p -> match p with Ast.TypeIdent (Name(s,_,_)) -> s | _ -> failwith "Cas impossible") vars in
                                let l_env = List.fold_left (fun env v -> ajoute_l_env_var v env) empty_env vars
                        in let instances' = List.map (fun i -> match i with
                                                        | Tvar i_name -> (i_name,[])
                                                        | Tconstr x -> x
                                                        | _ -> failwith "Cas impossible"
                                                        ) (List.map (bf g_env l_env) instances) in
                        let tau_list' = List.map (bf g_env l_env) tau_list in
                        let l_env = List.fold_left (fun env i -> ajoute_l_env_instance i env) l_env instances' in
                        List.iter (fun t -> match t with 
                                                | Tconstr (x,_) -> if existe_g_env_without_error x g_env || existe_l_env x l_env then () else raise (Error (start_pos, end_pos, "Le type "^x^" devrait exister")) 
                                                | _ -> ()) tau_list';
                        let tau_list2, tau_ret = pop_dernier tau_list' in
                        let t_expr_list = List.map (verification_definition (ajoute_g_env_fonction name vars instances' tau_list' g_env) l_env vars tau_list2 tau_ret) definitions in
                        let b, col = colonne_de_filtrage g_env l_env definitions in
                        if b then
                                let maped_defs = List.map (fun p -> match p with Ast.Definition(_,p_list,_) -> Ast.PatternArgument (List.nth p_list col) | _ -> failwith "Cas impossible") definitions in
                                let type_a_filtrer = List.nth tau_list2 col in
                                if TyperExpression.filtrage_exhaustif g_env l_env start_pos end_pos (type_a_filtrer, maped_defs) then
                                        let maped_defs_shorted,_ = pop_dernier maped_defs in
                                        if List.length maped_defs_shorted > 0 && TyperExpression.filtrage_exhaustif g_env l_env start_pos end_pos (type_a_filtrer, maped_defs_shorted) then
                                                raise (Error (start_pos, end_pos, "La fonction "^name^" comporte au moins une définition de trop"))
                                                else
                                                        (*Il faut transformer la declaration pour avoir un case sur la colonne de matching*)
                                                        let Ast.Definition (Name(_,start_p,end_p),p_list_premier,e_premier) = List.hd definitions in
                                                        let counter = ref 0 in

                                                        let final_patarg_list = List.map (fun p -> 
                                                                if !counter <> col then let _ = incr counter in p else let _ = incr counter in Ast.PatargIdent "ma_super_variable_de_filtrage") p_list_premier in

                                                        let final_def_expr = TCase (TVariable ("ma_super_variable_de_filtrage", type_a_filtrer), List.combine maped_defs t_expr_list, type_of_texpr (List.hd t_expr_list)) in

                                                        (decl, TDefinition (name, final_patarg_list, final_def_expr), ajoute_g_env_fonction name vars instances' tau_list' g_env)
                                                                else
                                                                        raise (Error (start_pos, end_pos, "Filtrage non exhaustif de la colonne "^(string_of_int col)))
                                else
                                        if List.length definitions > 1 then
                                                raise (Error (start_pos, end_pos, "La fonction "^name^" comporte au moins une définition de trop"))
                        else
                                let Definition(Name(name,_,_),p_list,e) = List.hd definitions in
                                (decl, TDefinition (name, p_list, List.hd t_expr_list), ajoute_g_env_fonction name vars instances' tau_list' g_env)

        | _ -> failwith "Il devrait s'agire d'une TypeDeclaration"
