open Tast
open PrettyPrinterBeta
open Preprocessing
open TyperFonction
open TyperExpression
open GestionEnv

let rec decoupe_defs s = function
        | [] -> ([], [])
        | Ast.Definition (Name (s',_,_),_,_) as d :: q when s=s' -> 
                        let s_defs, suite = decoupe_defs s q in (d::s_defs, suite)
        | l -> ([], l)

let rec defs_successives decl_list = 
        match decl_list with
        | [] -> ()
        | Ast.Definition (Name (s,start_p,end_p),_,_) :: q -> let s_defs, suite = decoupe_defs s q in
        if List.exists (fun p -> match p with Ast.Definition (Name (s',_,_),_,_) -> s=s' | _ -> failwith "Cas impossible") suite then
                raise (Error (start_p, end_p, "Les definitions de "^s^" ne sont pas succesives"))
        else
                defs_successives suite
        | _ -> failwith "Cas impossible"

let rec trouve_fun start_p end_p f = function
        | [] -> raise (Error (start_p, end_p, "La fonction "^f^" n'est pas déclarée"))
        | (g, vars, instances, tau_list) as res :: q when g=f -> res
        | _ :: q -> trouve_fun start_p end_p f q

let def_conforme g_env l_env start_p end_p def (f,vars,instances,tau_list) =
        let tau_list2, tau_ret = pop_dernier tau_list in        
        verification_definition g_env l_env [] tau_list2 tau_ret def

let declaration_d_instance g_env = function
        | Ast.Instance (typed_list, decl_list) -> 
                        let Ast.TypeConstructor (Name (c,start_p,end_p), tau_list), instances = pop_premier typed_list in
                        let name,vars,funs = trouve_g_env_classe c g_env start_p end_p in
                        (*Verifier que les types de types sont bien definies*)
                        (*Vérification que toutes les fonctions sont définies*)
                        List.iter (fun p -> match p with (s,_,_,_) -> 
                                if List.exists (fun p -> match p with Ast.Definition (Name (s',_,_),_,_) -> s=s' | _ -> failwith "Cas impossible") decl_list then
                                        ()
                                else
                                        raise (Error (start_p, end_p, "La fonction "^s^" de la classe "^c^" devrait être définie"))) funs;
                        (*Vérification que les définitions d'une même fonction sont successives*)
                        defs_successives decl_list;
                        (*Vérification que chaque définition est conforme à sa déclaration*)
                        let l_env = List.fold_left etend_l_env_typed empty_env instances in
                        let typed_instances = List.map (fun i -> match i with
                                                        | Tvar i_name -> (i_name,[])
                                                        | Tconstr x -> x
                                                        | _ -> failwith "Cas impossible"
                                                        ) (List.map (bf g_env l_env) typed_list) in
                        let i,instances = pop_premier typed_instances in
                        let l_env = List.fold_left (fun e i' -> ajoute_l_env_instance i' e) l_env instances in
                        List.iter (fun p -> match p with Ast.Definition ((Name (f,start_p,end_p)),pat_l, e) as d -> 
                        def_conforme g_env l_env start_p end_p d (trouve_fun start_p end_p f funs) | _ -> failwith "Cas impossible") decl_list;
                        (*TODO* vérifier qu'il n'y a pas plusieurs instances unifiables*)
                        (**)
                        begin
                        try 
                                List.iter (resoud_instance g_env empty_env start_p end_p) instances;
                                ajoute_g_env_instance instances i g_env;
                        with _ ->
                                ajoute_g_env_schema instances i g_env
                        end
         | _ -> failwith "Cas impossible"
