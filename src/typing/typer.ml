open Tast
open GestionEnv
open Preprocessing
open TyperFonction
open TyperData
open TyperClasse
open TyperInstance
open PrettyPrinterBeta

let rec separe_defs_suite name l = match l with
        | [] -> ([],[])
        | Ast.Definition (Name(s,start_pos, end_pos),_,_) as d :: q when s=name -> 
                        let l1, l2 = separe_defs_suite name q in (d::l1,l2)

        | _ -> ([], l)

let rec type_declarations g_env = function
        | [] -> g_env

        | Ast.Definition (_,_,e) :: q -> let (start_p, end_p) = e.location in raise (Error (start_p, end_p, "Definition typing not implemented"))

        | Ast.Data _ as d :: q -> type_declarations (declaration_de_type g_env d) q

        | Ast.TypeDeclaration (Name (name,start_pos, end_pos),_,_,_) as td :: q -> 
                        (*La liste des definitions qui suivent n'est pas construite au parsing nous devons donc le faire ici*)
                        let definitions, suite_du_fichier = separe_defs_suite name q in 
                        if name = "main" && List.length definitions <> 1 then raise (Error (start_pos, end_pos, "main doit avoir une unique definition"))
                        else
                                let g_env' = declaration_de_fonction g_env td definitions in
                                type_declarations g_env' suite_du_fichier

        | Ast.Class _ as c :: q -> type_declarations (declaration_de_classe g_env c) q

        | Ast.Instance _ as i :: q -> type_declarations (declaration_d_instance g_env i) q

let type_file = function
        | Ast.File (_, l) -> 
                        let g_env = type_declarations init_g_env l in
                        let start_p = {Lexing.pos_fname = ""; pos_lnum=0; pos_bol=0; pos_cnum=0} in
                                let end_p = {Lexing.pos_fname = ""; pos_lnum=0; pos_bol=0; pos_cnum=0} in
                                if existe_g_env_fonction "main" g_env then
                                        g_env
                                else
                                        raise (Error (start_p, end_p, "Le fichier doit contenir la fonction main"))

