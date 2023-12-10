open Tast

let g_env = {types = [];
                datas = [];
                fonctions=[];
                classes=[];
                instances=[];
                schemas=[]
        }


let empty = { instances = []; bindings = Smap.empty; fvars = Vset.empty }

let trouve_fun_genv s = 
        let rec aux = function
        | [] -> failwith "Fonction non déclarée"
        | (x,vars,instances,tau_list) as res :: q when x=s -> res 
        | _ :: q -> aux q
        in aux g_env.fonctions

let rec type_with_sub env sigma = function
        | _ -> Tunit

let rec match_data_types env sigma = function
        | [], [] -> ()
        | e::q, tau::q' -> if type_with_sub env sigma e = tau then
                match_data_types env sigma (q,q')
        else
                failwith "expression du mauvais type pour le constructeur"
        | _,_ -> failwith "Il y a trop peu ou pas assez d'expressions pour ce constructeur"

let appartient_instance_genv i =
        let rec aux = function
                | [] -> false
                | i'::q -> i=i' || aux q
        in aux g_env.instances

let appartient_instance_lenv i (env:env) =
        let rec aux = function
                | [] -> false
                | i'::q -> i=i' || aux q
        in aux env.instances

let trouve_schema_genv i = 
        let rec aux = function
                | [] -> failwith "Schema non trouvé"
                | (l,i')::q -> if i=i' then l else aux q
        in aux g_env.schemas

let rec resoud_instance env sigma i =
        if appartient_instance_lenv i env || appartient_instance_genv i then
                ()
        else
                let l = trouve_schema_genv i in
                List.iter (fun i' -> resoud_instance env sigma i') l

let rec substitution sigma = function
        | Tvar t -> failwith "undeclared variable"
	| Teffect t -> 
	| Tarrow t1,t2 ->  
        | Tconstr s, t_list ->
        | t -> t

(

(* module V pour les variables de type *)

(* réduction en tête d'un type (la compression de chemin serait possible) *)
let rec head = function
  | Tvar { def = Some t } -> head t
  | t -> t

(* forme canonique d'un type = on applique head récursivement *)
let rec canon t = match head t with
  | Tvar _ | Tint as t -> t
  | Tarrow (t1, t2) -> Tarrow (canon t1, canon t2)

(* unification *)

exception UnificationFailure of typ * typ

let unification_error t1 t2 = raise (UnificationFailure (canon t1, canon t2))

let rec occur v t = match head t with
  | Tvar w -> V.equal v w
  | Tarrow (t1, t2) -> occur v t1 || occur v t2
  | Tint -> false

let rec unify t1 t2 = match head t1, head t2 with
  | Tint, Tint ->
      ()
  | Tvar v1, Tvar v2 when V.equal v1 v2 ->
      ()
  | Tvar v1 as t1, t2 ->
      if occur v1 t2 then unification_error t1 t2;
      assert (v1.def = None);
      v1.def <- Some t2
  | t1, Tvar v2 ->
      unify t2 t1
  | Tarrow (t11, t12), Tarrow (t21, t22) ->
      unify t11 t21; unify t12 t22
  | t1, t2 ->
      unification_error t1 t2

let cant_unify ty1 ty2 =
  try let _ = unify ty1 ty2 in false with UnificationFailure _ -> true

(* schéma de type *)

(* variables libres *)

let rec fvars t = match head t with
  | Tint -> Vset.empty
  | Tarrow (t1, t2) -> Vset.union (fvars t1) (fvars t2)
  | Tvar v -> Vset.singleton v

let norm_varset s =
  Vset.fold (fun v s -> Vset.union (fvars (Tvar v)) s) s Vset.empty

let () =
  assert (Vset.is_empty (fvars (Tarrow (Tint, Tint))));
  let a = V.create () in
  let ta = Tvar a in
  let ty = Tarrow (ta, ta) in
  assert (Vset.equal (fvars ty) (Vset.singleton a));
  unify ty (Tarrow (Tint, Tint));
  assert (Vset.is_empty (fvars ty))

(* environnement c'est une table bindings (string -> schema),
   et un ensemble de variables de types libres *)

let add gen x t env =
  let vt = fvars t in
  let s, fvars =
    if gen then
      let env_fvars = norm_varset env.fvars in
      { vars = Vset.diff vt env_fvars; typ = t }, env.fvars
    else
      { vars = Vset.empty; typ = t }, Vset.union env.fvars vt
  in
  { instances = env.instances; bindings = Smap.add x s env.bindings; fvars = fvars }

(* find x env donne une instance fraîche de env(x) *)
let find x env =
  let tx = Smap.find x env.bindings in
  let s =
    Vset.fold (fun v s -> Vmap.add v (Tvar (V.create ())) s)
      tx.vars Vmap.empty
  in
  let rec subst t = match head t with
    | Tvar x as t -> (try Vmap.find x s with Not_found -> t)
    | Tint -> Tint
    | Tbool -> Tbool
    | Tunit -> Tunit
    | Tstring -> Tstring
    | Tarrow (t1, t2) -> Tarrow (subst t1, subst t2)
  in
  subst tx.typ

  (*vérifie qu'un type suggéré est bien formé et renvoie le cas échéant son type*)

let ajoute_contexte_decl s tau e = {instances = e.instances; bindings = Smap.add s tau e.bindings; fvars=e.fvars }

let rec est_exhaustive l t = true

let rec type_of_typed env = function
        | TypeIdent s -> (Smap.find s env.bindings).typ
        | TypeConstructor (Name s, t_list) -> Tconstr (s, List.map (fun tau_i -> type_of_typed env tau_i) t_list)

let rec type_pattern env = function
        | PatternArgument p -> begin match p with
                                | PatargConstant c -> begin match c with
                                                        | Boolean _ -> Tbool
                                                        | Integer _ -> Tint
                                                        | String _ -> Tstring
                                end
                                | PatargIdent x -> Tvar (V.create ()) 
                                | Pattern p -> type_pattern env p
        end
        | PatternConstructor (s,p_list) -> Tunit (*à faire*)

let rec type_expr env = function
  | Variable x ->
      find x env

  | Constant c -> begin match c with
                | Boolean b -> Tbool
                | Integer i -> Tint
                | String s -> Tstring
  end

  | UnaryOperation (Not, e) -> if type_expr env e = Tbool then Tbool else failwith "Not appliqué à une expression non booléenne"

  | TypedExpression (e, tau) -> 
                  let res = type_expr env e in 
                  unify res (type_of_typed env tau);
                  res
  
  | BinaryOperation (e1, binop, e2) -> begin match type_expr env e1, binop, type_expr env e2 with
        | Tint, Plus, Tint | Tint, Minus, Tint | Tint, Times, Tint | Tint, Divide, Tint -> Tint 
        | Tint, Equal, Tint | Tint, NotEqual, Tint | Tint, LessThan, Tint | Tint, LessThanOrEqual, Tint | Tint, GreaterThan, Tint | Tint, GreaterThanOrEqual, Tint -> Tbool 
        | Tbool, Equal, Tbool | Tbool, NotEqual, Tbool | Tstring, Equal, Tstring | Tstring, NotEqual, Tstring -> Tbool
        | Tbool, And, Tbool | Tbool, Or, Tbool -> Tbool
        | Tstring, Concatenate, Tstring -> Tstring
  end
        
  | Conditional (e1, e2, e3) -> if type_expr env e1 = Tbool then
                let res = type_expr env e2 in
                if type_expr env e3 = res then 
                        res
                else 
                        failwith "Type de retour différents dans un if"
        else
                failwith "Opérande non booléenne dans un if"

  | Do e_list -> 
                  if List.for_all (fun e -> type_expr env e = Teffect Tunit) e_list then 
                          Teffect Tunit
                  else
                          failwith "Les expressions du Do doivent être de type Effect Unit"

  | Let (l, e) -> let env' = ref env in
  List.iter (fun (s,exp) -> let tau = type_expr !env' exp in env' := ajoute_contexte_decl s { vars = Vset.empty; typ=tau } !env') l; type_expr !env' e

  | FunctionCall (s,l) (*ident * expression list*) -> let name,vars,instances,tau_list = 
          trouve_fun_genv s in
  let sigma = ref (List.map (fun s -> (, typ)) vars) in
  let _ = match_data_types env sigma (l, tau_list) in
  let _ =  List.iter (fun i -> resoud_instance env sigma i) in substitution sigma (List.tl tau_list)

  | ExplicitConstructor (s,l) (*ident * expression list*) -> let name,vars,name_constr,tau_list = trouve_data_genv s in 
  let sigma = ref (List.map (fun s -> (s,Tvar (V.create ()))) vars) in
  let _ = match_data_types env sigma (l,tau_list) in
  Tconstr (name, remplace_vars sigma)

  | Case (e,l) (*expression * (pattern * expression) list*) -> let e_type = type_expr env e in
  if List.for_all (fun (p_i,_) -> type_pattern env p_i = e_type) l && est_exhaustive l e_type then
          let p_1,e_1 = List.hd l in
          let tau' = type_expr (env (*auquel on ajoute p_1 i.e toutes les variables de p_1*)) e_1 in
          if List.for_all (fun (p_i,e_i) -> type_expr (env (*auquel on ajoute p_i*)) e_i = tau') l then
                  tau'
          else
                  failwith "Types des expressions de retours du case différents"
  else
          failwith "Tous les patternes ne sont pas du bons type"


  (*
  | Pair (e1, e2) ->
      let t1 = w env e1 in
      let t2 = w env e2 in
      Tproduct (t1, t2)
  | Fun (x, e1) ->
      let v = Tvar (V.create ()) in
      let env = add false x v env in
      let t1 = w env e1 in
      Tarrow (v, t1)
  | App (e1, e2) ->
      let t1 = w env e1 in
      let t2 = w env e2 in
      let v = Tvar (V.create ()) in
      unify t1 (Tarrow (t2, v));
      v
  | Let (x, e1, e2) ->
      let t1 = w env e1 in
      let env = add true x t1 env in
      w env e2
      *)

let typeof e = canon (type_expr empty e)
let rec print_type = function
	| Tint -> print_string " Tint "
	| Tstring -> print_string " Tstring "
	| Tbool -> print_string " Tbool "
	| Tunit -> print_string " Tunit "
        | Tvar tvar -> print_char ' '; print_int tvar.id; print_char ' '
        | Teffect t -> print_string " Teffect("; print_type t; print_string ") "
	| Tarrow (t1,t2) -> ()
        | Tconstr constr -> ()

let _ = print_type (type_expr empty (Conditional (BinaryOperation (Constant (Boolean false), Or, Constant (Boolean true)),Constant (Integer 0), Constant (Integer 1))))
