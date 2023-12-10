(*open Preprocessing.Ast*)

(* Abstract syntax tree of Little-PureScript *)

(* (identifier, number of line, number of column) *)
type ident = string

type name = Name of ident

type typed = 
  | TypeConstructor of ntype
  | TypeIdent of ident
and ntype = name * typed list


(* Constructor (constructor name, list of arguments) *)
type constructor = Constructor of name * typed list

type constant = 
	| Boolean of bool
	| Integer of int
	| String of string
type binaryOperation = 
	| Plus
	| Minus
	| Times
	| Divide
	| Modulo
	| And 
	| Or
  | Concatenate
	| Equal
	| NotEqual
	| LessThan
	| LessThanOrEqual
	| GreaterThan
	| GreaterThanOrEqual

type pattern = 
  | PatternArgument of patarg 
  | PatternConstructor of ident * patarg list
and patarg = 
  | PatargConstant of constant
  | PatargIdent of ident 
  | Pattern of pattern 

type unaryOperation = Not

type expression = 
	| Constant of constant
	| Variable of ident
  | UnaryOperation of unaryOperation * expression
	| TypedExpression of expression * typed
	| BinaryOperation of expression * binaryOperation * expression
	| Conditional of expression * expression * expression
  | ExplicitConstructor of ident * expression list
	| FunctionCall of ident * expression list
	| Do of expression list
	| Let of (ident * expression) list * expression
	| Case of expression * (pattern * expression) list


type typ = 
	| Tint 
	| Tstring 
	| Tbool
	| Tunit 
        | Tvar of tvar
	| Teffect of typ
	| Tarrow of typ*typ 
        | Tconstr of tconstr

and tvar = 
	{ 
		id : int; 
		mutable def : typ option; 
	}

and tconstr = string * typ list

type tfun = string * typ list
type tclass = string * tvar list * tfun list
type tinstance = string * typ list
type tinstance_schema = tinstance list * tinstance
type tdecfun = string * string list * tinstance list * typ list

type global_environment = {mutable datas : (string * string list * tconstr list) list;
                                mutable constr : tconstr list;
                                mutable fonctions : tdecfun list;
                                mutable classes : (string * string list * tdecfun list) list;
                                mutable instances : tinstance list;
                                mutable schemas : tinstance_schema list
        }

(* module V pour les variables de type *)

module V = struct
  type t = tvar
  let compare v1 v2 = Stdlib.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id
  let create = let r = ref 0 in fun () -> incr r; { id = !r; def = None }
end

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

module Vset = Set.Make(V)

type schema = { vars : Vset.t; typ : typ }

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

module Smap = Map.Make(String)

type env = { bindings : schema Smap.t; fvars : Vset.t }

let empty = { bindings = Smap.empty; fvars = Vset.empty }

let add gen x t env =
  let vt = fvars t in
  let s, fvars =
    if gen then
      let env_fvars = norm_varset env.fvars in
      { vars = Vset.diff vt env_fvars; typ = t }, env.fvars
    else
      { vars = Vset.empty; typ = t }, Vset.union env.fvars vt
  in
  { bindings = Smap.add x s env.bindings; fvars = fvars }

module Vmap = Map.Make(V)

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
    | Tarrow (t1, t2) -> Tarrow (subst t1, subst t2)
  in
  subst tx.typ

  (*vérifie qu'un type suggéré est bien formé et renvoie le cas échéant son type*)

let ajoute_contexte_decl s tau e = {bindings = Smap.add s tau e.bindings; fvars=e.fvars }

let rec type_of_typed env = function
        | TypeIdent s -> (Smap.find s env.bindings).typ
        | TypeConstructor (Name s, t_list) -> Tconstr (s, List.map (fun tau_i -> type_of_typed env tau_i) t_list)

(* algorithme W *)
let rec w env = function
  | Variable x ->
      find x env

  | Constant c -> begin match c with
                | Boolean b -> Tbool
                | Integer i -> Tint
                | String s -> Tstring
  end

  | UnaryOperation (Not, e) -> if w env e = Tbool then Tbool else failwith "Not appliqué à une expression non booléenne"

  | TypedExpression (e, tau) -> 
                  let res = w env e in 
                  unify res (type_of_typed env tau);
                  res
  
  | BinaryOperation (e1, binop, e2) -> begin match w env e1, binop, w env e2 with
        | Tint, Plus, Tint | Tint, Minus, Tint | Tint, Times, Tint | Tint, Divide, Tint -> Tint 
        | Tint, Equal, Tint | Tint, NotEqual, Tint | Tint, LessThan, Tint | Tint, LessThanOrEqual, Tint | Tint, GreaterThan, Tint | Tint, GreaterThanOrEqual, Tint -> Tbool 
        | Tbool, Equal, Tbool | Tbool, NotEqual, Tbool | Tstring, Equal, Tstring | Tstring, NotEqual, Tstring -> Tbool
        | Tbool, And, Tbool | Tbool, Or, Tbool -> Tbool
        | Tstring, Concatenate, Tstring -> Tstring
  end
        
  | Conditional (e1, e2, e3) -> if w env e1 = Tbool then
                let res = w env e2 in
                if w env e3 = res then 
                        res
                else 
                        failwith "Type de retour différents dans un if"
        else
                failwith "Opérande non booléenne dans un if"

  | Do e_list -> 
                  if List.for_all (fun e -> w env e = Teffect Tunit) e_list then 
                          Teffect Tunit
                  else
                          failwith "Les expressions du Do doivent être de type Effect Unit"

  | Let (l, e) -> let env' = ref env in
  List.iter (fun (s,exp) -> let tau = w !env' exp in env' := ajoute_contexte_decl s { vars = Vset.empty; typ=tau } !env') l; w !env' e
  | FunctionCall (s,l) (*ident * expression list*) -> Tunit
  | ExplicitConstructor (s,l) (*ident * expression list*) -> Tunit
  | Case (e,l) (*expression * (pattern * expression) list*) -> Tunit

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

let typeof e = canon (w empty e)
let test = print_string "ça compile"
