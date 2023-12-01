open Preprocessing.Ast

type typ = 
	| Tint 
	| Tstring 
	| Tbool
	| Tunit 
	| Teffect of typ
	| Tvar of tvar 
	| Tarrow of typ * typ
	| Tproduct of typ * typ
and tvar = 
	{ 
		id : int; 
		mutable def : typ option; 
	}
type inst = C of typ list 
type schema = S of inst list * inst

module V = struct
	type t = tvar
	let compare v1 v2 = Stdlib.compare v1.id v2.id
	let equal v1 v2 = v1.id = v2.id
	let create = let r = ref 0 in fun () -> incr r; { id = !r; def = None }
end

let rec head = function
	| Tvar { def = Some t } -> head t
	| t -> t

let rec canon t = match head t with 
	| Tarrow (t1, t2) -> Tarrow (canon t1, canon t2)
	| Tproduct (t1, t2) -> Tproduct (canon t1, canon t2)
	| t -> t

exception UnificationFailure of typ * typ
let unification_error t1 t2 = raise (UnificationFailure (t1, t2))

let rec occur v t = match head t with
	| Tvar v' -> V.equal v v'
	| Teffect t -> occur v t
	| Tarrow (t1, t2) -> occur v t1 || occur v t2
	| Tproduct (t1, t2) -> occur v t1 || occur v t2
	| _ -> false

let rec unify t1 t2 = match head t1, head t2 with
	| Tint, Tint | Tstring, Tstring | Tbool, Tbool | Tunit, Tunit -> ()
	| Tvar v1, Tvar v2 when V.equal v1 v2 -> ()
	| Tvar v1 as t1, t2 ->
		if occur v1 t2 then unification_error t1 t2;
		v1.def <- Some t2
	| t1, Tvar v2 ->
		if occur v2 t1 then unification_error t1 t2;
		v2.def <- Some t1
	| Tarrow (t11, t12), Tarrow (t21, t22) ->
		unify t11 t21;
		unify t12 t22
	| Tproduct (t11, t12), Tproduct (t21, t22) ->
		unify t11 t21;
		unify t12 t22
	| Teffect t1, Teffect t2 -> unify t1 t2
	| t1, t2 -> unification_error t1 t2
