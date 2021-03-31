open Syntax
open Metacontext

exception Impossible

let rec eval env = function
    Var x -> List.nth env x
  | App (t, u, i) -> vApp (eval env t) (eval env u) i
  | Lam (x, i, t) -> VLam (x, i, (Closure (env, t)))
  | Pi (x, i, a, b) -> VPi (x, i, eval env a, Closure (env, b))
  | Let (_, _, t, u) -> eval ((eval env t) :: env) u
  | Set -> VSet
  | Meta m -> vMeta m
  | InsertedMeta (m, bds) -> vAppBDs env (vMeta m) bds

and vApp t u i =
  match t with
    VLam (_, _, t) -> t $$ u
  | VFlex (m, sp) -> VFlex (m, (u, i) :: sp)
  | VRigid (x, sp) -> VRigid (x, (u, i) :: sp)
  | _ -> raise Impossible

and vAppSp t = function
    [] -> t
  | (u, i) :: sp -> vApp (vAppSp t sp) u i

and vAppBDs env v bds =
  match (env, bds) with
    ([], []) -> v
  | (t :: env, Bound :: bds) -> vApp (vAppBDs env v bds) t Exp
  | (_ :: env, Defined :: bds) -> vAppBDs env v bds
  | _ -> raise Impossible

and vMeta m =
  match lookupMeta m with
    Solved v -> v
  | Unsolved -> VFlex (m, [])

and ($$) (Closure (env, t)) u = eval (u :: env) t

let rec force v =
  match v with
    VFlex (m, sp) ->
     begin
       match lookupMeta m with
         Solved v -> force (vAppSp v sp)
       | Unsolved  -> v
     end
  | v -> v

let lvl2ix l l' =
  l - l' - 1

let rec quote l t =
  match force t with
    VFlex (m, sp) -> quoteSp l (Meta m) sp
  | VRigid (x, sp) -> quoteSp l (Var (lvl2ix l x)) sp
  | VLam (x, i, t) -> Lam (x, i, quote (l + 1) (t $$ (VRigid (l, []))))
  | VPi (x, i, a, b) ->
     Pi (x, i, quote l a, quote (l + 1) (b $$ (VRigid (l, []))))
  | VSet -> Set

and quoteSp l t = function
    [] -> t
  | (u, i) :: sp -> App (quoteSp l t sp, quote l u, i)

let rec rmmeta = function
    Set -> Set
  | Var n -> Var n
  | InsertedMeta (m, _)
  | Meta m ->
     begin
       match Metacontext.lookupMeta m with
         Unsolved -> Meta m
       | Solved v -> quote 0 v
     end
  | App (e, e', i) -> App (rmmeta e, rmmeta e', i)
  | Let (v, e, t, e') ->
     Let (v, rmmeta e, rmmeta t, rmmeta e')
  | Lam (v, i, e) -> Lam (v, i, rmmeta e)
  | Pi (v, i, a, b) -> Pi (v, i, rmmeta a, rmmeta b)
