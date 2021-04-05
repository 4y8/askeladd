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
  | GVar v -> VGlob (v, [])
  | InsertedMeta (m, env) -> vAppMEnv env.vals (vMeta m)

and vApp t u i =
  match t with
    VLam (_, _, t) -> t $$ u
  | VFlex (m, sp) -> VFlex (m, (u, i) :: sp)
  | VRigid (x, sp) -> VRigid (x, (u, i) :: sp)
  | VGlob (v, sp) -> VGlob (v, (u, i) :: sp)
  | _ -> raise Impossible

and vAppSp t = function
    [] -> t
  | (u, i) :: sp -> vApp (vAppSp t sp) u i

and vAppMEnv env v =
  match env with
    [] -> v
  | Bound t :: env -> vApp (vAppMEnv env v) t Exp
  | Defined _ :: env -> vAppMEnv env v

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
  | VGlob (v, sp) -> quoteSp l (GVar v) sp
  | VLam (x, i, t) -> Lam (x, i, quote (l + 1) (t $$ (VRigid (l, []))))
  | VPi (x, i, a, b) ->
     Pi (x, i, quote l a, quote (l + 1) (b $$ (VRigid (l, []))))
  | VSet -> Set

and quoteSp l t = function
    [] -> t
  | (u, i) :: sp -> App (quoteSp l t sp, quote l u, i)
