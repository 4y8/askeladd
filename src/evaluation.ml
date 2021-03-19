open Syntax

exception Impossible

let rec eval env = function
    Var x -> List.nth env x
  | App (t, u, i) -> vApp (eval env t) (eval env u) i
  | Lam (x, i, t) -> VLam (x, i, (Closure (env, t)))
  | Set -> VSet
  | Meta m -> VFlex (m, [])

and vApp t u i =
  match t with
    VLam (_, _, t) -> t $$ u
  | VFlex (m, sp) -> VFlex (m, (u, i) :: sp)
  | VRigid (x, sp) -> VRigid (x, (u, i) :: sp)
  | _ -> raise Impossible

and vAppSp t = function
    [] -> t
  | (u, i) :: sp -> vApp (vAppSp t sp) u i

and ($$) (Closure (env, t)) u = eval (u :: env) t
