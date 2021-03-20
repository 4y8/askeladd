open Syntax
open Evaluation

let rec unify l t t' =
  match (force t, force t') with
    VLam (_, _, t), VLam (_, _, t') ->
     unify (l + 1) (t $$ VRigid (l, [])) (t' $$ VRigid (l, []))
  | t, VLam (_, i, t') ->
     unify (l + 1) (vApp t (VRigid (l, [])) i) (t' $$ (VRigid (l, [])))
  | VLam (_, i, t), t' ->
     unify (l + 1) (t $$ (VRigid (l, []))) (vApp t' (VRigid (l, [])) i)
  | VSet, VSet -> ()
