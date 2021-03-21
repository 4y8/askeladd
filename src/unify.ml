open Syntax
open Evaluation

exception Unify_error

type partial_renaming = PRen of int * int * ((int * int) list)

let lift (PRen (dom, cod, ren)) =
  PRen (dom + 1, cod + 1, (dom, cod) :: ren)

let invert gamma sp =
  let rec go = function
      [] -> (0, [])
    | (t, _) :: sp ->
       let dom, ren = go sp in
       match force t with
         VRigid (x, []) when List.mem_assoc x ren ->
         (dom + 1, (x, dom) :: ren)
       | _ -> raise Unify_error
  in
  let dom, ren = go sp in
  PRen (dom, gamma, ren)

let rename m pren v =
  let rec goSp pren t = function
      [] -> t
    | (u, i) :: sp -> App (goSp pren t sp, go pren u, i)
  and go pren t =
    match force t with
      V
    | VSet -> Set
  in
  go pren v

let lams =
  let rec lams x l t =
    match l with
      [] -> t
    | hd :: tl -> Lam ("x" ^ (string_of_int x), hd, lams (x + 1) tl t)
  in
  lams 0

let rec unify l t t' =
  match (force t, force t') with
    VLam (_, _, t), VLam (_, _, t') ->
     unify (l + 1) (t $$ VRigid (l, [])) (t' $$ VRigid (l, []))
  | t, VLam (_, i, t') ->
     unify (l + 1) (vApp t (VRigid (l, [])) i) (t' $$ (VRigid (l, [])))
  | VLam (_, i, t), t' ->
     unify (l + 1) (t $$ (VRigid (l, []))) (vApp t' (VRigid (l, [])) i)
  | VPi (_, i, a, b), VPi (_, i', a', b') when i = i' ->
     unify l a a';
     unify (l + 1) (b $$ (VRigid (l, []))) (b' $$ (VRigid (l, [])))
  | VRigid (x, sp), VRigid (x', sp') when x = x' -> unifySp l sp sp'
  | VFlex (m, sp), VFlex (m', sp') when m = m' -> unifySp l sp sp'
  | VSet, VSet -> ()
  | _ -> raise Unify_error

and unifySp l sp sp' =
  match sp, sp' with
    [], [] -> ()
  | (t, _) :: sp, (t', _) :: sp' -> unifySp l sp sp'; unify l t t'
  | _ -> raise Unify_error
