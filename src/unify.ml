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

let dom (PRen (dom, _, _)) = dom
let cod (PRen (_, cod, _)) = cod
let ren (PRen (_, _, ren)) = ren

let rename m pren v =
  let rec auxSp pren t = function
      [] -> t
    | (u, i) :: sp -> App (auxSp pren t sp, aux pren u, i)
  and aux pren t =
    match force t with
      VFlex (m', sp) ->
       if m = m'
       then raise Unify_error
       else auxSp pren (Meta m') sp
    | VRigid (x, sp) ->
       auxSp pren (Var (lvl2ix (dom pren) (List.assoc x (ren pren)))) sp
    | VGlob (v, sp) -> auxSp pren (GVar v) sp
    | VLam (x, i, t) ->
       Lam (x, i, aux (lift pren) (t $$ (VRigid (cod pren, []))))
    | VPi (x, i, a, b) ->
       Pi (x, i, aux pren a, aux (lift pren) (b $$ (VRigid (cod pren, []))))
    | VSet -> Set
  in
  aux pren v

let lams =
  let rec lams x l t =
    match l with
      [] -> t
    | hd :: tl -> Lam ("x" ^ (string_of_int x), hd, lams (x + 1) tl t)
  in
  lams 0

let solve gamma m sp rhs =
  let pren = invert gamma sp in
  let rhs = rename m pren rhs in
  let sol = eval [] (lams (List.rev (List.map snd sp)) rhs) in
  let MetaVar m = m in
  Metacontext.(metaContext := (m, Solved sol) :: !metaContext)

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
  | VGlob (v, sp), VGlob (v', sp') when v = v' -> unifySp l sp sp'
  | VSet, VSet -> ()
  | VFlex (m, sp), t
  | t, VFlex (m, sp) -> solve l m sp t
  | _ -> raise Unify_error

and unifySp l sp sp' =
  match sp, sp' with
    [], [] -> ()
  | (t, _) :: sp, (t', _) :: sp' -> unifySp l sp sp'; unify l t t'
  | _ -> raise Unify_error
