open Evaluation
open Unify
open Syntax

let freshMeta ctx =
  Metacontext.(
    let m = !nextMeta in
    nextMeta := m + 1;
    metaContext := (m, Unsolved) :: !metaContext;
    InsertedMeta (MetaVar m, ctx.bds)
  )

let insert' ctx =
  let rec go (t, va) =
    match force va with
      VPi (_, Imp, _, b) ->
       let m = freshMeta ctx in
       let mv = eval (ctx.env) m in
       go (App (t, m, Imp), b $$ mv)
    | va -> t, va
  in
  go

let insert ctx = function
    Lam (_, Imp, _) as t, va -> t, va
  | t, va -> insert' ctx (t, va)

let rec check ctx e t =
  match e, force t with
    RLam (x, i, t), VPi (_, i', a, b) when i = i' ->
     Lam (x, i, check (bind ctx x a) t (b $$ (VRigid (ctx.lvl, []))))
  | t, VPi (x, Imp, a, b) ->
     Lam (x, Imp, check (newBinder ctx x a) t (b $$ (VRigid (ctx.lvl, []))))
  | RLet (x, a, t, u), a' ->
     let a = check ctx a VSet in
     let va = eval ctx.env a in
     let t = check ctx t va in
     let vt = eval ctx.env t in
     let u = check (define ctx x vt va) u a' in
     Let (x, a, t, u)
  | RHole, _ -> freshMeta ctx
  | t, exp ->
     let t, inf = insert ctx (infer ctx t) in
     unify ctx.lvl exp inf;
     t

and infer ctx = function
    RSet -> Set, VSet
