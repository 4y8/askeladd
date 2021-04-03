open Evaluation
open Unify
open Syntax
open Ctx

let freshMeta ctx =
  Metacontext.(
    let m = !nextMeta in
    nextMeta := m + 1;
    metaContext := (m, Unsolved) :: !metaContext;
    InsertedMeta (MetaVar m, ctx.env)
  )

let insert' ctx =
  let rec aux (t, va) =
    match force va with
      VPi (_, Imp, _, b) ->
       let m = freshMeta ctx in
       let mv = eval (vals ctx) m in
       aux (App (t, m, Imp, Type), b $$ mv)
    | va -> t, va
  in
  aux

let insert ctx = function
    Lam (_, Imp, _, _) as t, va -> t, va
  | t, va -> insert' ctx (t, va)

let istype = function
    VSet | VPi _ -> Type
    | _ -> Term

let rec check ctx e t =
  match e, force t with
    RLam (x, i, t), VPi (_, i', a, b) when i = i' ->
     Lam (x, i, check (bind ctx x a Unknown) t (b $$ (VRigid (ctx.lvl, []))), istype a)
  | t, VPi (x, Imp, a, b) ->
     Lam (x, Imp, check (newBinder ctx x a Unknown) t (b $$ (VRigid (ctx.lvl, []))), istype a)
  | RLet (x, a, t, u), a' ->
     let a = check ctx a VSet in
     let va = eval (vals ctx) a in
     let t = check ctx t va in
     let vt = eval (vals ctx) t in
     let u = check (define ctx x vt va (istype vt)) u a' in
     Let (x, a, t, u)
  | RHole, _ -> freshMeta ctx
  | t, exp ->
     let t, inf = insert ctx (infer ctx t) in
     unify ctx.lvl exp inf;
     t

and infer ctx = function
    RVar x ->
     let rec go ix = function
         (x', orig, t) :: types ->
          if x = x' && orig = Source
          then Var ix, t
          else go (ix + 1) types
       | [] -> raise Not_found
     in
     go 0 ctx.env.types
  | RLam (x, i, t) ->
     let a = eval (vals ctx) (freshMeta ctx) in
     let (t, b) = insert ctx (infer (bind ctx x a Unknown) t) in
     Lam (x, i, t, istype a), VPi (x, i, a, closeVal ctx b)
  | RApp (t, u, i) ->
     let t, tty =
       match i with
         Imp -> infer ctx t
       | Exp -> insert' ctx (infer ctx t)
     in
     let a, b =
       match force tty with
         VPi (_, i', a, b) ->
          if i <> i'
          then raise Unify_error
          else (a, b)
       | tty ->
          let a = eval (vals ctx) (freshMeta ctx) in
          let b = Closure (vals ctx, freshMeta (bind ctx "x" a Type)) in
          unify ctx.lvl tty (VPi ("x", i, a, b));
          a, b
     in
     let u = check ctx u a in
     App (t, u, i, istype a), b $$ (eval (vals ctx) u)
  | RPi (x, i, a, b) ->
     let a = check ctx a VSet in
     let b = check (bind ctx x (eval (vals ctx) a) Type) b VSet in
     Pi (x, i, a , b), VSet
  | RSet -> Set, VSet
  | RHole ->
     let a = eval (vals ctx) (freshMeta ctx) in
     let t = freshMeta ctx in
     t, a
  | RLet (x, a, t, u) ->
     let a = check ctx a VSet in
     let va = eval (vals ctx) a in
     let t = check ctx t va in
     let vt = eval (vals ctx) t in
     let u, b = infer (define ctx x vt va (istype vt)) u in
     Let (x, a, t, u), b
