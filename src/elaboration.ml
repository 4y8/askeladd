open Evaluation
open Unify
open Syntax

let freshMeta ctx =
  Metacontext. (
    let m = !nextMeta in
    nextMeta := m + 1;
    metaContext := (m, Unsolved) :: !metaContext;
    InsertedMeta (MetaVar m, ctx.bds)
  )

let rec check ctx e t =
  match e, force t with
    RHole, _ -> freshMeta ctx
