open Syntax
open Elaboration
open Erasure
open Perceus

let _ =
  let l = RLet ("id", RPi ("a", Imp, RSet, (RPi ("", Exp, RVar "a", RVar "a"))), RLam ("x", Exp, RVar "x"), RApp (RVar "id", RSet, Exp)) in
  let e = (fst (infer Ctx.emptyCtx l)) in
  let p = erase [] e in
  let p = perceus [] [] p in
  print_endline (show_term e);
  print_endline (show_expr p)
