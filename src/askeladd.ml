open Syntax
open Elaboration

let _ =
  let l = RLet ("id", RPi ("a", Imp, RSet, (RPi ("", Exp, RVar "a", RVar "a"))), RLam ("x", Exp, RVar "x"), RApp (RVar "id", RSet, Exp)) in
  print_endline (show_term (fst (infer Ctx.emptyCtx l)));
