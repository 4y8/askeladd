open Syntax
open Elaboration

let _ =
  let t = check Ctx.emptyCtx (RPi ("a", Imp, RSet, (RPi ("", Exp, RVar "a", RVar "a")))) VSet in
  let t = Evaluation.eval [] t in
  print_endline (show_term (check Ctx.emptyCtx (RLam ("x", Exp, RVar "x")) t))
