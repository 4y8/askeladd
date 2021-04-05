open Syntax
open Evaluation

let varname = ref 0
let newvar () =
  let n = !varname in
  varname := n + 1;
  string_of_int n

let rec erase env = function
    Lam (_, _, e) ->
    let v = newvar () in
    Perceus.Lam (v, erase (v :: env) e)
  | Set
  | Pi _ -> Perceus.Erased
  | InsertedMeta _ as e ->
     let v = eval [] e in
     let e = quote 0 v in
     erase env e
  | Meta _ as e ->
     let v = eval [] e in
     let e = quote 0 v in
     erase env e
  | Var n -> Perceus.Var (List.nth env n)
  | GVar v -> Perceus.Var v
  | Let (_, _, e, e') ->
     let v = newvar () in
     Perceus.Let (v, erase env e, erase (v :: env) e')
  | App (e, e', _) ->
     Perceus.App (erase env e, erase env e')
