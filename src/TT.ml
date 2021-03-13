open Gram

let update env v value =
  (v, value) :: env

let rec app u v =
  match u with
    VClo (env, (Abs (x, e))) -> eval (update env x v) e
  | _ -> VApp (u, v)

and eval env e =
  match e with
    Var x -> List.assoc x env
  | App (e, e') -> app (eval env e) (eval env e')
  | Let (x, e, _, e') -> eval (update env x (eval env e)) e'
  | Set -> VSet
  | _ -> VClo (env, e)

let rec whnf =
  function VApp (u, v) -> app (whnf u) (whnf v)
         | VClo (env, e) -> eval env e
         | v -> v

let rec eqVal (n, u, v) =
  match (whnf u, whnf v) with
    VSet, VSet -> true
  | VGen k, VGen k' -> k = k'
  | VApp (u, v), VApp (u', v') -> eqVal (n, u, u') && eqVal (n, v, v')
  | VClo (env, Abs (x, e)), VClo (env', Abs (x', e')) ->
     let v = VGen (n + 1) in
     eqVal (n + 1, VClo ((update env x v), e), VClo ((update env' x' v), e'))
  | VClo (env, Pi (x, e, t)), VClo (env', Pi (x', e', t')) ->
     let v = VGen (n + 1) in
     eqVal (n, VClo (env, e), VClo (env', e')) &&
       eqVal (n + 1, VClo ((update env x v), t), VClo ((update env' x' v), t'))
  | _ -> false
