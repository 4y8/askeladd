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

let rec checkExpr (k, rho, gamma) e v =
  match e with
    Abs (x, e) ->
     begin
       match whnf v with
         VClo (env, Pi (y, a, b)) ->
          let v = VGen k in
          checkExpr (k + 1, update rho x v, update gamma x (VClo (env, a))) e
            (VClo (update env y v, b))
       | _ -> failwith "expected Pi"
     end
  | Pi (x, a, b) ->
     begin
       match whnf v with
         VSet -> checkSet (k, rho, gamma) a &&
                   checkSet (k + 1,
                             update rho x (VGen k),
                             update gamma x (VClo (rho, a)))
                     b
       | _ -> failwith "expected Set"
     end
  | Let (x, e, t, e') ->
     checkSet (k, rho, gamma) t &&
       checkExpr (k,
                  update rho x (eval rho e),
                  update gamma x (eval rho t))
         e' v
  | _ ->
     eqVal (k, inferExpr (k, rho, gamma) e, v)

and checkSet (k, rho, gamma) e =
  checkExpr (k, rho, gamma) e VSet

and inferExpr (k, rho, gamma) e =
  match e with
    Var x -> List.assoc x gamma
  | App (e, e') ->
     begin
       match whnf (inferExpr (k, rho, gamma) e) with
         VClo (env, Pi (x, a, b)) ->
          if checkExpr (k, rho, gamma) e' (VClo (env, a))
          then VClo (update env x (VClo (rho, e')), b)
          else failwith "application error"
       | _ -> failwith "expected Pi"
     end
  | Set -> VSet
  | _ -> failwith "Can't infer type"
