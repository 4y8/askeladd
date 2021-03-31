exception Perceus_error

type expr
  = Lam of string * expr
  | Var of string
  | App of expr * expr
  | Let of string * expr * expr
  | Dup of string list * expr
  | Drop of string list * expr
  | Clo of string * string list * expr

let inter =
  let rec aux acc l = function
      [] -> acc
    | hd :: tl when List.mem hd l -> aux (hd :: acc) l tl
    | _ :: tl -> aux acc l tl
  in aux []

let rec sub =
  let rec aux acc l l' =
    match l with
      [] -> acc
    | hd :: tl when List.mem hd l' -> aux acc tl l'
    | hd :: tl -> aux (hd :: acc) tl l'
  in aux []

let fv =
  let rec aux env = function
      Var v when List.mem v env -> []
    | Var _ -> []
    | App (e, e') -> (aux env e) @ (aux env e')
    | Let (v, e, e') -> (aux env e) @ (aux (v :: env) e')
    | Clo (v, _, e)
    | Lam (v, e) -> aux (v :: env) e
    | Drop (_, e)
    | Dup (_, e) -> aux env e
  in aux []

let rec perceus delta gamma = function
    Var v when gamma = [v] -> Var v
  | Var v when gamma = [] && List.mem v delta -> Dup ([v], Var v)
  | App (e, e') ->
     let gamma' = inter gamma (fv e') in
     let e' = perceus delta gamma' e' in
     let e = perceus (delta @ gamma') (sub gamma gamma') e in
     App (e, e')
  | Lam (v, e) as le when List.mem v (fv e) ->
     let ys = fv le in
     let e = perceus [] (v :: ys) e in
     Dup (sub ys gamma, Clo (v, ys, e))
  | Lam (v, e) as le ->
     let ys = fv le in
     let e = perceus [] ys e in
     Dup (sub ys gamma, Clo (v, ys, Drop ([v], e)))
  | Let (v, e, e') when List.mem v (fv e') ->
     let gamma' = inter gamma (sub (fv e') [v]) in
     let e = perceus (delta @ gamma') (sub gamma gamma') e in
     let e' = perceus delta (v :: gamma') e' in
     Let (v, e, e')
  | Let (v, e, e') ->
     let gamma' = inter gamma (fv e') in
     let e = perceus (delta @ gamma') (sub gamma gamma') e in
     let e' = perceus delta gamma' e' in
     Let (v, e, Drop ([v], e'))
  | _ -> raise Perceus_error
