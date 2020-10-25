open Syntax

let rec to_deb e v n =
  match e with
    Var v' when v = v' -> Deb n
  | App (l, r) -> App (to_deb l v n, to_deb r v n)
  | Lam (v', None, b) ->
     Lam ("", None, to_deb (to_deb b v' 0) v (n + 1))
  | Lam (v', Some t, b) ->
     Lam ("", Some (to_deb t v n), to_deb (to_deb b v' 0) v (n + 1))
  | Pi (v', t, b) ->
     Pi ("", to_deb t v n, to_deb (to_deb b v' 0) v (n + 1))
  | Let (v', e, b) -> Let (v', to_deb e v n, to_deb b v n)
  | e -> e

let rec subst e n s =
  match e with
    Deb n' when n' = n -> s
  | App (l, r) -> App (subst l n s, subst r n s)
  | Lam (_, None, b) ->
     Lam ("", None, subst b (n + 1) s)
  | Lam (_, Some t, b) ->
     Lam ("", Some (subst t n s), subst b (n + 1) s)
  | Pi (_, t, b) -> Pi ("", subst t n s, subst b (n + 1) s)
  | Let (v, e, b) -> Let (v, subst e n s, subst b n s)
  | e -> e

let rec reloc e i =
  match e with
    Pi (_, t, b) -> Pi ("", reloc t i, reloc b (i + 1))
  | Lam (_, None, b) -> Lam ("", None, reloc b (i + 1))
  | Lam (_, Some t, b) ->
     Lam ("", Some (reloc t i), reloc b (i + 1))
  | Let (v, e, b) -> Let (v, reloc e i, reloc b i)
  | Deb k when k >= i -> Deb (k + 1)
  | App (l, r) -> App (reloc l i, reloc r i)
  | e -> e

let reloc_ctx =
  List.map (fun e -> reloc e 0)

let rec whnf c g =
  function
    Var v ->
     begin
       match List.assoc_opt v g with
         None -> Var v
       | Some e -> e
     end
  | App (e, e') ->
     let e' = whnf c g e' in
     (match whnf c g e with
         Lam (_, _, b) -> whnf c g (subst b 0 e')
       | e -> App (e, e'))
  | e -> e

let rec equal c g e e' =
  let e  = whnf c g e in
  let e' = whnf c g e' in
  match e, e' with
    Deb n, Deb n' -> n = n'
  | Var v, Var v' -> v = v'
  | Set n, Set m -> n = m
  | Lam (_, Some t, b), Lam (_, Some t', b') ->
     equal c g t t' && equal c g b b'
  | Lam (_, None, b), Lam (_, None, b') -> equal c g b b'
  | Ann (e, t), Ann (e', t') ->
     equal c g e e' && equal c g t t'
  | Pi (_, t, p), Pi (_, t', p') ->
     equal c g t t' && equal c g p p'
  | _ -> false

let rec infer_type (e, exp) c g =
  let check_equal e e' =
    match equal c g e e' with
      true -> ()
    | false -> failwith "Non equal types"
  in
  match e with
    Deb n -> (List.nth c n)
  | Ann (e, t) ->
     let t' = infer_type (e, None) c g in
     check_equal t t';
     t
  | Lam (_, t, b) ->
     (match exp with
       Some (Pi (_, p, p')) ->
        let te = infer_type (b, Some p') (reloc_ctx (p :: c)) g in
        check_equal p' te;
        Pi ("", p, te)
      | None ->
         begin
           match t with
             Some p ->
             let te = infer_type (b, None) (reloc_ctx (p :: c)) g in
             Pi ("", p, te)
           | None -> failwith "missing type annotation."
         end
      | _ -> failwith "bad type for lambda abstraction.")
  | Set n -> Set (n + 1)
  | Pi (_, p, p') ->
     let m = infer_set c g p in
     let n = infer_set (p :: c) g p' in
     Set (max m n)
  | Var v -> List.assoc v g
  | Let (v, t, b) ->
     let t = infer_type (t, None) c g in
     infer_type (b, None) c ((v, t) :: g)
  | App (l, r) ->
     let p, p' = infer_pi l c g in
     let tr = infer_type (r, None) c g in
     check_equal tr p;
     subst p' 0 r
  | _ -> raise Not_found (* TODO *)

and infer_set c g e =
  let u = infer_type (e, None) c g  in
  match whnf c g u with
    Set n -> n
  | _ -> failwith "expected type"

and infer_pi e c g =
  let t = infer_type (e, None) c g in
    match whnf c g t with
      Pi (_, p, p') -> p, p'
    | _ -> failwith "Expected a function"
