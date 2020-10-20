open Combo

type expr
  = Var  of string
  | Pi   of string * expr * expr
  | Lam  of string * expr * expr
  | App  of expr * expr
  | Deb  of int
  | Univ
  | Let  of string * expr * expr
[@@deriving show]

type decl
  = TDecl of string * expr
  | FDecl of string * expr

let (%) f g x = f (g x)

let rec expr s =
  let ide = inplode <$> many1 letter in
  let keyword s = word s <* space in
  let comb s p q r = 
    s
    <$> (p
    <*  spaces)
     *> ide
    <*  between spaces q spaces
    <*> expr
    <*  between spaces r spaces
    <*> expr
  in
  let lam =
    let lam v t b = Lam (v, t, b) in
    comb lam (keyword "fun") (sym ':') (word "=>")
  in
  let univ = sym '*' *> return Univ in
  let pi =
    let pi v t b = Pi (v, t, b) in
    comb pi (sym '(') (sym ':') (sym ')' <* spaces <* word "->")
  in
  let var = (fun v -> Var v) <$> ide in
  let letin =
    let letin v e b = Let (v, e, b) in
    comb letin (keyword "let") (sym '=') (keyword "in")
  in
  let app l r = App (l, r) in
  let arr l r = Pi ("", l, r) in
  let p = (univ <|> lam <|> pi <|> letin <|> var) in
  let p = chainl1 (spaces *> (arr <$ word "->") <* spaces) p
           <|> packs "(" p ")" in
  let app = chainl1 (spaces *> return app <* spaces) p in
  app s

let rec to_deb e v n =
  match e with
    Var v' when v = v' -> Deb n
  | App (l, r) -> App (to_deb l v n, to_deb r v n)
  | Lam (v', t, b) -> Lam ("", to_deb t v n, to_deb (to_deb b v' 0) v (n + 1))
  | Pi (v', t, b) -> Pi ("", to_deb t v n, to_deb (to_deb b v' 0) v (n + 1))
  | Let (v', e, b) -> Let (v', to_deb e v n, to_deb b v n)
  | e -> e

let rec relocation e i =
  match e with
    Pi (_, t, b) -> Pi ("", relocation t i, relocation b (i + 1))
  | Lam (_, t, b) -> Lam ("", relocation t i, relocation b (i + 1))
  | Let (v, e, b) -> Let (v, relocation e i, relocation b i)
  | Deb k when k >= i -> Deb (k + 1)
  | App (l, r) -> App (relocation l i, relocation r i)
  | e -> e

let relocate_ctx =
  List.map (fun e -> relocation e 0)

let rec eval c g =
  function
    Univ -> Univ
  | Var v ->
     begin 
       match List.assoc_opt v g with
         None -> Var v
       | Some e -> e
     end
  | App (Lam (_, _, b), l) ->
     eval (l :: c) g b
  | App (l, r) ->
     let l' = eval c g l in
     if l = l' then App (l, r) else eval c g (App (l', r))
  | e -> e

let rec infer_type e c g =
  let check e t c g =
    let t' = infer_type e c g in
    if t = t'
    then ()
    else failwith "Type error"
  in
  match e with
    Deb n -> (List.nth c n)
  | Lam (_, t, b) ->
     let t' = infer_type b (relocate_ctx (t :: c)) g in
     Pi ("", t, t')
  | Univ -> Univ
  | Pi (_, p, p') ->
     check p p' c g;
     let t = eval c g p in
     check p' Univ (t :: c) g;
     Univ
  | Var v -> List.assoc v g
  | Let (v, t, b) ->
     let t = infer_type t c g in
     infer_type b c ((v, t) :: g)
  | App (l, r) ->
     let t = infer_type l c g in
     match t with
       Pi (_, t, t') ->
       check r t c g;
       t'
     | _ -> failwith "Type error"

let _ =
  let e = expr (explode (read_line ())) in
  match e with
    None -> failwith "Syntax error"
  | Some (e, _) -> (print_endline %
                      show_expr %
                        (fun x -> infer_type x [] []) %
                          (fun x -> to_deb x "" 0))
                     e
