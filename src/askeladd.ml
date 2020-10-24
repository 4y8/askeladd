open Combo

type expr
  = Var of string
  | Pi  of string * expr * expr
  | Lam of string * expr option * expr
  | App of expr * expr
  | Deb of int
  | Ann of expr * expr
  | Univ
  | Let of string * expr * expr
[@@deriving show]

type decl
  = TDecl of string * expr
  | FDecl of string * expr
  | Data  of string * expr * (string * expr) list
[@@deriving show]

let (%) f g x = f (g x)

let keywords = ["fun"; "Set"; "data"; "where"]

let is_keyword v = List.mem v keywords

let ide s =
  match (inplode <$> many1 (letter <|> char '_')) s with
    Some (v, _) when is_keyword v -> None
  | e -> e

let nul = space <|> newline <|> tab

let blank p =
  many nul *> p <* many nul

let keyword s = word s

let rec expr s =
  let lam =
    let lam v t b = Lam (v, t, b) in
        lam
    <$  keyword "fun"
    <*> blank ide
    <*> (opt None ((fun x -> Some x) <$  blank (char ':')
                                     <*> expr))
    <*  blank (word "=>")
    <*> expr
  in
  let univ = Univ <$ keyword "Set" in
  let pi =
    let pi v t b = Pi (v, t, b) in
        pi
    <$  sym '('
    <*> blank ide
    <*  blank (char ':')
    <*> expr
    <*  blank (sym ')')
    <*  blank (word "->")
    <*> expr
  in
  let var = (fun v -> Var v) <$> ide in
  let letin =
    let letin v e b = Let (v, e, b) in
        letin
    <$  keyword "let"
    <*> blank ide
    <*  blank (char '=')
    <*> blank expr
    <*  blank (keyword "in")
    <*> expr
  in
  let app l r = App (l, r) in
  let arr l r = Pi ("", l, r) in
  let p = (univ <|> lam <|> pi <|> letin <|> var) in
  let p = chainr1 (blank (arr <$ word "->")) (p
           <|> between (blank (word "(")) p (blank (word ")"))) in
  let app = chainl1 (spaces *> return app <* spaces) p in
  app s

let data_decl =
  let constructor =
    let pair x y = x, y in
            pair
        <$  blank (char '|')
        <*> ide
        <*  blank (char ':')
        <*> expr
        <*  blank (char ';')
  in
  let data n t l = Data (n, t, l) in
      data
  <$  keyword "data"
  <*> blank ide
  <*  blank (char ':')
  <*> expr
  <*  blank (keyword "where")
  <*> many1 (blank constructor)

let top_level =
  let rec wrap_lam l e =
    match l with
      [] -> e
    | hd :: tl -> Lam (hd, None, wrap_lam tl e)
  in
  let tdecl =
    let tdecl s e = TDecl (s, e) in
    tdecl <$> ide <* blank (char ':') <*> expr
  in
  let fdecl =
    let fdecl s e = FDecl (s, e) in
        fdecl
    <$> ide
    <*> (wrap_lam
     <$> (many (blank ide))
     <*  blank (char '=')
     <*> expr)
  in
  (tdecl <|> fdecl <* blank (char ';')) <|> data_decl

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
    Univ -> Univ
  | Var v ->
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
  | Univ, Univ -> true
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
  | Univ -> Univ
  | Pi (_, p, p') ->
     infer_universe c g p;
     infer_universe (p :: c) g p';
     Univ
  | Var v -> List.assoc v g
  | Let (v, t, b) ->
     let t = infer_type (t, None) c g in
     infer_type (b, None) c ((v, t) :: g)
  | App (l, r) ->
     let p, p' = infer_pi l c g in
     let tr = infer_type (r, None) c g in
     check_equal tr p;
     subst p' 0 r

and infer_universe c g e =
  let u = infer_type (e, None) c g  in
  match whnf c g u with
    Univ -> ()
  | _ -> failwith "expected type"

and infer_pi e c g =
  let t = infer_type (e, None) c g in
    match whnf c g t with
      Pi (_, p, p') -> p, p'
    | _ -> failwith "Expected a function"

let rec get_type_decl v l =
  match l with
    [] -> failwith "Untype declared varible"
  | TDecl (v', t) :: _ when v = v' -> to_deb t "" 0
  | _ :: tl -> get_type_decl v tl

let rec program l l' c =
  match l with
    [] -> c
  | FDecl (v, e) :: tl ->
     let t = get_type_decl v l' in
     let t = infer_type (to_deb e "" 0, Some t) [] c in
     program tl l' ((v, t) :: c)
  | Data (v, t, l) :: tl ->
     let c = (v, t) :: l @ c in
     program tl l' c
  | _ :: tl -> program tl l' c

let _ =
  let s = Sys.argv.(1) in
  let ic = open_in s in
  let s = explode (really_input_string ic (in_channel_length ic)) in
  let p = (many (blank top_level)) s in
  match p with
    None -> failwith "Syntax error"
  | Some (p, _) ->
     let c = program p p [] in
     List.iter (fun (v, t) -> Printf.printf "%s : %s\n" v (show_expr t)) c
