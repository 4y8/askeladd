open Combo

type expr
  = Var  of string
  | Pi   of string * expr * expr
  | Lam  of string * expr * expr
  | App  of expr * expr
  | Deb  of int
  | Texp of expr * expr
  | Univ of int
  | Let  of string * expr * expr

type decl
  = TDecl of string * expr
  | FDecl of string * expr

let (%) f g x = f (g x)

let rec expr s =
  let ide = inplode <$> many alpha in
  let lam =
    let lam v t b = Lam (v, t, b) in
    lam
    <$> word "fun"
     *> ide
    <*  sym ':'
    <*> expr
    <*  word "=>"
    <*> expr
  in
  let univ = sym '*' *> return (Univ 0) in
  let pi =
    let pi v t b = Pi (v, t, b) in
    pi
    <$> sym '('
     *> ide
    <*  sym ':'
    <*> expr
    <*  sym ')'
    <*  word "->"
    <*> expr
  in
  let var =
    let var v = Var v in
    var <$> ide
  in
  let letin =
    let letin v e b = Let (v, e, b) in
    letin
    <$> word "let"
     *> ide
    <*  sym '='
    <*> expr
    <*  word "in"
    <*> expr
  in
  let app =
    let app l r = App (l, r) in
    app <$> expr <*> expr
  in
  let arr =
    let arr l r = Pi ("", l, r) in
    chainl (arr <$ word "->") expr
  in
  (univ <|> lam <|> pi <|> var <|> letin <|> app <|> arr) s

let rec to_deb e v n =
  match e with
    Var v' when v = v' -> Deb n
  | App (l, r) -> App (to_deb l v n, to_deb r v n)
  | Lam (v', t, b) -> Lam ("", to_deb t v n, to_deb (to_deb b v' 0) v (n + 1))
  | Pi (v', t, b) -> Pi ("", to_deb t v n, to_deb (to_deb b v' 0) v (n + 1))
  | Let (v', e, b) -> Let (v', to_deb e v n, to_deb b v n)
  | e -> e

let rec subst e n s =
  match e with
    Deb n' when n' = n -> s
  | App (l, r) -> App (subst l n s, subst r n s)
  | Lam (_, t, b) -> Lam("", subst t n s, subst b (n + 1) s)
  | Pi (_, t, b) -> Pi("", subst t n s, subst b (n + 1) s)
  | Let (v, e, b) -> Let(v, subst e n s, subst b n s)
  | e -> e

let rec relocation e i =
  match e with
    Pi(_, t, b) -> Pi("", relocation t i, relocation b (i + 1))
  | Lam(_, t, b) -> Lam("", relocation t i, relocation b (i + 1))
  | Let(v, e, b) -> Let(v, relocation e i, relocation b i)
  | Deb k when k >= i -> Deb(k + 1)
  | App(l, r) -> App(relocation l i, relocation r i)
  | e -> e

let relocate_ctx =
  List.map (fun e -> relocation e 0)

let is_type e =
  match e with
    Univ _ -> true
  | _      -> false

let get_univ e =
  match e with
    Univ n -> n
  | _      -> -1

let rec get_type e c gc =
  match e with
    Deb n -> (List.nth c n)
  | Lam(_, t, b) -> let bt = get_type b (relocate_ctx (t :: c)) gc in
    Pi("", t, bt)
  | Pi(_, t, b) -> let tt = get_type t c gc in
    let tu = get_univ tt in
    if tu <> -1
    then
      let bt = get_type b (relocate_ctx (t :: c)) gc in
      let bu = get_univ bt in
      if bu <> -1
      then (Univ(max tu bu))
      else raise Not_found
    else raise Not_found
  | App(l, r) ->
    begin
      let rt = get_type r c gc in
      match get_type l c gc with
        Pi (_, fl, fr) when fl = rt ->
        subst fr 0 r
      | _ -> raise Not_found
    end
  | Univ n -> (Univ (n + 1))
  | Texp(_, t) -> t
  | Var v -> List.assoc v gc
  | Let(v, e, b) ->
    get_type b c ((v, get_type e c gc) :: gc)
