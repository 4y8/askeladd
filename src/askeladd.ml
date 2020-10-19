open Combo

type token
  = FUN
  | PI
  | VAR of string
  | RPAR
  | LPAR
  | COL
  | STAR
  | DARR
  | SARR
  | EOL
  | EQUAL
  | LET
  | IN

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

let tokens = [
    "fun", FUN;
    "let", LET;
    "in",  IN;
    "pi",  PI;
    ":",   COL;
    "(",   LPAR;
    ")",   RPAR;
    "\n",  EOL;
    "=>",  DARR;
    "->",  SARR;
    "=",   EQUAL;
    "*",   STAR
  ]


let tok (s, t) =
  word s *> return t

let token =
  let var v = VAR v in
  choice (List.map tok tokens) <|> ((var % inplode) <$> (many alpha))

let rec tokens =
  many token

let rec expr s =
  let ide = function VAR v :: tl -> Some (v, tl) | _ -> None in
  let lam =
    let lam v t b = Lam (v, t, b) in
    lam
    <$> sym FUN
     *> ide
    <*  sym COL
    <*> expr
    <*  sym DARR
    <*> expr
  in
  let univ =
    sym STAR *> return (Univ 0)
  in 
  let pi =
    let pi v t b = Pi (v, t, b) in
    pi
    <$> sym LPAR
    *> ide
    <*  sym COL
    <*> expr
    <*  sym RPAR
    <*  sym SARR
    <*> expr
  in
  let var =
    let var v = Var v in
    var <$> ide
  in 
  (univ <|> lam <|> pi <|> var) s

let rec parse_til t e d =
  match t with
    d' :: tl when d' = d ->
    begin
      match e with
        None -> raise Not_found
      | Some e -> e, tl
    end
  | RPAR :: tl ->
    begin
      match e with
        None -> raise Not_found
      | Some e -> e, tl
    end
  | _ -> let e, tl = parser t e in
    parse_til tl (Some e) d

and parser t l =
  let r, tl =
    match t with
      FUN :: VAR v :: COL :: tl ->
      let ty, tl = parser tl None in
      let b,  tl =
        match tl with
          DARR :: tl -> parser tl None
        | _         -> raise Not_found
      in
      Lam(v, ty, b), tl
    | LPAR :: VAR v :: COL :: tl ->
      let ty, tl = parse_til tl None RPAR in
      let b,  tl =
        match tl with
          SARR :: tl -> parser tl None
        | _         -> raise Not_found
      in
      Pi(v, ty, b), tl
    | VAR v :: tl -> Var v, tl
    | LPAR :: tl -> parse_til tl None RPAR
    | STAR :: tl -> (Univ 0), tl
    | SARR :: tl ->
      let rec parse_type t =
        let r, tl = parser t None in
        match tl with
          SARR :: tl -> let lt, tl = parse_type tl in
          Pi("", r, lt), tl
        | tl -> r, tl
      in
      let lt, tl = parse_type tl in
      begin
        match l with
          None -> raise Not_found
        | Some l -> Pi("", l, lt), tl
      end
    | COL :: tl -> let r, tl = parser tl None in
      begin
        match l with
          None -> raise Not_found
        | Some l -> Texp(l, r), tl
      end
    | LET :: VAR v :: EQUAL :: tl ->
      let e, tl = parse_til tl None IN in
      let b, tl = parse_til tl None EOL in
      Let(v, e, b), tl
    | _ -> raise Not_found
  in
  let e =
    match l with
      None -> r
    | Some l ->
      match List.hd t with
        SARR | COL -> r
      | _ -> App(l, r)
  in
  if List.hd tl = SARR
  then parser tl (Some e)
  else e, tl

let rec to_deb e v n =
  match e with
    Var v' when v = v' -> Deb n
  | App(l, r) -> App(to_deb l v n, to_deb r v n)
  | Lam(v', t, b) -> Lam("", to_deb t v n, to_deb (to_deb b v' 0) v (n + 1))
  | Pi(v', t, b) -> Pi("", to_deb t v n, to_deb (to_deb b v' 0) v (n + 1))
  | Let(v', e, b) -> Let(v', to_deb e v n, to_deb b v n)
  | e -> e

let rec subst e n s =
  match e with
    Deb n' when n' = n -> s
  | App(l, r) -> App(subst l n s, subst r n s)
  | Lam(_, t, b) -> Lam("", subst t n s, subst b (n + 1) s)
  | Pi(_, t, b) -> Pi("", subst t n s, subst b (n + 1) s)
  | Let(v, e, b) -> Let(v, subst e n s, subst b n s)
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

let top_level t =
  match t with
    VAR v :: COL :: tl -> let t, tl = parse_til tl None EOL in
    TDecl(v, t), tl
  | VAR v :: EQUAL :: tl -> let t, tl = parse_til tl None EOL in
    FDecl(v, t), tl
  | _ -> raise Not_found

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
        Pi(_, fl, fr) when fl = rt ->
        subst fr 0 r
      | _ -> raise Not_found
    end
  | Univ n -> (Univ (n + 1))
  | Texp(_, t) -> t
  | Var v -> List.assoc v gc
  | Let(v, e, b) ->
    get_type b c ((v, get_type e c gc) :: gc)
