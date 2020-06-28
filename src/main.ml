type token
  = FUN
  | PI
  | VAR of char list
  | RPAR
  | LPAR
  | COL
  | STAR
  | ARR

type expr
  = Var  of char list
  | Pi   of char list * expr * expr
  | Lam  of char list * expr * expr
  | App  of expr * expr
  | Deb  of int
  | Star

let rec lexer s pos =
  let is_alpha c =
    (Char.code('A') <= Char.code c) && (Char.code('Z') >= Char.code c) ||
    (Char.code('a') <= Char.code c) && (Char.code('z') >= Char.code c)
  in
  match s with
    [] -> []
  | 'f' :: 'u' :: 'n' :: tl -> FUN :: (lexer tl (pos + 3))
  | 'p' :: 'i' :: tl -> PI :: (lexer tl (pos + 2))
  | a :: _ when is_alpha a ->
    let rec parse_var s =
      match s with
        a :: tl when is_alpha a ->
        let s, tl = parse_var tl in
        a :: s, tl
      | _ -> [], s
    in
    let v, tl = parse_var s in
    (VAR v) :: (lexer tl (pos + (List.length v)))
  | ':' :: tl -> COL  :: (lexer tl (pos + 1))
  | '(' :: tl -> LPAR :: (lexer tl (pos + 1))
  | ')' :: tl -> RPAR :: (lexer tl (pos + 1))
  | '*' :: tl -> STAR :: (lexer tl (pos + 1))
  | '-' :: '>' :: tl -> ARR :: (lexer tl (pos + 2))
  | c :: _ -> raise Not_found

let rec parser t =
  match t with
    FUN :: VAR v :: COL :: tl ->
    let ty, tl = parser tl in
    let b,  tl =
      match tl with
        ARR :: tl -> parser tl
      | _         -> raise Not_found
    in
    Lam(v, ty, b), tl
  | PI :: VAR v :: COL :: tl ->
    let ty, tl = parser tl in
    let b,  tl =
      match tl with
        ARR :: tl -> parser tl
      | _         -> raise Not_found
    in
    Pi(v, ty, b), tl
  | VAR v :: tl -> Var v, tl
  | LPAR :: tl ->
    let rec parse_par t e =
      match t with
        RPAR :: tl -> e, tl
      | _ -> let ne, tl = parser t in
        parse_par tl (App(e, ne))
    in
    let e, tl = parser tl in
    parse_par tl e
  | STAR :: tl -> Star, tl
  | _ -> raise Not_found

let rec to_deb e v n =
  match e with
    Var v' when v = v' -> Deb n
  | App(l, r) -> App(to_deb l v n, to_deb r v n)
  | Lam(v', t, b) -> Lam([], to_deb t v n, to_deb (to_deb b v' 0) v (n + 1))
  | Pi(v', t, b) -> Pi([], to_deb t v n, to_deb (to_deb b v' 0) v (n + 1))
  | e -> e

let rec subst e n s =
  match e with
    Deb n' when n' = n -> s
  | App(l, r) -> App(subst l n s, subst r n s)
  | Lam(_, t, b) -> Lam([], subst t n s, subst t (n + 1) s)
  | Pi(_, t, b) -> Pi([], subst t n s, subst t (n + 1) s)
  | e -> e

let rec relocation e i =
  match e with
    Pi(_, t, b) -> Pi([], relocation t i, relocation b (i + 1))
  | Lam(_, t, b) -> Lam([], relocation t i, relocation b (i + 1))
  | Deb k when k >= i -> Deb(k + 1)
  | App(l, r) -> App(relocation l i, relocation r i)
  | e -> e

let relocate_ctx =
  List.map (fun e -> relocation e 0)

let rec get_type e c =
  match e with
    Deb n -> (List.nth c n)
  | Star -> Star
  | Lam(_, t, b) -> let bt = get_type b (relocate_ctx (t :: c)) in
    Pi([], t, bt)
  | Pi(_, t, b) -> let tt = get_type t c in
    if tt = Star
    then let bt = get_type b (relocate_ctx (t :: c)) in
      if bt = Star
      then Star
      else raise Not_found
    else raise Not_found
  | App(l, r) ->
    begin
      let rt = get_type r c in
      match get_type l c with
        Pi(_, fl, fr) when fr = rt ->
        subst fr 0 rt
      | _ -> raise Not_found
    end
  | _ -> raise Not_found
