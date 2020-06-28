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
  = Var of char list
  | Pi  of char list * expr * expr
  | Lam of char list * expr * expr
  | App of expr * expr

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
  | _         -> raise Not_found
