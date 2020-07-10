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

type un_expr
  = UnVar of string
  | UnLam of un_expr
  | UnApp of un_expr * un_expr
  | UnDeb of int
  | UnLet of string * un_expr * un_expr
  | Erased

type decl
  = TDecl of string * expr
  | FDecl of string * expr

let rec string_to_char_list s =
  match s with
    "" -> []
  | _  ->
    (String.get s 0) ::
    (string_to_char_list (String.sub s 1 (String.length s - 1)))

let rec char_list_to_string s =
  match s with
    [] -> ""
  | hd :: tl ->
    (String.make 1 hd) ^ (char_list_to_string tl)

let rec lexer s pos =
  let is_alpha c =
    (Char.code('A') <= Char.code c) && (Char.code('Z') >= Char.code c) ||
    (Char.code('a') <= Char.code c) && (Char.code('z') >= Char.code c)
  in
  match s with
    [] -> []
  | 'f' :: 'u' :: 'n' :: tl -> FUN :: (lexer tl (pos + 3))
  | 'l' :: 'e' :: 't' :: tl -> LET :: (lexer tl (pos + 3))
  | 'p' :: 'i' :: tl -> PI :: (lexer tl (pos + 2))
  | 'i' :: 'n' :: tl -> IN :: (lexer tl (pos + 2))
  | a :: _ when is_alpha a ->
    let rec parse_var s =
      match s with
        a :: tl when is_alpha a ->
        let s, tl = parse_var tl in
        a :: s, tl
      | _ -> [], s
    in
    let v, tl = parse_var s in
    (VAR (char_list_to_string v)) :: (lexer tl (pos + (List.length v)))
  | ':' :: tl -> COL  :: (lexer tl (pos + 1))
  | '(' :: tl -> LPAR :: (lexer tl (pos + 1))
  | ')' :: tl -> RPAR :: (lexer tl (pos + 1))
  | '*' :: tl -> STAR :: (lexer tl (pos + 1))
  | '\n' :: tl -> EOL :: (lexer tl (pos + 1))
  | '=' :: '>' :: tl -> DARR :: (lexer tl (pos + 2))
  | '-' :: '>' :: tl -> SARR :: (lexer tl (pos + 2))
  | ' ' :: tl | '\t' :: tl -> (lexer tl (pos + 1))
  | '=' :: tl -> EQUAL :: (lexer tl (pos + 1))
  | c :: _ -> raise Not_found

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
  | Let(v', e, b) -> Let(v', to_deb e v n, to_deb e v n)
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

let rec erase_type e =
  match e with
    Pi _
  | Texp _
  | Univ _ -> Erased
  | Var v -> UnVar v
  | Deb n -> UnDeb n
  | Lam(_, _, b) -> UnLam(erase_type b)
  | App(l, r) -> UnApp(erase_type l, erase_type r)
  | Let(v, e, b) -> UnLet(v, erase_type e, erase_type b)


let rec is_comb e =
  match e with
    UnVar "I"
  | UnVar "K"
  | UnVar "S"
  | UnVar "U"
  | UnVar "Z"
  | UnVar "B"
  | UnVar "C"
  | UnVar "C'"
  | UnVar "B*"
  | UnVar "S'" -> true
  | UnApp(l, r) -> (is_comb l) && (is_comb r)
  | _ -> false

let opt e =
  match e with
    UnApp(UnApp(UnVar "S", UnApp(UnVar "K", p)), UnApp(UnVar "K", q)) ->
    UnApp(UnVar "K", UnApp(p, q))
  | UnApp(UnApp(UnVar "S", UnApp(UnVar "K", p)), UnVar "I") -> p
  | UnApp(UnApp(UnVar "S", UnApp(UnVar "K", p)), UnApp(UnApp(UnVar "B", q), r)) ->
    UnApp(UnApp(UnApp(UnVar "B*", p), q), r)
  | UnApp(UnApp(UnVar "S", UnApp(UnVar "K", p)), q) ->
    UnApp(UnApp(UnVar "B", p), q)
  | UnApp(UnApp(UnVar "S",  UnApp(UnApp(UnVar "B", p), q)), UnApp(UnVar "K", r)) ->
    UnApp(UnApp(UnApp(UnVar "C'", p), q), r)
  | UnApp(UnApp(UnVar "S",  p), UnApp(UnVar "K", q)) ->
    UnApp(UnApp(UnVar "C", p), q)
  | UnApp(UnApp(UnVar "S",  UnApp(UnApp(UnVar "B", p), q)), r) ->
    UnApp(UnApp(UnApp(UnVar "S'", p), q), r)
  | e -> e

let rec no_in_lam e =
  match e with
    UnDeb _
  | UnVar _
  | Erased -> true
  | UnLet(_, l, r)
  | UnApp(l, r) -> (no_in_lam l) && (no_in_lam r)
  | UnLam _ -> false

let rec abs e =
  match e with
    UnDeb 0 -> UnVar "I"
  | UnApp(l, r) -> opt (UnApp(UnApp(UnVar "S", abs l), abs r))
  | e ->
    let rec dec_deb e =
      match e with
        UnDeb n -> UnDeb (n - 1)
      | UnApp(l, r) -> UnApp(dec_deb l, dec_deb r)
      | UnLam b -> UnLam(dec_deb b)
      | UnLet(v, e, b) -> UnLet(v, dec_deb e, dec_deb b)
      | n -> n
    in
    UnApp(UnVar "K", dec_deb e)

let rec brack e c =
  match e with
    UnApp(UnApp(UnVar "S", UnVar "K"), _) -> UnApp(UnVar "S", UnVar "K")
  | UnApp(UnApp(m, l), UnApp(n, l')) when l = l' && is_comb m && is_comb n ->
    UnApp(UnApp(UnApp(UnVar "S", m), n), l)
  | UnApp(l, r) -> UnApp(brack l c, brack r c)
  | UnLam b -> abs (brack b c)
  | UnLet(v, e, b) -> brack b ((v, brack e c) :: c)
  | UnVar v when not (is_comb e) -> List.assoc v c
  | Erased -> UnVar "I"
  | e -> e

let ski_to_n v =
  match v with
    "S" -> 0
  | "K" -> 1
  | "U" -> 2
  | "Z" -> 3
  | "I" -> 4
  | "B" -> 5
  | "C" -> 6
  | "S'" -> 7
  | "B*" -> 8
  | "C'" -> 9
  | _ -> raise Not_found (* Shouldn't happend *)

let rec arr e i =
  match e with
    UnVar v -> [ski_to_n v], []
  | UnApp(UnVar v, UnVar v') -> [ski_to_n v; ski_to_n v'], []
  | UnApp(e, UnVar v) -> let e, l = arr e (i + 2) in
    [i; ski_to_n v], e @ l
  | UnApp(UnVar v, e) -> let e, l = arr e (i + 2) in
    [ski_to_n v; i], e @ l
  | UnApp(e, e') -> let e, l = arr e (i + 4) in
    let e', l' = arr e' (i + 4 + (List.length l)) in
    [i; i + 2], e @ e' @ l @ l'
  | _ -> raise Not_found (* Shouldn't happend *)

let comp s =
  let t = lexer (string_to_char_list s) 0 in
  let p = fst (parser t None) in
  let d = to_deb p "" 0 in
  let un = erase_type d in
  let ski = brack un [] in
  let a = arr ski 10 in
  let f = List.fold_left (fun x y ->  (string_of_int y) ^ " " ^ x) "" (fst a) in
  let s = List.fold_left (fun x y ->  (string_of_int y) ^ " " ^ x) "" (snd a) in
  (String.sub f 0 (max (String.length f - 1) 0)) ^ "
" ^ (String.sub s 0 (max (String.length s - 1) 0))

let _ = print_string(comp (read_line() ^ "\n"))
