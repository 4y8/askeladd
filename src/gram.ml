open Combo
open Syntax

let keywords = ["fun"; "Set"; "data"; "where"]

let is_keyword v = List.mem v keywords

let ide s =
  match (inplode <$> many1 (letter <|> char '_')) s with
    Some (v, _) when is_keyword v -> None
  | e -> e

let nul = space <|> newline <|> tab

let blank p =
  many nul *> p <* many nul

let keyword v s =
  let s' = (inplode <$> many1 (letter <|> char '_')) s in
  match s' with
    Some (v', _) when v' <> v -> None
  | e -> e

let rec expr s =
  let app l r = App (l, r) in
  let arr l r = Pi ("", l, r) in
  let p = (univ <|> lam <|> pi <|> letin <|> var) in
  let p = chainr1 (blank (arr <$ word "->")) (p
           <|> between (blank (word "(")) p (blank (word ")"))) in
  let app = chainl1 (spaces *> return app <* spaces) p in
  app s

and letin s =
  let letin v e b = Let (v, e, b) in
     (letin
  <$  keyword "let"
  <*> blank ide
  <*  blank (char '=')
  <*> blank expr
  <*  blank (keyword "in")
  <*> expr) s

and lam s =
  let lam v t b = Lam (v, t, b) in
     (lam
  <$  keyword "fun"
  <*> blank ide
  <*> (opt None ((fun x -> Some x) <$  blank (char ':')
                                   <*> expr))
  <*  blank (word "=>")
  <*> expr) s

and pi s =
  let pi v t b = Pi (v, t, b) in
      (pi
  <$  sym '('
  <*> blank ide
  <*  blank (char ':')
  <*> expr
  <*  blank (sym ')')
  <*  blank (word "->")
  <*> expr) s

and univ =
  Univ <$ keyword "Set"

and var =
  (fun v -> Var v) <$> ide

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
