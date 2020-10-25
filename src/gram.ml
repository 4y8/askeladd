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

let semicolon =
  blank (char ';')

let colon =
  blank (char ':')

let rec expr s =
  let app l r = App (l, r, Expl) in
  let arr l r = Pi ("", l, r, Expl) in
  let p = (set <|> lam <|> pi <|> letin <|> var) in
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
  let lam v t b = Lam (v, t, b, Expl) in
     (lam
  <$  keyword "fun"
  <*> blank ide
  <*> (opt None ((fun x -> Some x) <$  blank (char ':')
                                   <*> expr))
  <*  blank (word "=>")
  <*> expr) s

and pi s =
  let pi v t b = Pi (v, t, b, Expl) in
      (pi
  <$  sym '('
  <*> blank ide
  <*  blank (char ':')
  <*> expr
  <*  blank (sym ')')
  <*  blank (word "->")
  <*> expr) s

and set =
  let positive = int_of_string <$> (inplode <$> many1 digit) in
  let set n = Set (n) in
  set <$ keyword "Set" <*> (opt 0 positive)

and hole =
  Hole <$ (word "???")

and var =
  (fun v -> Var v) <$> ide

and case s =
  let pattern =
       (fun x y -> x, y)
   <$  blank (char '|')
   <*> expr
   <*  blank (word "->")
   <*> expr
   <*  semicolon
  in
  let case e l = Case (e, l) in
       (case
   <$  keyword "case"
   <*> blank expr
   <*  keyword "of"
   <*> many1 pattern) s

let data_decl =
  let constructor =
    let pair x y = x, y in
            pair
        <$  blank (char '|')
        <*> ide
        <*  colon
        <*> expr
        <*  semicolon
  in
  let data n t l = Data (n, t, l) in
      data
  <$  keyword "data"
  <*> blank ide
  <*  colon
  <*> expr
  <*  blank (keyword "where")
  <*> many1 (blank constructor)

let top_level =
  let rec wrap_lam l e =
    match l with
      [] -> e
    | hd :: tl -> Lam (hd, None, wrap_lam tl e, Expl)
  in
  let tdecl =
    let tdecl s e = TDecl (s, e) in
    tdecl <$> ide <* colon <*> expr
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
  (tdecl <|> fdecl <* semicolon) <|> data_decl
