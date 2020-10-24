type expr
  = Var of string
  | Pi  of string * expr * expr
  | Lam of string * expr option * expr
  | App of expr * expr
  | Deb of int
  | Ann of expr * expr
  | Univ
  | Let of string * expr * expr
  | Case of expr * (expr * expr) list
[@@deriving show]

type decl
  = TDecl of string * expr
  | FDecl of string * expr
  | Data  of string * expr * (string * expr) list
[@@deriving show]
