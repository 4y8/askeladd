(* Explicitness of the expression. *)
type icit
  = Expl
  | Impl
[@@deriving show]

type expr
  = Var of string                             (* v *)
  | Pi  of string * expr * expr * icit        (* (x:A) -> B or {x:A} -> B *)
  | Lam of string * expr option * expr * icit (* \x(:A).B   or \{x}(:A).B *)
  | App of expr * expr * icit                 (* t u or t{u} *)
  | Deb of int                                (* n, De Bruijn indice *)
  | Ann of expr * expr                        (* x:t *)
  | Set of int                                (* Set^n *)
  | Let of string * expr * expr               (* let x = e in b *)
  | Case of expr * (expr * expr) list         (* case e of {p -> b} *)
  | Hole                                      (* ??? *)
[@@deriving show]

type decl
  = TDecl of string * expr
  | FDecl of string * expr
  | Data  of string * expr * (string * expr) list
[@@deriving show]
