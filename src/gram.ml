type expr
  = Var of string
  | App of expr * expr
  | Abs of string * expr
  | Let of string * expr * expr * expr
  | Pi  of string * expr * expr
  | Set

type value
  = VGen of int
  | VApp of value * value
  | VSet
  | VClo of env * expr
and env = (string * value) list
