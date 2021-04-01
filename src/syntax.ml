type icit
  = Exp
  | Imp
[@@deriving show]

type origin = Inserted | Source
[@@deriving show]

type raw
  = RHole
  | RSet
  | RLam of string * icit * raw
  | RVar of string
  | RApp of raw * raw * icit
  | RPi  of string * icit * raw * raw
  | RLet of string * raw * raw * raw
[@@deriving show]

type metavar = MetaVar of int
[@@deriving show]

type term
  = Var of int
  | Lam of string * icit * term
  | App of term * term * icit
  | Set
  | Pi of string * icit * term * term
  | Let of string * term * term * term
  | Meta of metavar
  | InsertedMeta of metavar * env
and spine = (value * icit) list
and env_val = Bound of value | Defined of value
and env = {
    vals: env_val list;
    types: (string * origin * value) list
  }
and closure = Closure of (value list) * term
and value
  = VFlex  of metavar * spine
  | VRigid of int * spine
  | VLam   of string * icit * closure
  | VPi    of string * icit * value * closure
  | VSet
[@@deriving show]
