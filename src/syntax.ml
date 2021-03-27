type icit
  = Exp
  | Imp

type origin = Inserted | Source

type raw
  = RHole
  | RSet
  | RLam of string * icit * raw
  | RVar of string
  | RApp of raw * raw * icit
  | RPi  of string * icit * raw * raw
  | RLet of string * raw * raw * raw

type vstate
  = Bound
  | Defined

type metavar = MetaVar of int

type term
  = Var  of int
  | Lam  of string * icit * term
  | App  of term * term * icit
  | Set
  | Pi   of string * icit * term * term
  | Let  of string * term * term * term
  | Meta of metavar
  | InsertedMeta of metavar * (vstate list)

type spine = (value * icit) list
and env = value list
and closure = Closure of env * term
and value
  = VFlex  of metavar * spine
  | VRigid of int * spine
  | VLam   of string * icit * closure
  | VPi    of string * icit * value * closure
  | VSet

