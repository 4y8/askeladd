type icit
  = Exp
  | Imp

type origin = Inserted | Source

type raw
  = RHole
  | RSet
  | RLam of string * icit * raw
  | RVar of string
  | RApp of raw * raw
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

type context = {
    lvl: int;
    bds: vstate list;
    types: (string * origin * value) list;
    env: value list;
  }

let bind {lvl; bds; types; env} name t =
  { env = (VRigid (lvl, []) :: env);
    lvl = lvl + 1;
    bds = Bound :: bds;
    types = (name, Source, t) :: types
  }

let newBinder {lvl; bds; types; env} name t =
  { env = (VRigid (lvl, []) :: env);
    lvl = lvl + 1;
    bds =Bound :: bds;
    types = (name, Inserted, t) :: types
  }

let define {lvl; bds; types; env} name v t =
  { env = v :: env;
    lvl = lvl + 1;
    bds = Defined :: bds;
    types = (name, Source, t) :: types
  }
