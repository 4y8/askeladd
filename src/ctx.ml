open Syntax

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

let closeVal ctx t =
  Closure (ctx.env, Evaluation.quote (ctx.lvl + 1) t)
