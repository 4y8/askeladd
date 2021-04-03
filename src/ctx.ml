open Syntax

type context = {
    lvl: int;
    env: env;
  }

let bind {lvl; env = {vals; types}} name t a =
  { lvl = lvl + 1;
    env = { vals = Bound (VRigid (lvl, []), a) :: vals;
            types = (name, Source, t) :: types
          }
  }

let newBinder {lvl; env = {vals; types}} name t a =
  { lvl = lvl + 1;
    env = { vals = Bound (VRigid (lvl, []), a) :: vals;
            types = (name, Inserted, t) :: types
          }
  }

let define {lvl; env = {vals; types}} name v t a =
  { lvl = lvl + 1;
    env = { vals = Defined (v, a) :: vals;
            types = (name, Source, t) :: types
          }
  }

let vals ctx =
  let rec aux acc = function
      [] -> List.rev acc
    | Defined (v, _) :: tl | Bound (v, _) :: tl -> aux (v :: acc) tl 
  in aux [] ctx.env.vals

let closeVal ctx t =
  Closure (vals ctx, Evaluation.quote (ctx.lvl + 1) t)

let emptyCtx = {env = {vals = []; types = []}; lvl = 0 }
