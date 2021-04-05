open Syntax

type context = {
    lvl: int;
    env: env;
  }

let bind {lvl; env = {vals; types}} name t =
  { lvl = lvl + 1;
    env = { vals = Bound (VRigid (lvl, [])) :: vals;
            types = (name, Source, t) :: types
          }
  }

let newBinder {lvl; env = {vals; types}} name t =
  { lvl = lvl + 1;
    env = { vals = Bound (VRigid (lvl, [])) :: vals;
            types = (name, Inserted, t) :: types
          }
  }

let define {lvl; env = {vals; types}} name v t =
  { lvl = lvl + 1;
    env = { vals = Defined v :: vals;
            types = (name, Source, t) :: types
          }
  }

let vals ctx =
  let rec aux acc = function
      [] -> List.rev acc
    | Defined v :: tl | Bound v :: tl -> aux (v :: acc) tl 
  in aux [] ctx.env.vals

let closeVal ctx t =
  Closure (vals ctx, Evaluation.quote (ctx.lvl + 1) t)

let emptyCtx = {env = {vals = []; types = []}; lvl = 0 }
