open Syntax

type meta_entry
  = Solved of value
  | Unsolved

let nextMeta = ref 0

let metaContext : (int * meta_entry) list ref = ref []

let lookupMeta (MetaVar m) =
  List.assoc m (!metaContext)

let resetMeta =
  nextMeta := 0;
  metaContext := []
