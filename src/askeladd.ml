open Combo
open Syntax
open Gram
open TT

let rec get_type_decl v l =
  match l with
    [] -> failwith "Untype declared varible"
  | TDecl (v', t) :: _ when v = v' -> to_deb t "" 0
  | _ :: tl -> get_type_decl v tl

let rec program l l' c =
  match l with
    [] -> c
  | FDecl (v, e) :: tl ->
     let t = get_type_decl v l' in
     let t = infer_type (to_deb e "" 0, Some t) [] c in
     program tl l' ((v, t) :: c)
  | Data (v, t, l) :: tl ->
     let c = (v, t) :: l @ c in
     program tl l' c
  | _ :: tl -> program tl l' c

let _ =
  let s = Sys.argv.(1) in
  let ic = open_in s in
  let s = explode (really_input_string ic (in_channel_length ic)) in
  let p = (many (blank top_level)) s in
  match p with
    None -> failwith "Syntax error"
  | Some (p, _) ->
     let c = program p p [] in
     List.iter (fun (v, t) -> Printf.printf "%s : %s\n" v (show_expr t)) c
