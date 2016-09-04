open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let rec plant_mapper argv =
  (* Our getenv_mapper only overrides the handling of expressions in the default mapper. *)
  { default_mapper with
    structure_item = fun mapper structure_item ->
      match structure_item with
      (* Is this an extension node? *)
      | { pstr_desc = (Pstr_value (flag, binding)); pstr_loc} ->
             default_mapper.pat mapper binding;
      (* Delegate to the default mapper. *)
      | x -> default_mapper.structure_item mapper x;
  }

let () = register "plant" plant_mapper
