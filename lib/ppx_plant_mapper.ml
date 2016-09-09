open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Location
open Lexing

let position_to_string pos =
  let (name, l, r) = Location.get_pos_info pos in
  Printf.sprintf "file_name : %s, pos : %d, %d\n" name l r

let location_to_string location =
  let pos_start = position_to_string location.loc_start in
  let pos_end =  position_to_string location.loc_end in
  let ghost = location.loc_ghost in
  Printf.sprintf "Pos_start: '%s', Pos_end:'%s', ghost: %b" pos_start pos_end ghost

let expressions_belonging_to_structure_item plant_model=
    {default_mapper with
       expr = fun mapper expr ->
          match expr with
            | {pexp_desc =
                Pexp_apply (pexp_ident, expression_list)} ->
                  begin
                    match pexp_ident.pexp_desc with
                      | Pexp_ident longident ->
                        let () = Plant_model.log plant_model longident in
                        default_mapper.expr mapper expr
                      | _ ->
                        default_mapper.expr mapper expr
                  end
            | {pexp_desc =
                Pexp_send (sub_exp, fun_name )} ->
                  begin
                    match sub_exp.pexp_desc with
                      | Pexp_ident longident ->
                        let () = Plant_model.log plant_model longident in
                        default_mapper.expr mapper expr
                      | _ ->
                        default_mapper.expr mapper expr
                  end
            | x -> default_mapper.expr mapper x
    }

let handle_patterns value_bindings =
  List.fold_left (fun acc binding ->
      match binding.pvb_pat.ppat_desc with
        | Ppat_var {txt=label;loc}  -> Some label
        | _ -> acc
    ) None value_bindings

let plant_mapper argv =
  (* Our getenv_mapper only overrides the handling of expressions in the default mapper. *)
  { default_mapper with
    structure_item = fun mapper structure_item ->
      match structure_item with
      | { pstr_desc = (Pstr_value (flag, bindings)); pstr_loc} ->
             let si_name_opt = handle_patterns bindings in
             let si_name = BatOption.get si_name_opt in
             let file_name =  pstr_loc.loc_start.pos_fname in
             let plant_model = Plant_model.create_without_module file_name si_name in
             default_mapper.structure_item (expressions_belonging_to_structure_item plant_model) structure_item
      | x -> default_mapper.structure_item mapper x;
  }

(*let () = register "plant" plant_mapper*)
let run_main mapper =
  try
    let a = Sys.argv in
    let n = Array.length a in
    if n > 2 then
      apply ~source:a.(n - 2) ~target:a.(n - 1) @@ mapper (Array.to_list (Array.sub a 1 (n - 3)))
    else begin
      Printf.eprintf "Usage: %s [extra_args] <infile> <outfile>\n%!"
                     Sys.executable_name;
      exit 2
    end
  with exn ->
    prerr_endline (Printexc.to_string exn);
    exit 2

let () = run_main plant_mapper
