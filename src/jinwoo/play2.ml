(* Most of the code is copied from flow_dot_js.ml *)

let error_of_parse_error source_file (loc, err) =
  let flow_err = Flow_error.EParseError (loc, err) in
  Flow_error.error_of_msg ~trace_reasons:[] ~source_file flow_err

let error_of_file_sig_error source_file e =
  let flow_err = File_sig.(match e with
      | IndeterminateModuleType loc -> Flow_error.EIndeterminateModuleType loc
    ) in
  Flow_error.error_of_msg ~trace_reasons:[] ~source_file flow_err

let parse_content file content =
  let parse_options = Some Parser_env.({
      (**
       * Always parse ES proposal syntax. The user-facing config option to
       * ignore/warn/enable them is handled during inference so that a clean error
       * can be surfaced (rather than a more cryptic parse error).
      *)
      esproposal_class_instance_fields = true;
      esproposal_class_static_fields = true;
      esproposal_decorators = true;
      esproposal_export_star_as = true;
      esproposal_optional_chaining = true;
      types = true;
      use_strict = false;
    }) in
  let ast, parse_errors =
    Parser_flow.program_file ~fail:false ~parse_options content (Some file)
  in
  if parse_errors <> [] then
    let converted = List.fold_left (fun acc parse_error ->
        Errors.ErrorSet.add (error_of_parse_error file parse_error) acc
      ) Errors.ErrorSet.empty parse_errors in
    Error converted
  else
    match File_sig.program ~ast with
    | Error e -> Error (Errors.ErrorSet.singleton (error_of_file_sig_error file e))
    | Ok fsig -> Ok (ast, fsig)

let load_lib_files ~master_cx ~metadata files
    save_parse_errors save_infer_errors save_suppressions save_lint_suppressions =
  (* iterate in reverse override order *)
  let _, result = List.rev files |> List.fold_left (

      fun (exclude_syms, result) file ->
        let lib_content = Sys_utils.cat file in
        let lib_file = File_key.LibFile file in
        match parse_content lib_file lib_content with
        | Ok (ast, file_sig) ->
          let sig_cx = Context.make_sig () in
          let cx = Context.make sig_cx metadata lib_file Files.lib_module_ref in
          Flow_js.mk_builtins cx;
          let syms = Type_inference_js.infer_lib_file cx ast
              ~exclude_syms ~file_sig ~lint_severities:LintSettings.empty_severities
          in

          Context.merge_into (Context.sig_cx master_cx) sig_cx;

          let () =
            let from_t = Context.find_module master_cx Files.lib_module_ref in
            let to_t = Context.find_module cx Files.lib_module_ref in
            Flow_js.flow_t master_cx (from_t, to_t)
          in

          let errors = Context.errors cx in
          let suppressions = Context.error_suppressions cx in
          let severity_cover = Context.severity_cover cx in

          Context.remove_all_errors cx;
          Context.remove_all_error_suppressions cx;
          Context.remove_all_lint_severities cx;

          save_infer_errors lib_file errors;
          save_suppressions lib_file suppressions;
          save_lint_suppressions lib_file severity_cover;

          (* symbols loaded from this file are suppressed
             if found in later ones *)
          let exclude_syms = SSet.union exclude_syms (SSet.of_list syms) in
          let result = (lib_file, true) :: result in
          exclude_syms, result

        | Error parse_errors ->
          save_parse_errors lib_file parse_errors;
          exclude_syms, ((lib_file, false) :: result)

    ) (SSet.empty, [])

  in result

let stub_docblock = {
  Docblock.
  flow = None;
  preventMunge = None;
  providesModule = None;
  isDeclarationFile = false;
  jsx = None;
}

let stub_metadata ~root ~checked = {
  Context.
  (* local *)
  checked;
  munge_underscores = false;
  verbose = None;
  weak = false;
  jsx = None;
  strict = false;

  (* global *)
  enable_const_params = false;
  enforce_strict_call_arity = true;
  esproposal_class_static_fields = Options.ESPROPOSAL_ENABLE;
  esproposal_class_instance_fields = Options.ESPROPOSAL_ENABLE;
  esproposal_decorators = Options.ESPROPOSAL_ENABLE;
  esproposal_export_star_as = Options.ESPROPOSAL_ENABLE;
  esproposal_optional_chaining = Options.ESPROPOSAL_ENABLE;
  facebook_fbt = None;
  ignore_non_literal_requires = false;
  max_trace_depth = 0;
  max_workers = 0;
  root;
  strip_root = true;
  suppress_comments = [];
  suppress_types = SSet.empty;
}

let get_master_cx =
  let master_cx = ref None in
  fun root ->
    match !master_cx with
    | Some (prev_root, cx) -> assert (prev_root = root); cx
    | None ->
      let sig_cx = Context.make_sig () in
      let cx = Context.make sig_cx
          (stub_metadata ~root ~checked:false)
          File_key.Builtins
          Files.lib_module_ref in
      Flow_js.mk_builtins cx;
      master_cx := Some (root, cx);
      cx

let set_libs filenames =
  let root = Path.dummy_path in
  let master_cx = get_master_cx root in
  let metadata = stub_metadata ~root ~checked:true in
  let _: (File_key.t * bool) list = load_lib_files
      ~master_cx
      ~metadata
      filenames
      (fun _file _errs -> ())
      (fun _file _errs -> ())
      (fun _file _sups -> ())
      (fun _file _lint -> ()) in

  Flow_js.Cache.clear();
  let reason = Reason.builtin_reason (Reason.RCustom "module") in
  let builtin_module = Obj_type.mk master_cx reason in
  Flow_js.flow_t master_cx (builtin_module, Flow_js.builtins master_cx);
  Merge_js.ContextOptimizer.sig_context master_cx [Files.lib_module_ref]

(* Currently supports only one file, as written below.
   TODO(jinwoo): Support multiple files *)

open Utils_js
module Reqs = Merge_js.Reqs

type file_info = {
  file_key: File_key.t;
  ast: Loc.t Ast.program;
  file_sig: File_sig.t;
}

let reqs_of_component file_keys required =
  let dep_cxs, reqs =
    List.fold_left (fun (dep_cxs, reqs) req ->
        let r, loc, resolved_r, file = req in
        Module_js.(match get_file Expensive.ok resolved_r with
            | Some (File_key.ResourceFile f) ->
              dep_cxs, Reqs.add_res (loc, f, file) reqs
            | Some dep ->
              let info = get_info_unsafe ~audit:Expensive.ok dep in
              if info.checked && info.parsed then
                (* checked implementation exists *)
                let m = Files.module_ref dep in
                if List.mem dep file_keys then
                  (* impl is part of component *)
                  dep_cxs, Reqs.add_impl (dep, m, loc, file) reqs
                else
                  (* look up impl sig_context *)
                  let leader = Context_cache.find_leader dep in
                  let dep_cx = Context_cache.find_sig leader in
                  dep_cx::dep_cxs, Reqs.add_dep_impl (dep_cx, m, loc, file) reqs
              else
                (* unchecked implementation exists *)
                dep_cxs, Reqs.add_unchecked (r, loc, file) reqs
            | None ->
              (* implementation doesn't exist *)
              dep_cxs, Reqs.add_decl (r, loc, resolved_r, file) reqs
          )
      ) ([], Reqs.empty) required
  in

  let master_cx = Context_cache.find_sig File_key.Builtins in

  master_cx, dep_cxs, reqs

let infer_and_merge ~root file_info_list =
  (* this is a VERY pared-down version of Merge_service.merge_strict_context.
     it relies on the JS version only supporting libs + 1 file, so every
     module you can require() must come from a lib; this skips resolving
     module names and just adds them all to the `decls` list. *)
  Flow_js.Cache.clear();
  let metadata = stub_metadata ~root ~checked:true in
  let master_cx = get_master_cx root in
  let (file_keys, required, file_sigs) =
    Nel.fold_left (fun (file_keys, required, file_sigs) file_info ->
        let {file_key; ast; file_sig} = file_info in
        let require_loc_map = File_sig.(require_loc_map file_sig.module_sig) in
        let required = SMap.fold (fun module_name locs acc ->
            (* TODO(jinwoo): Must resolve modules and call
               Module_js.add_parsed_resolved_modules from somewhere *)
            let m = Module_js.find_resolved_module ~audit:Expensive.ok
                file_key module_name in
            (module_name, locs, m, file_key) :: acc
          ) require_loc_map required
        in
        let file_sigs = FilenameMap.add file_key file_sig file_sigs in
        (file_key :: file_keys, required, file_sigs)
      ) ([], [], FilenameMap.empty) file_info_list
  in
  let master_cx, dep_cxs, file_reqs =
    reqs_of_component file_keys required
  in
  let lint_severities = LintSettings.empty_severities in
  let strict_mode = StrictModeSettings.empty in
  let file_sigs = FilenameMap.singleton filename file_sig in
  let cx, _other_cxs = Merge_js.merge_component_strict
      ~metadata ~lint_severities ~strict_mode ~file_sigs
      ~get_ast_unsafe:(fun _ -> ast)
      ~get_docblock_unsafe:(fun _ -> stub_docblock)
      (Nel.one filename) reqs [] (Context.sig_cx master_cx)
  in
  cx

let infer_and_merge_multiple ~root file_info_list = ()


let mk_loc file line col = {
  Loc.
  source = Some file;
  start = { Loc.line; column = col; offset = 0; };
  _end = { Loc.line; column = col + 1; offset = 0; };
}

let infer_type filename content line col =
  let filename = File_key.SourceFile filename in
  let root = Path.dummy_path in
  match parse_content filename content with
  | Error _ -> failwith "parse error"
  | Ok (ast, file_sig) ->
    let cx = infer_and_merge ~root filename ast file_sig in
    let loc = mk_loc filename line col in Query_types.(
        match query_type cx loc with
        | FailureNoMatch -> Loc.none, Error "No match"
        | FailureUnparseable (loc, _, _) -> loc, Error "Unparseable"
        | Success (loc, t) ->
          loc, Ok (Ty_printer.string_of_t ~force_single_line:true t)
      )

(* List of library files to load before other files *)
let lib_files = ["lib/core.js"; "lib/node.js"]

let file_content filename =
  let ic = open_in_bin filename in
  let length = in_channel_length ic in
  let buf = Buffer.create length in
  Buffer.add_channel buf ic length;
  Buffer.contents buf

let () =
  let _ = set_libs lib_files in
  let content = file_content "test.js" in
  let (_, result) = infer_type "test.js" content 15 30 in
  let answer = match result with
    | Ok result -> result
    | Error err -> "Error: " ^ err
  in
  Printf.printf "%s\n" answer
