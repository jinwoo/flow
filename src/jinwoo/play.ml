(*
type item =
  | String of string
  | Bool of bool
  | Object of (string * item) list
  | Array of item list
  | Number of float
  | Null
  | Regexp of (Loc.t * string * string)  (* (loc, pattern, flags) *)

let indent_lines str =
  String.split_on_char '\n' str
  |> List.map (fun line -> "  " ^ line)
  |> String.concat "\n"

let string_of_item item =
  let rec aux item =
    match item with
    | String value -> "'" ^ value ^ "'"
    | Bool value -> string_of_bool value
    | Object props ->
      let payload =
        List.map (fun (key, value) -> key ^ ": " ^ aux value) props
        |> String.concat ",\n"
      in
      "{\n" ^ indent_lines payload ^ "\n}"
    | Array items ->
      let payload =
        List.map (fun item -> aux item) items
        |> String.concat ",\n"
      in
      "[\n" ^ indent_lines payload ^ "\n]"
    | Number num -> string_of_float num
    | Null -> "null"
    | Regexp (_, pattern, flags) -> Printf.sprintf "%s: %s" pattern flags
  in
  aux item

module MyTranslator : (Estree_translator.Translator with type t = item) =
struct
  type t = item
  let string value = String value
  let bool value = Bool value
  let obj value = Object value
  let array value = Array value
  let number value = Number value
  let null = Null
  let regexp loc pattern flags = Regexp (loc, pattern, flags)
end

module Translate = Estree_translator.Translate (MyTranslator) (struct
    let include_comments = true
    let include_locs = true
  end)

let parse content =
  let (ast, _) = Parser_flow.program ~fail:false content in
  let result = Translate.program ast in
  Printf.printf "%s\n" (string_of_item result)

let () = parse "1 + 2;
console.log('hello');
}"
*)

let play filename =
  let sig_cx = Context.make_sig () in
  (* TODO(jinwoo): Fine-tune options *)
  let metadata = {
    Context.
    checked = true;
    munge_underscores = false;
    verbose = None;
    weak = false;
    jsx = None;
    strict = true;
    enable_const_params = false;
    enforce_strict_call_arity = false;
    esproposal_class_static_fields = Options.ESPROPOSAL_IGNORE;
    esproposal_class_instance_fields = Options.ESPROPOSAL_IGNORE;
    esproposal_decorators = Options.ESPROPOSAL_IGNORE;
    esproposal_export_star_as = Options.ESPROPOSAL_IGNORE;
    esproposal_optional_chaining = Options.ESPROPOSAL_IGNORE;
    facebook_fbt = None;
    ignore_non_literal_requires = false;
    max_trace_depth = 100;
    root = Path.make filename;
    strip_root = false;
    suppress_comments = [];
    suppress_types = SSet.empty;
    max_workers = 10;
  }
  in
  let file_key = File_key.SourceFile filename in
  let cx = Context.make sig_cx metadata file_key "" in
  (* TODO(jinwoo):
     1. Parse & generate ast
     2. Call Type_inference_js.infer_ast
     3. Query_types.query_type
  *)
  cx
