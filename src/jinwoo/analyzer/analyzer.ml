let main pid =
  Printf.eprintf "Spawned flow server (pid=%d)\n%!" pid;

  let type_at_command = TypeAtPosCommand.command in
  let args = ["--root"; "/Users/jinwoo/src/flow-exp"; "/Users/jinwoo/src/flow-exp/index.js"; "3"; "16"] in
  CommandSpec.args_of_argv type_at_command args
  |> CommandSpec.run type_at_command

let () =
  (* Anlz_init.init ~on_spawn:main (Path.make (Sys.getcwd ())); *)
  Anlz_init.init ~on_spawn:main (Path.make "/Users/jinwoo/src/flow-exp");
  Printf.printf "Server is running\n%!"

(* let () = Anlz_ast.print_ast "test.js" *)

(* let () =
   let type_at_command = TypeAtPosCommand.command in
   let args = ["--root"; "/Users/jinwoo/src/flow-exp"; "/Users/jinwoo/src/flow-exp/index.js"; "3"; "16"] in
   CommandSpec.args_of_argv type_at_command args
   |> CommandSpec.run type_at_command *)
