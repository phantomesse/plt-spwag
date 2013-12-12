type action = Ast | Irgenerator | Compile

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);
			      ("-i", Irgenerator);
			      ("-c", Compile) ]
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  match action with
    Ast -> let listing = Ast.string_of_program program
    (* TODO: Implement Ast.string_of_program function in ast.ml *)
           in print_string listing
  | Irgenerator -> ignore (Irgenerator.run program)
  | Compile -> Compile.translate program
    (* TODO: Implement Compile.translate function in Compile.ml *)
 