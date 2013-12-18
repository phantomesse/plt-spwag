open Compile

type action = Ast | Irgenerator | Compile

let _ =
    let action =
        if Array.length Sys.argv > 1 then
            List.assoc Sys.argv.(1) [
                ("-a", Ast);
                ("-i", Irgenerator);
                ("-c", Compile)
            ]
        else Compile in
            let lexbuf = Lexing.from_channel stdin in
            let program = Parser.program Scanner.token lexbuf in
                match action with
                | Ast -> let listing = Ast.string_of_program program
                         in print_string listing
                | Irgenerator -> (* let sast = Semantic_analyzer.evalprogram program symbol_table
                                 in *) (*let ir = Irgenerator.generate program (* sast *) (* for testing purposes only *)
                                 in print_string ir*) print_string "HI" 
                | Compile -> (*Compile.compile program*) print_string "HI"