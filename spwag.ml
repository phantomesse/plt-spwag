(* Author: Aftab Khan, Lauren Zou, Yunhe (John) Wang *)

type action = Ast | Irgenerator | Compile

external preprocess: unit -> string = "caml_preprocess"

let _ =
    let action =
        if Array.length Sys.argv > 1 then
            List.assoc Sys.argv.(1) [
                ("-a", Ast);
                ("-i", Irgenerator);
                ("-c", Compile)
            ]
        else Compile in
            let processed_code = stdin (* preprocess() *) in
            let lexbuf = Lexing.from_channel processed_code in
            let program = Parser.program Scanner.token lexbuf in
                match action with
                | Ast -> let listing = Ast.string_of_program program
                         in print_string listing
                | Irgenerator -> let sast = Sastinjector.inject program 
                                 in let ir = Irgenerator.generate sast
                                 in let output = Compile.compile ir
								 in print_string output
                | Compile -> (*Compile.compile program*) print_string "HI"
