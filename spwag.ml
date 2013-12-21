(* Author: Aftab Khan, Lauren Zou Contributor: Yunhe (John) Wang *)

type action = Ast | Irgenerator | Preprocessor | Compile

external preprocess: unit -> string = "caml_preprocess"

let _ =
    let action =
        if Array.length Sys.argv > 1 then
            List.assoc Sys.argv.(1) [
                ("-a", Ast);
                ("-i", Irgenerator);
				("-p", Preprocessor);
                ("-c", Compile)
            ]
        else Irgenerator 
	in
	(match action with
	    Ast ->
			let lexbuf = Lexing.from_channel stdin in
		 	let program = Parser.program Scanner.token lexbuf in
			let listing = Ast.string_of_program program
			in print_string listing
	    | Irgenerator ->
			let lexbuf = Lexing.from_channel stdin in
			let program = Parser.program Scanner.token lexbuf in
			let sast = Sastinjector.inject program in
			let ir = Irgenerator.generate sast in 
			let output = Compile.compile ir
			in print_string output
		| Preprocessor ->
			let processed_code = preprocess() in
			let lexbuf = Lexing.from_string processed_code in
			let program = Parser.program Scanner.token lexbuf in
			let sast = Sastinjector.inject program in
			let ir = Irgenerator.generate sast in 
			let output = Compile.compile ir
			in print_string output
	    | Compile ->
			let processed_code = preprocess() in
			let lexbuf = Lexing.from_string processed_code in
			let program = Parser.program Scanner.token lexbuf in
			let sast = Semantic_analyzer.evalprogram program in
			let sast = (ignore sast; Sastinjector.inject program) in
			let ir = Irgenerator.generate sast in 
			let output = Compile.compile ir
			in print_string output 
	)
