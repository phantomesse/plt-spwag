open Ast

let eval ast = "hi"

let _ =
    let input =
        if Array.length Sys.argv > 1 then
            open_in Sys.argv.(1)
    else stdin in
        let lexbuf = Lexing.from_channel input in
        let ast = Parser.program Scanner.token lexbuf in
        let result = eval ast in
            print_endline result