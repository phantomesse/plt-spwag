open Ir

let _ =
    let input =
        if Array.length Sys.argv > 1 then
            open_in Sys.argv.(1)
    else stdin in
        let lexbuf = Lexing.from_channel input in
        let ir = Ir.program Scanner.token lexbuf in
            print_string "Hello!"