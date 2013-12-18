open Sast
open Ir

let string_of_vdecl = function
    Identifier(s) -> "var " ^ s ^ "\n"

let generate (vars, funcs) =
    String.concat "" (List.map string_of_vdecl vars) ^ "\n"