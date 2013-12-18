(* Authors: Aftab Khan, John Wang *)

open Sast
open Ir

module functions_list = struct
	
end

(* TODO:

- How are vars handled? Vars here refers to SAST.identifier list, 
	which is a list of global vars. Do we have global vars in SPWAG?
*)

(* list declarations *)
let javascript_funcs_list = [];
let slides_list = [];
let css_list = [];



(* determines function type and triggers the appropriate parsing procedure*)
let separate_func_type (func : Sast.func_definition) = 
	match func.t with
	| Ast.Func -> func::javascript_funcs_list
	| Ast.Attr -> resolve_css func
	| Ast.Comp -> resolve_comp func
	| Ast.Slide -> resolve_slide

(* Main function that performs IR generation *)
let generate (vars, funcs) =

	List.map separate_func_type funcs

    

let blah = print_string "blah"