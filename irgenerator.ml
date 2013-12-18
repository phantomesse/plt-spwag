(* Authors: Aftab Khan, John Wang *)

open Sast
open Ir


(* TODO:

- How are vars handled? Vars here refers to SAST.identifier list, 
	which is a list of global vars. Do we have global vars in SPWAG?
	How does this work with local scoping paradigm?
*)

(* list declarations *)
let javascript_funcs_list = [];
let slides_list = [];
let css_list = [];
(* the empty string represents NULL *)
let null = "";


let resolve_css func = (* attr function must only contain single block (list) of attributes *)
	let attr_list = List.map field_value_pair func.body in
	{
		clazz = func.name;

		display = false; (* TODO how to handle this? Where does bool value come from? *)
   		position_x = field_value_from_pair_list "position_x" attr_list;
    	position_y = field_value_from_pair_list "position_y" attr_list;
    
    	margin_top = field_value_from_pair_list "margin_top" attr_list;
    	margin_bottom = field_value_from_pair_list "margin_bottom" attr_list;
    	margin_left = field_value_from_pair_list "margin_left" attr_list;
    	margin_right = field_value_from_pair_list "margin_right" attr_list;
    
    	padding_top = field_value_from_pair_list "padding_top" attr_list;
    	padding_bottom = field_value_from_pair_list "padding_bottom" attr_list;
    	padding_left = field_value_from_pair_list "padding_left" attr_list;
    	padding_right = field_value_from_pair_list "padding_right" attr_list;

    	text_color = field_value_from_pair_list "text_color" attr_list;
    	background_color = field_value_from_pair_list "background_color" attr_list;

    	font = field_value_from_pair_list "font" attr_list;
    	font_size = field_value_from_pair_list "font_size" attr_list;
    	font_decoration = field_value_from_pair_list "font_decoration" attr_list;

    	border = field_value_from_pair_list "border" attr_list;
    	border_color = field_value_from_pair_list "border_color" attr_list;

    	width = field_value_from_pair_list "width" attr_list;
    	height = field_value_from_pair_list "height" attr_list;
	} :: css_list


(* get the last-provided value of a field from a (field:String, value:String) tuple list *)
let field_value_from_pair_list (expr : String) (l: list) =
	if (List.length l = 0) then null 
	else if (List.exists (expr, _) l) then (snd (List.find (expr, _) (List.rev l)))
	else null


(* the (field:String, value:String) tuple of a single-line expression *)
let field_value_pair (expr : String) =
	let lparen_index = (String.index expr '(') in
		(String.sub expr 0 lparen_index) , 
			(String.sub expr (lparen_index + 1) (((String.length expr) - 1 ) - (lparen_index + 1)))


(* determines function type and triggers the appropriate parsing procedure*)
let separate_func_type (func : Sast.func_definition) = 
	match func.t with
	| Ast.Func -> func :: javascript_funcs_list
	| Ast.Attr -> resolve_css func 		(* css for classes *)
	| Ast.Comp -> resolve_comp func
	| Ast.Slide -> resolve_slide


(* Main function that performs IR generation *)
let generate (vars, funcs) =
	List.map separate_func_type funcs

    

let blah = print_string "blah"