(* Authors: Aftab Khan, John Wang *)

open Sast
open Ir


(* TODO:

- How are vars handled? Vars here refers to SAST.identifier list, 
	which is a list of global vars. Do we have global vars in SPWAG?
	How does this work with local scoping paradigm?
- Handle StringMap Resolution

*)

(* list declarations *)
let javascript_funcs_list = [];
let slides_list = [];
let css_list = [];

(* the empty string represents NULL *)
let null = "";


(* get the last-provided value of a field from a (field:String, value:String) tuple list *)
let field_value_from_pair_list expr l =
	if (List.length l = 0) then null 
	else if ((List.exists (expr; _) l) then (snd (List.find (expr; _) (List.rev l)))
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
	| Ast.Attr -> resolve_global_css func 		(* css for classes *)
	| Ast.Comp -> resolve_comp func
	| Ast.Slide -> resolve_slide


(* handle global css classes ('define attr ...') and add to list *)
let resolve_global_css func =
	let attr_list = List.map field_value_pair func.body in
		let css_clazz = func.name;
		(resolve_css css_clazz attr_list) :: css_list


(* Main function that performs IR generation *)
let generate (vars, funcs) =
	List.map separate_func_type funcs



let resolve_slide func =
	let body_list = List.map field_value_pair func.body in
	{
		id = func.name;                          (* Id of the slide = name of the slide function*)
    	next = field_value_from_pair_list "next" body_list;     (* Id of the next slide = name of the slide function that is next *)
    	prev = field_value_from_pair_list "prev" body_list;		(* Id of the previous slide = name of the slide function that is prev *)
    	image = field_value_from_pair_list "image" body_list;	(* URL of any background image *)
    	style = resolve_slide_css body_list;                    (* CSS as applied to the slide in general *)
    	(*onclick : Sast.func_call;             (* Name of javascript function to apply on click *)
    	onpress : string * Sast.func_call;*)    (* Key to press, name of javascript function to apply on press *)
    	
    	(* TODO : Handle StringMap Resolution *)
		elements = element StringMap.t; 

	} :: slides_list


let resolve_comp func = 
	let body_list = List.map field_value_pair func.body in
	{
    	id = func.name;                          (* Unique id of a component WITHIN its outer component, concatenate with hyphens to obtain css id*)
    	
    	(* TODO : Where does this come from? *) 
		clazz = func.parent       					 (* This class specifies all attributes applied to this element via the component definition *)
    	
    	image = field_value_from_pair_list "image" body_list;		(* Image inside the element (optional) *)
    	text = field_value_from_pair_list "text" body_list;         (* Text inside the element (optional) *)
    	
    	(* TODO : What is the css class name of element-specific css? *)
    	style = resolve_css "" body_list;                 (* CSS as applied to this particular element with this id *)
    	
    	(*onclick : Sast.func_call;*)						(* Name of javascript function to apply on click, empty string means none *)

		(* TODO : Handle StringMap Resolution *) 
		elements = element StringMap.t;				(* Map of element id (string) -> element *)

	}


let resolve_css css_clazz field_value_pair_list = (* attr function must only contain single block (list) of attributes *)
	
	{
		clazz = css_clazz;

		display = false; (* TODO how to handle this? Where does bool value come from? *)
   		position_x = field_value_from_pair_list "position_x" field_value_pair_list;
    	position_y = field_value_from_pair_list "position_y" field_value_pair_list;
    
    	margin_top = field_value_from_pair_list "margin_top" field_value_pair_list;
    	margin_bottom = field_value_from_pair_list "margin_bottom" field_value_pair_list;
    	margin_left = field_value_from_pair_list "margin_left" field_value_pair_list;
    	margin_right = field_value_from_pair_list "margin_right" field_value_pair_list;
    
    	padding_top = field_value_from_pair_list "padding_top" field_value_pair_list;
    	padding_bottom = field_value_from_pair_list "padding_bottom" field_value_pair_list;
    	padding_left = field_value_from_pair_list "padding_left" field_value_pair_list;
    	padding_right = field_value_from_pair_list "padding_right" field_value_pair_list;

    	text_color = field_value_from_pair_list "text_color" field_value_pair_list;
    	background_color = field_value_from_pair_list "background_color" field_value_pair_list;

    	font = field_value_from_pair_list "font" field_value_pair_list;
    	font_size = field_value_from_pair_list "font_size" field_value_pair_list;
    	font_decoration = field_value_from_pair_list "font_decoration" field_value_pair_list;

    	border = field_value_from_pair_list "border" field_value_pair_list;
    	border_color = field_value_from_pair_list "border_color" field_value_pair_list;

    	width = field_value_from_pair_list "width" field_value_pair_list;
    	height = field_value_from_pair_list "height" field_value_pair_list;
	}


let resolve_slide_css field_value_pair_list = 
	{
    	padding_top = field_value_from_pair_list "padding_top" field_value_pair_list;
    	padding_bottom = field_value_from_pair_list "padding_bottom" field_value_pair_list;
    	padding_left = field_value_from_pair_list "padding_left" field_value_pair_list;
    	padding_right = field_value_from_pair_list "padding_right" field_value_pair_list;

    	text_color = field_value_from_pair_list "text_color" field_value_pair_list;
   		background_color = field_value_from_pair_list "background_color" field_value_pair_list;

    	font = field_value_from_pair_list "font" field_value_pair_list;
    	font_size = field_value_from_pair_list "font_size" field_value_pair_list;
    	font_decoration = field_value_from_pair_list "font_decoration" field_value_pair_list;

    	border = field_value_from_pair_list "border" field_value_pair_list;
    	border_color = field_value_from_pair_list "border_color" field_value_pair_list;
	}
    

let blah = print_string "blah"