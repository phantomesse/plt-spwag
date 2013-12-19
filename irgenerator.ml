(* Authors: Yunhe (John) Wang and Aftab Khan *)
(* Reference the Ast only for function types and operators, all else is in sast/ir *)

open Sast
open Ir

(* Lookup table
 * Cur_path starts with the slide's name, followed by the parent elements
 * That means if cur_path has only one string, it's a slide
 * Slides are added immediately to slides_out
 * cur_element stores the current element if it's id is not given
 * If its id is given, cur_element is None, and you must look it up in slides_out *)
type lookup_table = {
    funcs_in 		: func_definition StringMap.t; (* Map function names to functions *)
	vars_in			: literal StringMap.t; 	(* Global Variables *)
	slides_out		: Slide.slide StringMap.t; (* Output Map of slides *)
	cur_slide		: string; (* Name of the current slide being evaluated *)
	cur_element		: Element.element option; (* Temporary storage for current element, before it is bound *)
}

(* John says: I agree, identifier is annoying, oh well*)
let id_to_str = function Identifier(s) -> s 

(* Converts funcs (Not attrs/comps/slides) to js definitions *)
let funcs_to_js funcs = 
	List.fold_left (fun jss (func:Sast.func_definition) -> match func.t with
					Ast.Func -> {Ir.name=func.name;formals=func.formals;body=func.body}:: jss
					| _ -> jss
					) [] funcs 

(* The following are functions needed for binding *)
(* Creates a blank slide given it's Identifier *)
let create_blank_slide i = 
	let fill_css = 
		{Slide.padding_top="";padding_bottom="";padding_left="";padding_right = "";
		text_color= "";background_color = "";font = "";font_size = "";font_decoration = "";
		border = "";border_color = "";}
	in
	{Slide.id=(id_to_str i);next="";prev="";image="";style=fill_css;onclick=None;onpress=None;elements=StringMap.empty}
(* Creates a blank element *)
let create_blank_element = 
	let fill_css = 
		{Element.display=true; position_x = ""; position_y = ""; margin_top = "";
    	margin_bottom = ""; margin_left = ""; margin_right = ""; padding_top = "";
    	padding_bottom = ""; padding_left = ""; padding_right = ""; text_color = "";
    	background_color = ""; font = ""; font_size = ""; font_decoration = "";
    	border = ""; border_color = ""; width = ""; height = "";}
	in
	{Element.id="";image="";text="";style=fill_css;onclick=None;elements=StringMap.empty;}
(*
(* Binds something to the given slide or element
 * @param path the path to whatever you want to bind
 * @param output what you're binding to, should be slides_out
 * @param elementp The element to bind to the path, or None to bind a blank slide
 * @return the updated output -> slides_out
 *)
let bind_to_slide_comp path output = function
	None -> 
				in
		StringMap.add (hd path) (create_blank_slide (hd path)) output
	| Some(x) ->
		(* e is the element, p is the full path for error printing, last param is the partial path *)
		(* returns the parent element that needs its binding changed *)
		let rec get_element (e : Element.element) p = function
			[] -> e
			| hd::[] -> e
			| hd::tl -> 
				try get_element (StringMap.find hd e.elements) tl
  				with Not_found -> raise (Failure ("Cannot find the following element: " ^ String.concat "->" p))
		in
		let element_name = hd (List.rev path) in
		if (StringMap.mem (id_to_str func.name) lookup.funcs_in) 
		then raise (Failure ("There are two definitions for function name " ^ (id_to_str func.name)))
		else {lookup with funcs_in = StringMap.add (id_to_str func.name) func lookup.funcs_in} 	
*)
			


(* Main function that performs IR generation *)
let generate (vars, funcs) =

	(* Create lookup table 
	 * @param vars is the variables from Sast.program
	 * @param funcs is the function definitions from Sast.program 
	 * @return the lookup table as specified *)
	let create_lookup vars funcs = 
		let fill_funcs lookup (func : Sast.func_definition) = 
			if (StringMap.mem (id_to_str func.name) lookup.funcs_in) 
			then raise (Failure ("There are two definitions for function name " ^ (id_to_str func.name)))
			else {lookup with funcs_in = StringMap.add (id_to_str func.name) func lookup.funcs_in} 	
		in 
		let fill_vars lookup id = {lookup with vars_in=StringMap.add (id_to_str id) Litnull lookup.vars_in}
		in
		(List.fold_left fill_vars (List.fold_left fill_funcs 
										({funcs_in=StringMap.empty; 
										  vars_in=StringMap.empty;
										  slides_out=StringMap.empty;
										  cur_slide="";
										  cur_element=None;}) funcs) vars)
	in 

	(* This calls a function to generate ir
	 * @param fdef is the function definition (Sast.func_definition)
	 * @param actuals are the actual parameter list, of literals
	 * @param lookup is the lookup table 
	 * @return the updated lookup table *)
	let rec call (fdef:Sast.func_definition) actuals lookupparam = 
		
		(* Actually executes statements
		 * @param loclook (locals, lookup)
		 * @param statement the statement the execute
		 * @return (locals, lookup) *)
		let rec exec loclook statement = 
			(fst loclook, snd loclook) 
		in
		
		(* This section takes care of scoping issues before calling exec on statements *)
		(* Assign the locals from the actual parameters *)
		let locals = 
      		try List.fold_left2
	  			(fun locals formal actual -> StringMap.add (id_to_str formal) actual locals)
	  			StringMap.empty fdef.formals actuals
      		with Invalid_argument(_) ->
				raise (Failure ("Wrong number of arguments passed to " ^ (id_to_str fdef.name)))
		in
		(* Immediately bind if it's a slide, update current slide as well *)
		(* Create the element if it's a comp and its parent is "box" *)
		(* Call its parent if it's a comp and its parent is not "box" *)
		(* Finally, do nothing if it's any other type of function*)
		let lookup = function 
					Ast.Slide -> {lookupparam with 
						slides_out= StringMap.add (id_to_str fdef.name) (create_blank_slide fdef.name) lookupparam.slides_out;
						cur_slide = (id_to_str fdef.name);}
					| Ast.Comp -> (match fdef.inheritance with
						Some(Sast.Identifier("box")) -> {lookupparam with cur_element = Some(create_blank_element)}
						| Some(Sast.Identifier(s)) ->  
							let parent = 
								try StringMap.find s lookupparam.funcs_in
								with Not_found -> raise (Failure ("The following component is not defined: " ^ s))
							in
							let isComp = function Ast.Comp -> true | _ -> false in
							if isComp parent.t
							then
								(call parent [] lookupparam) (* TODO: Evaluate the paractuals *)  
							else
								raise (Failure ("A component can only inherit from a component for " ^ (id_to_str fdef.name)))
						| None -> raise (Failure ("The following component needs to inherit from a component: " ^ (id_to_str fdef.name)))
						)
					| _ -> lookupparam
		in			
		(* Now recursively execute every statement with the updated locals *)
		snd (List.fold_left exec (locals, lookup fdef.t) fdef.body) 
	in

	(* Here is where all the functions get called to produce the final output *)
	let lookup = create_lookup vars funcs in
	let pre_ir = 
		try (call (StringMap.find "main" lookup.funcs_in) [] lookup)  
		with Not_found -> raise (Failure ("There must exist a main() slide"))
	in
	(StringMap.fold (fun k d l -> d :: l) (pre_ir.slides_out) [], vars, funcs_to_js funcs)

	
