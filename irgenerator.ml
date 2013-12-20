(* Author: Yunhe (John) Wang Contributor: Aftab Khan *)
(* Reference the Ast only for function types and operators, all else is in sast/ir *)

open Sast
open Ir
open Slide
open Element

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

(* Given local_outer and loclook returned by a statement eval, this merges them for a single local variable map for outer scope
 * Returns the new loclook *)
let merge_locals local_outer (local_inner, lookup) = 
	(StringMap.fold (fun k d a -> if StringMap.mem k a
		then StringMap.add k d a
		else a) local_inner local_outer, lookup)

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

(* Binds css to an element 
 * @param attribute the css attribute, as a string
 * @param value the value to bind it to, Ir.literal
 * @param elementp the element to bind css to
 * @return the updated element
 *)
let ir_bind_css_element attribute value (elementp:Element.element) = (match (attribute, value) with
	("display", Ir.Litbool(b)) -> {elementp with style= {elementp.style with display=b}}
    | ("position-x", Ir.Litint(i)) -> {elementp with style= {elementp.style with position_x=(string_of_int i)}}
    | ("position-x", Ir.Litper(i)) -> {elementp with style= {elementp.style with position_x=(string_of_int i)^"%"}}
    | ("position-y", Ir.Litint(i)) -> {elementp with style= {elementp.style with position_y=(string_of_int i)}}
    | ("position-y", Ir.Litper(i)) -> {elementp with style= {elementp.style with position_y=(string_of_int i)^"%"}}
    | ("margin-top", Ir.Litint(i)) -> {elementp with style= {elementp.style with margin_top=(string_of_int i)}}
    | ("margin-top", Ir.Litper(i)) -> {elementp with style= {elementp.style with margin_top=(string_of_int i)^"%"}}
    | ("margin-bottom", Ir.Litint(i)) -> {elementp with style= {elementp.style with margin_bottom=(string_of_int i)}}
    | ("margin-bottom", Ir.Litper(i)) -> {elementp with style= {elementp.style with margin_bottom=(string_of_int i)^"%"}}
    | ("margin-left", Ir.Litint(i)) -> {elementp with style= {elementp.style with margin_left=(string_of_int i)}}
    | ("margin-left", Ir.Litper(i)) -> {elementp with style= {elementp.style with margin_left=(string_of_int i)^"%"}}
    | ("margin-right", Ir.Litint(i)) -> {elementp with style= {elementp.style with margin_right=(string_of_int i)}}
    | ("margin-right", Ir.Litper(i)) -> {elementp with style= {elementp.style with margin_right=(string_of_int i)^"%"}}
    | ("padding-top", Ir.Litint(i)) -> {elementp with style= {elementp.style with padding_top=(string_of_int i)}}
    | ("padding-top", Ir.Litper(i)) -> {elementp with style= {elementp.style with padding_top=(string_of_int i)^"%"}}
    | ("padding-bottom", Ir.Litint(i)) -> {elementp with style= {elementp.style with padding_bottom=(string_of_int i)}}
    | ("padding-bottom", Ir.Litper(i)) -> {elementp with style= {elementp.style with padding_bottom=(string_of_int i)^"%"}}
    | ("padding-left", Ir.Litint(i)) -> {elementp with style= {elementp.style with padding_left=(string_of_int i)}}
    | ("padding-left", Ir.Litper(i)) -> {elementp with style= {elementp.style with padding_left=(string_of_int i)^"%"}}
    | ("padding-right", Ir.Litint(i)) -> {elementp with style= {elementp.style with padding_right=(string_of_int i)}}
    | ("padding-right", Ir.Litper(i)) -> {elementp with style= {elementp.style with padding_right=(string_of_int i)^"%"}}
    | ("text-color", Ir.Litstr(s)) -> {elementp with style= {elementp.style with text_color=s}}
    | ("background-color", Ir.Litstr(s)) -> {elementp with style= {elementp.style with background_color=s}}
    | ("font", Ir.Litstr(s)) -> {elementp with style= {elementp.style with font=s}}
    | ("font-size", Ir.Litint(i)) -> {elementp with style= {elementp.style with font_size=(string_of_int i)}}
    | ("font-decoration", Ir.Litstr(s)) -> {elementp with style= {elementp.style with font_decoration=s}}
    | ("border", Ir.Litint(i)) -> {elementp with style= {elementp.style with border=(string_of_int i)}}
    | ("border-color", Ir.Litstr(s)) -> {elementp with style= {elementp.style with border_color=s}}
    | ("width", Ir.Litint(i)) -> {elementp with style= {elementp.style with width=(string_of_int i)}}
    | ("width", Ir.Litper(i)) -> {elementp with style= {elementp.style with width=(string_of_int i)^"%"}}
    | ("height", Ir.Litint(i)) -> {elementp with style= {elementp.style with height=(string_of_int i)}}
    | ("height", Ir.Litper(i)) -> {elementp with style= {elementp.style with height=(string_of_int i)^"%"}}
    | ("id", Ir.Litstr(s)) -> {elementp with id=s}
    | ("image", Ir.Litstr(s)) -> {elementp with image=s}
    | ("text", Ir.Litstr(s)) -> {elementp with text=s}
	| (_,_) -> raise (Failure ("The following built-in attribute is not used correctly on component: " ^ attribute))
	)	

(* Binds css to a slide 
 * @param attribute the css attribute, as a string
 * @param value the value to bind it to, Ir.literal
 * @param slidep the Slide.slide
 * @return the updated slide
 *)
let ir_bind_css_slide attribute value (slidep:Slide.slide) = (match (attribute, value) with
	("padding-top", Ir.Litint(i)) -> {slidep with style= {slidep.style with padding_top=(string_of_int i)}}
    | ("padding-top", Ir.Litper(i)) -> {slidep with style= {slidep.style with padding_top=(string_of_int i)^"%"}}
    | ("padding-bottom", Ir.Litint(i)) -> {slidep with style= {slidep.style with padding_bottom=(string_of_int i)}}
    | ("padding-bottom", Ir.Litper(i)) -> {slidep with style= {slidep.style with padding_bottom=(string_of_int i)^"%"}}
    | ("padding-left", Ir.Litint(i)) -> {slidep with style= {slidep.style with padding_left=(string_of_int i)}}
    | ("padding-left", Ir.Litper(i)) -> {slidep with style= {slidep.style with padding_left=(string_of_int i)^"%"}}
    | ("padding-right", Ir.Litint(i)) -> {slidep with style= {slidep.style with padding_right=(string_of_int i)}}
    | ("padding-right", Ir.Litper(i)) -> {slidep with style= {slidep.style with padding_right=(string_of_int i)^"%"}}
    | ("text-color", Ir.Litstr(s)) -> {slidep with style= {slidep.style with text_color=s}}
    | ("background-color", Ir.Litstr(s)) -> {slidep with style= {slidep.style with background_color=s}}
    | ("font", Ir.Litstr(s)) -> {slidep with style= {slidep.style with font=s}}
    | ("font-size", Ir.Litint(i)) -> {slidep with style= {slidep.style with font_size=(string_of_int i)}}
    | ("font-decoration", Ir.Litstr(s)) -> {slidep with style= {slidep.style with font_decoration=s}}
    | ("border", Ir.Litint(i)) -> {slidep with style= {slidep.style with border=(string_of_int i)}}
    | ("border-color", Ir.Litstr(s)) -> {slidep with style= {slidep.style with border_color=s}}
    | ("next", Ir.Litslide(Identifier(s))) -> {slidep with next=s}
    | ("prev", Ir.Litslide(Identifier(s))) -> {slidep with prev=s}
    | ("image", Ir.Litstr(s)) -> {slidep with image=s}
	| (_,_) -> raise (Failure ("The following built-in attribute is not used correctly on slide: " ^ attribute))
	)

(* Gets the css attribute of a particular component/slide that must have already been bound
 * @param attribute the attribute, as a string
 * @param path the Ir.Litcomp or Ir.Litslide
 * @param lookup the lookup table
 * @return the attribute, as an Ir.literal
 *)
let ir_get_css attribute path lookup = (match path with
	Ir.Litslide(Identifier(s)) -> 
		let the_slide = StringMap.find s lookup.slides_out in
		let is_null s_in = ((compare s_in "") = 0) in
		let is_per s_in = ((String.get s_in ((String.length s_in)-1)) = '%') in
		let per_to_int s_in = int_of_string (String.sub s_in 0 ((String.length s_in)-1)) in 
		let int_or_per the_attr = 
			if is_null the_attr then Ir.Litnull 
			else if is_per the_attr then Ir.Litper(per_to_int the_attr) 
			else Ir.Litint(int_of_string the_attr)
		in
		(match attribute with
			"padding-top" -> int_or_per the_slide.style.padding_top
		    | "padding-bottom" -> int_or_per the_slide.style.padding_bottom
		    | "padding-left" -> int_or_per the_slide.style.padding_left
		    | "padding-right" -> int_or_per the_slide.style.padding_right
		    | "text-color" -> Ir.Litstr(the_slide.style.text_color)
		    | "background-color" -> Ir.Litstr(the_slide.style.background_color)
		    | "font" -> Ir.Litstr(the_slide.style.font)
		    | "font-size" ->  int_or_per the_slide.style.font_size
		    | "font-decoration" ->  Ir.Litstr(the_slide.style.font_decoration)
		    | "border" -> int_or_per the_slide.style.border
		    | "border-color" -> Ir.Litstr(the_slide.style.border_color)
		    | "next" -> Ir.Litslide(Identifier(the_slide.next))
		    | "prev" -> Ir.Litslide(Identifier(the_slide.prev))
		    | "image" -> Ir.Litstr(the_slide.image)
			| _ -> raise (Failure ("The following built-in attribute does not exist for slide used in get(): " ^ attribute))
		)
	| Ir.Litcomp(Identifier(s), slist) ->
		let the_slide = StringMap.find s lookup.slides_out in
		let the_comp = StringMap.find (List.hd slist) the_slide.elements in
		(* e is the element, p is the full path for error printing, last param is the partial path *)
		(* returns the element represented by the path *)
		let rec get_comp (e : Element.element) p = function
			[] -> e
			| hd::tl -> 
				try get_comp (StringMap.find hd e.elements) p tl
  				with Not_found -> raise (Failure ("Cannot find the following component: " ^ String.concat "->" p))
		in
		let the_comp = get_comp the_comp slist (List.tl slist) in
		let is_null s_in = ((compare s_in "") = 0) in
		let is_per s_in = ((String.get s_in ((String.length s_in)-1)) = '%') in
		let per_to_int s_in = int_of_string (String.sub s_in 0 ((String.length s_in)-1)) in 
		let int_or_per the_attr = 
			if is_null the_attr then Ir.Litnull 
			else if is_per the_attr then Ir.Litper(per_to_int the_attr) 
			else Ir.Litint(int_of_string the_attr)
		in
		(match attribute with
			"display" -> Ir.Litbool(the_comp.style.display)
			| "position-x" -> int_or_per the_comp.style.position_x
		    | "position-y" -> int_or_per the_comp.style.position_y
		    | "margin-top" -> int_or_per the_comp.style.margin_top
		    | "margin-bottom" -> int_or_per the_comp.style.margin_bottom
		    | "margin-left" -> int_or_per the_comp.style.margin_left
		    | "margin-right" -> int_or_per the_comp.style.margin_right
			| "padding-top" -> int_or_per the_comp.style.padding_top
		    | "padding-bottom" -> int_or_per the_comp.style.padding_bottom
		    | "padding-left" -> int_or_per the_comp.style.padding_left
		    | "padding-right" -> int_or_per the_comp.style.padding_right
		    | "text-color" -> Ir.Litstr(the_comp.style.text_color)
		    | "background-color" -> Ir.Litstr(the_comp.style.background_color)
		    | "font" -> Ir.Litstr(the_comp.style.font)
		    | "font-size" ->  int_or_per the_comp.style.font_size
		    | "font-decoration" ->  Ir.Litstr(the_comp.style.font_decoration)
		    | "border" -> int_or_per the_comp.style.border
		    | "border-color" -> Ir.Litstr(the_comp.style.border_color)
		    | "width" -> int_or_per the_comp.style.width
		    | "height" -> int_or_per the_comp.style.height
		    | "id" -> Ir.Litstr(the_comp.id)
		    | "image" -> Ir.Litstr(the_comp.image)
		    | "text" -> Ir.Litstr(the_comp.text)
			| _ -> raise (Failure ("The following built-in attribute does not exist for comp used in get(): " ^ attribute))
		)
	| _ -> raise (Failure ("The first parameter of get() must be a slide or component."))
	)
(* 
This code might be useful, may not be, we'll see
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

exception ReturnException of literal * lookup_table

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
		
		(* Evaluates expressions for static code, binds elements, etc.
		 * @param loclook (locals, lookup)
		 * @param expression the expression to evaluate
		 * @return (Ir.literal, (locals, lookup)) *)
		let rec eval loclook = function
			Sast.Binop(e1, op, e2) ->
				let r1, loclook = eval loclook (fst e1) in
				let r2, loclook = eval loclook (fst e2) in
				let evaluate_eq b = function Ast.Equals -> b | _ -> (not b) in 
				(match r1, op, r2 with

					Ir.Litint(i1), Ast.Plus, Ir.Litint(i2) -> (Ir.Litint(i1 + i2), loclook)
					| Ir.Litint(a), Ast.Plus, Ir.Litstr(b) -> (Ir.Litstr((string_of_int a) ^ b), loclook)
					| Ir.Litstr(a), Ast.Plus, Ir.Litint(b) -> (Ir.Litstr(a ^ (string_of_int b)), loclook)
					| Ir.Litper(i1), Ast.Plus, Ir.Litper(i2) -> (Ir.Litper(i1 + i2), loclook)
					| Ir.Litper(a), Ast.Plus, Ir.Litstr(b) -> (Ir.Litstr((string_of_int a) ^ "%" ^ b), loclook)
					| Ir.Litstr(a), Ast.Plus, Ir.Litper(b) -> (Ir.Litstr(a ^ (string_of_int b) ^ "%"), loclook)
					| Ir.Litstr(a), Ast.Plus, Ir.Litstr(b) -> (Ir.Litstr(a ^ b), loclook)
					| Ir.Litstr(a), Ast.Plus, Ir.Litbool(b) -> (Ir.Litstr(a ^ (string_of_bool b)), loclook)
					| Ir.Litbool(a), Ast.Plus, Ir.Litstr(b) -> (Ir.Litstr((string_of_bool a) ^ b), loclook)
					| Ir.Litstr(a), Ast.Plus, Ir.Litslide(Identifier(b)) -> (Ir.Litstr(a ^ b), loclook)
					| Ir.Litslide(Identifier(a)), Ast.Plus, Ir.Litstr(b) -> (Ir.Litstr(a ^ b), loclook)
					| Ir.Litnull, Ast.Plus, Ir.Litstr(b) -> (Ir.Litstr(b), loclook)
					| Ir.Litstr(a), Ast.Plus, Ir.Litnull -> (Ir.Litstr(a), loclook)

					| Ir.Litint(i1), Ast.Minus, Ir.Litint(i2) -> (Ir.Litint(i1 - i2), loclook)
					| Ir.Litper(i1), Ast.Minus, Ir.Litper(i2) -> (Ir.Litper(i1 - i2), loclook)

					| Ir.Litint(i1), Ast.Times, Ir.Litint(i2) -> (Ir.Litint(i1 * i2), loclook)
					| Ir.Litint(i1), Ast.Times, Ir.Litper(i2) -> (Ir.Litper(i1 * i2), loclook)
					| Ir.Litper(i1), Ast.Times, Ir.Litint(i2) -> (Ir.Litper(i1 * i2), loclook)

					| Ir.Litint(i1), Ast.Divide, Ir.Litint(i2) -> (Ir.Litint(i1 / i2), loclook)
					| Ir.Litper(i1), Ast.Divide, Ir.Litint(i2) -> (Ir.Litper(i1 / i2), loclook)
					
					| Ir.Litint(i1), (Ast.Equals | Ast.Notequals), Ir.Litint(i2) -> (Ir.Litbool(evaluate_eq (i1 = i2) op), loclook)
					| Ir.Litper(i1), (Ast.Equals | Ast.Notequals), Ir.Litper(i2) -> (Ir.Litbool(evaluate_eq (i1 = i2) op), loclook)
					| Ir.Litstr(i1), (Ast.Equals | Ast.Notequals), Ir.Litstr(i2) -> (Ir.Litbool(evaluate_eq ((String.compare i1 i2) = 0) op), loclook)
					| Ir.Litbool(i1), (Ast.Equals | Ast.Notequals), Ir.Litbool(i2) -> (Ir.Litbool(evaluate_eq (i1 = i2) op), loclook)
					| Ir.Litcomp(Identifier(i1), slist1), (Ast.Equals | Ast.Notequals), Ir.Litcomp(Identifier(i2), slist2) -> 
						(Ir.Litbool(evaluate_eq ((String.compare i1 i2) = 0 && 
						(List.fold_left2 (fun t s1 s2 ->
							if t then ((String.compare s1 s2) = 0)
							else false) true slist1 slist2)) op
						), loclook)
					| Ir.Litslide(Identifier(s1)), (Ast.Equals | Ast.Notequals), Ir.Litslide(Identifier(s2)) ->  (Ir.Litbool(evaluate_eq ((String.compare s1 s2) = 0) op), loclook)
					| Ir.Litnull, (Ast.Equals | Ast.Notequals), Ir.Litnull -> (Ir.Litbool(evaluate_eq true op), loclook)
					| _, (Ast.Equals | Ast.Notequals), _ -> (Ir.Litbool(evaluate_eq false op), loclook)

					| Ir.Litint(i1), Ast.Lessthan, Ir.Litint(i2) -> (Ir.Litbool(i1 < i2), loclook)
					| Ir.Litint(i1), Ast.Greaterthan, Ir.Litint(i2) -> (Ir.Litbool(i1 > i2), loclook)
					| Ir.Litper(i1), Ast.Lessthan, Ir.Litper(i2) -> (Ir.Litbool(i1 < i2), loclook)
					| Ir.Litper(i1), Ast.Greaterthan, Ir.Litper(i2) -> (Ir.Litbool(i1 > i2), loclook)
					| Ir.Litstr(s1), Ast.Lessthan, Ir.Litstr(s2) -> (Ir.Litbool((String.compare s1 s2) < 0), loclook)
					| Ir.Litstr(s1), Ast.Greaterthan, Ir.Litstr(s2) -> (Ir.Litbool((String.compare s1 s2) > 0), loclook)
					
					| Ir.Litbool(s1), Ast.Or, Ir.Litbool(s2) -> (Ir.Litbool(s1 || s2), loclook)
					| Ir.Litbool(s1), Ast.And, Ir.Litbool(s2) -> (Ir.Litbool(s1 && s2), loclook)

				  	| a, op, b -> raise(Failure("Semantic Analyzer should print out error for this operation"))
				)
			| Sast.Notop(e) -> 
				let r, loclook = eval loclook (fst e) in
				(match r with
					Ir.Litbool(b) -> (Ir.Litbool(not b), loclook)
					| _ -> raise (Failure ("This cannot be notted")))
			| Sast.Litint(i) -> (Ir.Litint(i), loclook)
			| Sast.Litper(i) -> (Ir.Litper(i), loclook)
			| Sast.Litstr(s) -> (Ir.Litstr(s), loclook)
			| Sast.Litbool(b) -> (Ir.Litbool(b), loclook)
			| Sast.Litnull -> (Ir.Litnull, loclook)
			| Sast.Assign(Identifier(i), (e,_)) ->
				let r, (locals, lookup) = eval loclook e in
				if StringMap.mem i locals
					then r, (StringMap.add i r locals, lookup)
				else if StringMap.mem i lookup.vars_in 
					then r, (locals, {lookup with vars_in=StringMap.add i r lookup.vars_in})
				else raise (Failure ("Undeclared identifier " ^ i))
			| Sast.Variable(Identifier(i)) -> 
				let (locals, lookup) = loclook in
				if ((StringMap.mem i lookup.funcs_in) 
					&& (match (StringMap.find i lookup.funcs_in).t with Ast.Slide -> true | _ -> false))
					then if (not (StringMap.mem i lookup.slides_out))
						then try (Ir.Litslide(Identifier(i)), (locals, 
							{(call (StringMap.find i lookup.funcs_in) [] lookup) with cur_slide=lookup.cur_slide})) 
							with ReturnException(r, l) -> (Ir.Litslide(Identifier(i)), (fst loclook, {l with cur_slide=lookup.cur_slide}))
						else (Ir.Litslide(Identifier(i)), (locals, lookup)) 
				else if StringMap.mem i (fst loclook) 
					then (StringMap.find i (fst loclook), loclook)
				else if StringMap.mem i (snd loclook).vars_in
					then (StringMap.find i (snd loclook).vars_in, loclook)
				else raise (Failure ("Undeclared identifier " ^ i))
			| Sast.Component(i, elist) ->
				let (the_slide, loclook) = (eval loclook (Sast.Variable(i))) in
				let loclook = (match the_slide with
					Ir.Litslide(_) -> loclook
					| _ -> raise (Failure ("The following slide has not been created, used on a Component literal: " ^ (id_to_str i)))
					)
				in
				let (rlist, loclookp) = 
						List.fold_left
						(fun (actuals, loclook) actual -> let (r, loclook) = eval loclook (fst actual) in (r :: actuals, loclook))
						([], loclook) (List.rev elist)
				in
				let slist = List.fold_left
					(fun l r -> (match r with 
						Ir.Litstr(s) -> (s :: l) 
						| _ -> raise (Failure ("Only strings allowed in brackets, for " ^ (id_to_str i)) )) )
					[] (List.rev rlist)
				in 
				(Ir.Litcomp(i, slist), loclookp)
			| Sast.Call(f) ->
				(* TODO: Check for built-in functions 
				 * Left: get set *)
				let process_built_in_attr built_in_name =
					let (actual, loclook) = eval loclook (fst (List.hd f.actuals)) in
					let (locals, lookup) = loclook in
					(match lookup.cur_element with
							None -> 
								let the_slide = StringMap.find lookup.cur_slide lookup.slides_out in
								(actual, (locals,
								{lookup with slides_out = StringMap.add lookup.cur_slide
								(ir_bind_css_slide built_in_name actual the_slide) lookup.slides_out}))
							| Some(x) -> (actual, (locals, 
								{lookup with cur_element = Some(ir_bind_css_element built_in_name actual x)}))
					)
				in
				(match f.cname with
					Identifier("display") -> process_built_in_attr "display"
					| Identifier("position-x") -> process_built_in_attr "position-x"
   					| Identifier("position-y") -> process_built_in_attr "position-y" 
   					| Identifier("margin-top") -> process_built_in_attr "margin-top"
   					| Identifier("margin-bottom") -> process_built_in_attr "margin-bottom"
   					| Identifier("margin-left") -> process_built_in_attr "margin-left"
   					| Identifier("margin-right") -> process_built_in_attr "margin-right"
   					| Identifier("padding-top") -> process_built_in_attr "padding-top"
   					| Identifier("padding-bottom") -> process_built_in_attr "padding-bottom"
   					| Identifier("padding-left") -> process_built_in_attr "padding-left"
   					| Identifier("padding-right") -> process_built_in_attr "padding-right"
   					| Identifier("text-color") -> process_built_in_attr "text-color"
   					| Identifier("background-color") -> process_built_in_attr "background-color"
   					| Identifier("font") -> process_built_in_attr "font"
   					| Identifier("font-size") -> process_built_in_attr "font-size"
   					| Identifier("font-decoration") -> process_built_in_attr "font-decoration"
   					| Identifier("border") -> process_built_in_attr "border"
   					| Identifier("border-color") -> process_built_in_attr "border-color"
   					| Identifier("width") -> process_built_in_attr "width"
   					| Identifier("height") -> process_built_in_attr "height"
   					| Identifier("id") -> process_built_in_attr "id"
   					| Identifier("image") -> process_built_in_attr "image"
   					| Identifier("text") -> process_built_in_attr "text"
   					| Identifier("next") -> process_built_in_attr "next"
   					| Identifier("prev") -> process_built_in_attr "prev"
					| Identifier("random") -> 
						let (actual, loclook) = eval loclook (fst (List.hd f.actuals)) in
						let get_rand integer = Random.self_init(); Random.int integer in
						(match actual with
							Ir.Litint(i) -> (Ir.Litint(get_rand i), loclook)
							| _ -> raise (Failure ("You must pass in an integer to random()")))
 					| Identifier("on-click") ->
						let f_to_call = (match (List.hd f.actuals) with
							(Sast.Call(f),_) -> f
							| _ -> raise (Failure ("You must pass in an function to call  to on-click()")))
						in	
						let (pactuals, loclook) = 
							List.fold_left
							(fun (actuals, loclook) actual -> let (r, loclook) = eval loclook (fst actual) in (r :: actuals, loclook))
							([], loclook) (List.rev f_to_call.actuals)
						in
						let (locals, lookup) = loclook in
						(match lookup.cur_element with
								None -> 
									let the_slide = StringMap.find lookup.cur_slide lookup.slides_out in
									(Litnull, (locals,
									{lookup with slides_out = StringMap.add lookup.cur_slide
									({the_slide with onclick = Some({Ir.cname=f_to_call.cname; actuals=pactuals;})}) lookup.slides_out}))
								| Some(x) -> (Litnull, (locals, 
									{lookup with cur_element = Some({x with onclick = Some({Ir.cname=f_to_call.cname; actuals=pactuals;})})}))
						)
					| Identifier("on-press") ->
						let f_to_call = (match (List.nth f.actuals 1) with
							(Sast.Call(f),_) -> f
							| _ -> raise (Failure ("You must pass in a function to call as 2nd parameter to on-press()")))
						in
						let (key_to_press, loclook) = eval loclook (fst (List.hd f.actuals)) in
						let key_to_press = (match key_to_press with
							Ir.Litstr(s) -> s
							| _ ->  raise (Failure ("You must pass in a string as key as 1st parameter to on-press()")))
						in
						let (pactuals, loclook) = 
							List.fold_left
							(fun (actuals, loclook) actual -> let (r, loclook) = eval loclook (fst actual) in (r :: actuals, loclook))
							([], loclook) (List.rev f_to_call.actuals)
						in
						let (locals, lookup) = loclook in
						let the_slide = StringMap.find lookup.cur_slide lookup.slides_out in
							(Ir.Litnull, (locals,
							{lookup with slides_out = StringMap.add lookup.cur_slide
							({the_slide with onpress = Some(key_to_press,{Ir.cname=f_to_call.cname; actuals=pactuals;})}) lookup.slides_out}))
					| Identifier("get") -> 
						let (param1, loclook) = eval loclook (fst (List.hd f.actuals)) in
						let (param2, loclook) = eval loclook (fst (List.nth f.actuals 1)) in
						(match param2 with
							Ir.Litstr(s) ->  (ir_get_css s param1 (snd loclook), loclook)
							| _ ->  raise (Failure ("You must pass in a string as attribute name as 2nd parameter to get()"))
						)
					| Identifier(_) -> 
				(* The rest of these lines are for non-built-in functions *)
				let fdecl = 
					(match f.cname with
						(* Except the special component box *)
						Identifier("box") -> 
							{Sast.t=Ast.Comp;name=Sast.Identifier("box");formals=[];
							inheritance=Some(Sast.Identifier("box"));paractuals=[];body=[];}
						(* All others, you must find the function *)
						| Identifier(_) ->
						let not_slide (check:Sast.func_definition) = (match check.t with
							Ast.Slide -> raise (Failure ("Cannot call a slide like a regular function: " ^ (id_to_str f.cname)))
							| _ -> check)
						in
	    				try not_slide (StringMap.find (id_to_str f.cname) (snd loclook).funcs_in)
	    				with Not_found -> raise (Failure ("undefined function " ^ (id_to_str f.cname)))
					)
				in
				let (actuals, loclook) = 
						List.fold_left
						(fun (actuals, loclook) actual -> let (r, loclook) = eval loclook (fst actual) in (r :: actuals, loclook))
						([], loclook) (List.rev f.actuals)
				in
				let (locals, lookup) = loclook in
				let r, returned_lookup = 
					try (Ir.Litnull, call fdecl actuals lookup)
					with ReturnException(r, lookup) -> (r, lookup)
				in
				(match fdecl.t with
					Ast.Slide -> raise (Failure ("This shouldn't be displayed."))
					| Ast.Attr -> (Ir.Litnull, (locals, returned_lookup))
					| Ast.Func -> (r, (locals, returned_lookup))
					| Ast.Comp -> 
						let (locals, returned_lookup) = exec (locals, returned_lookup) f.mods in
						(match lookup.cur_element with
							(* Bind to slide *)
							None -> 
								let the_slide = (StringMap.find lookup.cur_slide returned_lookup.slides_out) in
								let the_element = (match returned_lookup.cur_element with
									None -> raise (Failure ("Element binding error"))
									| Some(x) -> x)
								in
								(the_slide.elements <- (StringMap.add the_element.id the_element the_slide.elements)); 
								(Ir.Litnull, (locals, {returned_lookup with cur_element=None}))
							(* Bind to another element *)
							| Some(par_element) -> 
								let the_element = (match returned_lookup.cur_element with
									None -> raise (Failure ("Element binding error"))
									| Some(x) -> x)
								in
								(par_element.elements <- (StringMap.add the_element.id the_element par_element.elements));  
								(Ir.Litnull, (locals, {returned_lookup with cur_element=Some(par_element)}))
						)
				)
				)

		(* Actually executes statements
		 * This part doesn't use the type info, more useful for javascript compiling later on
		 * @param loclook (locals, lookup)
		 * @param statement the statement the execute
		 * @return (locals, lookup) *)
		and exec loclook = function
			Sast.Block(stmts) -> merge_locals (fst loclook) (List.fold_left exec loclook stmts)
			| Sast.Expr(e) -> let _, loclook = eval loclook (fst e) in loclook
			| Sast.If(e, s1, s2) ->  
	  			let v, loclook = eval loclook (fst e) in
				(match v with
					Ir.Litbool(false) -> merge_locals (fst loclook) (exec loclook s2)
					| Ir.Litint(0) -> merge_locals (fst loclook) (exec loclook s2)
					| Ir.Litper(0) -> merge_locals (fst loclook) (exec loclook s2)
					| Ir.Litstr("") -> merge_locals (fst loclook) (exec loclook s2)
					| Ir.Litnull -> merge_locals (fst loclook) (exec loclook s2)
					| _ -> merge_locals (fst loclook) (exec loclook s1))
			| Sast.While(e, s) ->
				let rec loop loclook =
					let v, loclook = eval loclook (fst e) in
					(match v with
						Ir.Litbool(false) -> loclook
						| Ir.Litint(0) -> loclook
						| Ir.Litper(0) -> loclook
						| Ir.Litstr("") -> loclook
						| Ir.Litnull -> loclook
						| _ -> loop (exec loclook s))
				in merge_locals (fst loclook) (loop loclook)
			| Sast.Declaration(Identifier(s)) -> 
				if StringMap.mem s (fst loclook)
				then raise (Failure("The following variable, already declared: " ^ s))
				else (StringMap.add s Litnull (fst loclook), snd loclook)
			| Sast.Decassign(Identifier(s), e) ->
				let r, locklook = eval loclook (fst e) in
				if StringMap.mem s (fst loclook)
				then raise (Failure ("The following variable, already declared: " ^ s))
				else (StringMap.add s r (fst loclook), snd loclook)
			| Sast.Return((e, _)) ->
	  			let r, (locals, lookup) = eval loclook e in
	  			raise (ReturnException(r, lookup))
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
			Ast.Slide -> (locals, {lookupparam with 
				slides_out= StringMap.add (id_to_str fdef.name) (create_blank_slide fdef.name) lookupparam.slides_out;
				cur_slide = (id_to_str fdef.name);})
			| Ast.Comp -> (match fdef.inheritance with
				Some(Sast.Identifier("box")) -> (locals, {lookupparam with cur_element = Some(create_blank_element)})
				| Some(Sast.Identifier(s)) ->  
					let parent = 
						try StringMap.find s lookupparam.funcs_in
						with Not_found -> raise (Failure ("The following component is not defined: " ^ s))
					in
					let isComp = function Ast.Comp -> true | _ -> false in
					if isComp parent.t
					then
						let loclook = (locals, lookupparam) in
						let (paractuals, loclook) = 
							List.fold_left
							(fun (actuals, loclook) actual -> let (r, loclook) = eval loclook (fst actual) in (r :: actuals, loclook))
							([], loclook) (List.rev fdef.paractuals)
						in
						try (fst loclook, (call parent paractuals (snd loclook)))
						with ReturnException(r, lookup) -> (fst loclook, lookup)
					else
						raise (Failure ("A component can only inherit from a component for " ^ (id_to_str fdef.name)))
				| None -> raise (Failure ("The following component needs to inherit from a component: " ^ (id_to_str fdef.name)))
				)
			| _ -> (locals, lookupparam)
		in			
		(* Now recursively execute every statement with the updated locals *)
		snd (List.fold_left exec (lookup fdef.t) fdef.body) 
	in

	(* Here is where all the functions get called to produce the final output *)
	let lookup = create_lookup vars funcs in
	let pre_ir = 
		try (call (StringMap.find "main" lookup.funcs_in) [] lookup)  
		with Not_found -> raise (Failure ("There must exist a main() slide"))
	in
	(StringMap.fold (fun k d l -> d :: l) (pre_ir.slides_out) [], vars, funcs_to_js funcs)



