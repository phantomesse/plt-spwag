(* Authors: Yunhe (John) Wang and Aftab Khan *)
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
    | ("position_x", Ir.Litint(i)) -> {elementp with style= {elementp.style with position_x=(string_of_int i)}}
    | ("position_y", Ir.Litstr(s)) -> {elementp with style= {elementp.style with position_y=s}}
    | ("margin_top", Ir.Litstr(s)) -> {elementp with style= {elementp.style with margin_top=s}}
    | ("margin_bottom", Ir.Litstr(s)) -> {elementp with style= {elementp.style with margin_bottom=s}}
    | ("margin_left", Ir.Litstr(s)) -> {elementp with style= {elementp.style with margin_left=s}}
    | ("margin_right", Ir.Litstr(s)) -> {elementp with style= {elementp.style with margin_right=s}}
    | ("padding_top", Ir.Litstr(s)) -> {elementp with style= {elementp.style with padding_top=s}}
    | ("padding_bottom", Ir.Litstr(s)) -> {elementp with style= {elementp.style with padding_bottom=s}}
    | ("padding_left", Ir.Litstr(s)) -> {elementp with style= {elementp.style with padding_left=s}}
    | ("padding_right", Ir.Litstr(s)) -> {elementp with style= {elementp.style with padding_right=s}}
    | ("text_color", Ir.Litstr(s)) -> {elementp with style= {elementp.style with text_color=s}}
    | ("background_color", Ir.Litstr(s)) -> {elementp with style= {elementp.style with background_color=s}}
    | ("font", Ir.Litstr(s)) -> {elementp with style= {elementp.style with font=s}}
    | ("font_size", Ir.Litstr(s)) -> {elementp with style= {elementp.style with font_size=s}}
    | ("font_decoration", Ir.Litstr(s)) -> {elementp with style= {elementp.style with font_decoration=s}}
    | ("border", Ir.Litstr(s)) -> {elementp with style= {elementp.style with border=s}}
    | ("border_color", Ir.Litstr(s)) -> {elementp with style= {elementp.style with border_color=s}}
    | ("width", Ir.Litstr(s)) -> {elementp with style= {elementp.style with width=s}}
    | ("height", Ir.Litstr(s)) -> {elementp with style= {elementp.style with height=s}}
    | ("id", Ir.Litstr(s)) -> {elementp with id=s}
    | ("image", Ir.Litstr(s)) -> {elementp with image=s}
    | ("text", Ir.Litstr(s)) -> {elementp with text=s}
	| (_,_) -> raise (Failure ("The following built-in attribute is not used correctly: " ^ attribute))
	)	

(* Binds css to a slide 
 * @param attribute the css attribute, as a string
 * @param value the value to bind it to, Ir.literal
 * @param slidep the Slide.slide
 * @return the updated slide
 *)
let ir_bind_css_slide attribute value (slidep:Slide.slide) = (match (attribute, value) with
	("padding_top", Ir.Litint(i)) -> {slidep with style= {slidep.style with padding_top=(string_of_int i)}}
	| (_,_) -> raise (Failure ("The following built-in attribute is not used correctly: " ^ attribute))
	)




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
		(* This part is easy, can do when half asleep
			Sast.Binop(e1, op, e2) ->
				let r1, loclook = eval loclook e1 in
				let r2, loclook = eval loclook e2 in
				let boolean i = if i then 1 else 0 in
				(match r1, op, r2 with
					Ir.Litint(i1), Plus, Ir.Litint(i2) -> (Ir.Litint(i1 + i2), loclook)
					| Ir.Litint(i1), Minus, Ir.Litint(i2) -> (Ir.Litint(i1 - i2), loclook)
					| Ir.Litint(i1), Times, Ir.Litint(i2) -> (Ir.Litint(i1 * i2), loclook)
					| Ir.Litint(i1), Divide, Ir.Litint(i2) -> (Ir.Litint(i1 / i2), loclook)
					| 
				  | (Ir.Litbool), (And | Or), () ->  (* And/or operators *)
						Sast.Binop(e1, op, e2), Sast.Bool (* Boolean *) 
							
				  | (Int), (Lessthan | Greaterthan), (Int) ->  (* > , < *)
						Sast.Binop(e1, op, e2), Sast.Bool
								
				  | (Int), (Plus | Minus | Times | Divide), (Int) ->  (* Arithmetic on ints *)
						Sast.Binop(e1, op, e2), Sast.Int   
				
				  | (Per), (Plus | Minus | Times | Divide), (Per) ->  (* Arithmetic on percents *)
						Sast.Binop(e1, op, e2), Sast.Per   
						
				  | _, (Equals | Notequals), _  ->   (* Compare Anything *)
						Sast.Binop(e1, op, e2), Sast.Bool
								
				  | (Str), Plus, (Str | Int) ->  (* String Concatenation *) 
						Sast.Binop(e1, op, e2), Sast.Str		
								
				 (* Otherwise Invalid *)
				  | a, op, b -> raise(Failure("Binop "^ (string_of_binop op) ^" does not work with operands "^ (string_of_type_t a) ^", "^ (string_of_type_t b) ^ "\n"))
				)
				
				
				(match op with
				  Add -> v1 + v2
				| Sub -> v1 - v2
				| Mult -> v1 * v2
				| Div -> v1 / v2
				| Equal -> boolean (v1 = v2)
				| Neq -> boolean (v1 != v2)
				| Less -> boolean (v1 < v2)
				| Leq -> boolean (v1 <= v2)
				| Greater -> boolean (v1 > v2)
				| Geq -> boolean (v1 >= v2)), env
				|
				*)
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
				let (_, loclook) = (eval loclook (Sast.Variable(i))) in
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
				(* TODO: Check for built-in functions *)
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
					Identifier("position-x") -> process_built_in_attr "position-x"
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
		(* TODO: Local variable scoping doesn't work yet *)
		and exec loclook = function
			Sast.Block(stmts) -> let (_, lookup) = (List.fold_left exec loclook stmts) in (fst loclook, lookup) 
			| Sast.Expr(e) -> let _, loclook = eval loclook (fst e) in loclook
			| Sast.If(e, s1, s2) ->  
	  			let v, loclook = eval loclook (fst e) in
				(match v with
					Ir.Litbool(false) -> exec loclook s2
					| Ir.Litint(0) -> exec loclook s2
					| Ir.Litper(0) -> exec loclook s2
					| Ir.Litstr("") -> exec loclook s2
					| Ir.Litnull -> exec loclook s2
					| _ -> exec loclook s1)
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
				in loop loclook
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

