(* Authors: Yunhe (John) Wang and Aftab Khan *)
(* Reference the Ast only for function types and operators, all else is in sast/ir *)

open Sast
open Ir

(* Lookup table *)
type lookup_table = {
    lfuncs 			: func_definition StringMap.t; (* Map function names to functions *)
	mutable lvars  	: literal StringMap.t; 	(* Global Variables, assignments can be changed *)
	mutable lpath	: string list; (* Path, starting from slide, to the current slide/element in scope *)
}

(* John says: I agree, identifier is annoying, oh well*)
let id_to_str = function Identifier(s) -> s 

(* Converts funcs (Not attrs/comps/slides) to js definitions *)
let funcs_to_js funcs = 
	List.fold_left (fun jss (func:Sast.func_definition) -> match func.t with
					Ast.Func -> {Ir.name=func.name;formals=func.formals;body=func.body}:: jss
					| _ -> jss
					) [] funcs 

(* Creates a blank Ir.slide given its id (that is, the slide name) *)
let create_blank_slide i = 
	let fill_css = 
		{Slide.padding_top="";padding_bottom="";padding_left="";padding_right = "";
    	text_color= "";background_color = "";font = "";font_size = "";font_decoration = "";
    	border = "";border_color = "";}
	in
	{Slide.id=i;next="";prev="";image="";style=fill_css;onclick=None;onpress=None;elements=StringMap.empty}

(* Main function that performs IR generation *)
let generate (vars, funcs) =

	(* Create lookup table 
	 * @param vars is the variables from Sast.program
	 * @param funcs is the function definitions from Sast.program 
	 * @return the lookup table as specified *)
	let create_lookup vars funcs = 
		let fill_funcs lookup (func : Sast.func_definition) = 
			try ignore (StringMap.find (id_to_str func.name) lookup.lfuncs); 
				raise (Failure ("There are two definitions for function name " ^ (id_to_str func.name)))
			with Not_found ->
				{lookup with lfuncs = StringMap.add (id_to_str func.name) func lookup.lfuncs} 	
		in 
		let fill_vars lookup id = {lookup with lvars=StringMap.add (id_to_str id) Litnull lookup.lvars}
		in
		(List.fold_left fill_vars (List.fold_left fill_funcs 
										({lfuncs=StringMap.empty; 
										  lvars=StringMap.empty;
										  lpath=[]}) funcs) vars)
	in 

	(* This calls a function to generate ir
	 * @param fdef is the function definition (Sast.func_definition)
	 * @param actuals are the actual parameter list, of literals
	 * @param lookup is the lookup table 
	 * @param output is the string map of slides to be outputted
	 * @return (lookup, output) *)
	let rec call (fdef:Sast.func_definition) lookup actuals output = 
		
		let rec exec env body = 
			let elocals (a,_,_) = a in
			let elookup (_,b,_) = b in
			let eoutput (_,_,c) = c in
			(elocals env, elookup env, eoutput env) in
		
		(* This section takes care of scoping issues before calling exec on statements *)
		(* Assign the locals from the actual parameters *)
		let locals = 
      		try List.fold_left2
	  			(fun locals formal actual -> StringMap.add (id_to_str formal) actual locals)
	  			StringMap.empty fdef.formals actuals
      		with Invalid_argument(_) ->
				raise (Failure ("Wrong number of arguments passed to " ^ (id_to_str fdef.name)))
		in
		(* Next, functions to update, rollback the path, and get 2nd,3rd elements *)
		let update_lpath lookup s = {lookup with lpath = List.rev (s::lookup.lpath)} in
		let rollback_lpath lookup = {lookup with lpath = List.rev (List.tl (List.rev lookup.lpath))} in
		let sndthird (_,a,b) = (rollback_lpath a,b) in
		(* TODO: Worry about inheritance *)
		(* Now recursively execute every statement with the updated locals *)
		sndthird (List.fold_left exec (locals, update_lpath lookup (id_to_str fdef.name), output) fdef.body) 
	in

	(* Here is where all the functions get called to produce the final output *)
	let lookup = create_lookup vars funcs in
	let pre_ir = 
		try call (StringMap.find "main" lookup.lfuncs) lookup [] StringMap.empty  
		with Not_found -> raise (Failure ("There must exist a main() slide"))
	in
	(StringMap.fold (fun k d l -> d :: l) (snd pre_ir) [], vars, funcs_to_js funcs)

	
