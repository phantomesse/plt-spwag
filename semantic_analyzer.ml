(*  Author: Richard Chiou
    Contributor: Aditya Majumdar
    Quick to do list, order by priority:
   	program: take in an Ast.program and output an Sast.program (compiles)
		helper function that parses identifier list and adds to symbol table (compiles)
		helper function that parses func_definition list and adds to symbol table (compiles)
	Return (how do we return the correct type?)
	check_function: see if function definition is valid (Compiles)
	check_func_call: see if function call is valid (Compiles)
	Declaration of identifier: Compiles
	Decassign of identifier * expr: Compiles
	
	Problematic code:
	Call of func_call: Prototype is written, but still needs to be debugged
	Component of identifier: This is hard to write, save this for last
*)

open Ast
module StringMap = Map.Make(String)

type symbol_table = {
    parent : symbol_table option;
    functions : Ast.func_definition list;
    variables : Sast.identifier list;
}

type translation_environment = {
    scope : symbol_table;
}

(* We need the types from SAST because of an oversight *)
type t = Int | Per | Str | Bool | Slidetype | Comptype | Attrtype | Functype | Varidentifier | Null

(* See if t1 and t2 have the same types *)
let types_equal t1 t2 = match t1,t2 with _, _ -> if (t1 = t2)
    then true
		else false

let identifier_of_string = function
    s -> Identifier(s)
		
let string_of_identifier = function
    Identifier(s) -> s
	
let string_of_expr = function
    s -> Litstr(s)
		
let string_of_type_t = function
      Sast.Int -> "Int"
    | Sast.Bool -> "Bool"
    | Sast.Str -> "Str"
    | Sast.Per -> "Per"
	| Sast.Slidetype -> "Slidetype"
	| Sast.Comptype -> "Comptype"
	| Sast.Attrtype -> "Attrtype"
	| Sast.Functype -> "Functype"
	| Sast.Varidentifier -> "Varidentifier"
	| Sast.Null -> ""

let string_of_func_type = function
	  Slide -> "Slide"
	| Comp -> "Comp"
	| Attr -> "Attr"
	| Func -> "Func"

let type_to_t = function
	  Slide -> Sast.Slidetype
	| Comp -> Sast.Comptype
	| Attr -> Sast.Attrtype
	| Func -> Sast.Functype

(* Convert AST identifier to SAST identifier *)
let identify env = function
    Ast.Identifier(v) -> Sast.Identifier(v)
    (*let vdecl = find_variable env.scope Ast.Identifier in (* Locate a variable by name *)
	Sast.Identifier(vdecl), Sast.Varidentifier*)

	
let string_of_expr = function
	| _ -> "(not implemented ... yet)"

(* Operations: Plus | Minus | Times | Divide | Equals | Notequals | Lessthan | Greaterthan | Or | And *)
let string_of_binop = function
	 Plus -> "Plus"
	| Minus -> "Minus"
	| Times -> "Times"
	| Divide -> "Divide"
	| Equals -> "Equals"
	| Notequals -> "Notequals"
	| Lessthan -> "Lessthan"
	| Greaterthan -> "Greaterthan"
	| And -> "And"
	| Or -> "Or"

	
(* This find_variable function is adapted from the slides *)
let rec find_variable scope name =
    try
		List.find (fun (s) -> s = name) scope.variables
    with Not_found ->
        match scope.parent with
        Some(parent) -> find_variable parent name
    | _ -> raise Not_found
	
(* This find_function function goes up to global scope before searching *)
let rec find_function scope name = (* name is an identifier *)
	let rec global scope = match scope.parent with	(* All functions are global *)
		| None -> scope
		| Some(parent) -> (global parent)
	in
	try
		List.find (fun {t=_; name=s; formals=_; inheritance=_; paractuals=_; body=_} -> s = name) (global scope).functions
	with Not_found -> (* Not found, print error message *)
		(*let build_string tmpString nextString = tmpString^" \n"^nextString in
		let func_names_string = List.fold_left build_string("") (List.map (fun {t=_, name=s, formals=_, inheritance=_, paractuals=_, body=_} -> n ) (getGlobalScope scope).functions) in
		let num_funcs = List.length (getGlobalScope scope).functions in*)
		raise(Failure("Function not found in global scope"))

(*  Evaluate func call: Evaluate identifier to be valid (not slide), evaluate actuals are valid expressions, evaluate mods are statements  *)



(* check to see if valid function call *)
let functioncall env call = 
	(*let actuallist = List.map (expr env) call.actuals 
	and id = identify env call.cname in*)
	let funct = find_function env call.cname in

	(*let parentscope = {
		parent = env.parent;
		functions = env.functions;
		variables = env.variables;
    } in*)
	
	let checked_func_call = {
		cname = funct.name;
		actuals = call.actuals; 	
		mods = call.mods;  
      } in
	checked_func_call	
		
(* let func_call_conversion env (fc: Ast.func_call) = { 	
	Sast.cname = identify env fc.cname;
	Sast.actuals = List.rev(List.fold_left (fun l e -> (expr env e)::l ) [] c.actuals);
	Sast.mods = Lst.hd snd(stmts(env, []) fc.mods)} in *)
	
(* Check if valid expression*)
let rec expr env = function

    (* Simple evaluation of primitives *)
    Ast.Litint(v) -> Sast.Litint(v), Sast.Int
  | Ast.Litper(v) -> Sast.Litper(v), Sast.Per
  | Ast.Litstr(v) -> Sast.Litstr(v), Sast.Str
  | Ast.Litbool(v) -> Sast.Litbool(v), Sast.Bool
  | Ast.Litnull -> Sast.Litnull, Sast.Null
  
  | Ast.Binop (expr1, op, expr2) ->  (* evaluate operators *)
    (
	let e1 = expr env expr1 and
		e2 = expr env expr2 in		
 
    let _, t1 = e1 (* Get the type of each child *) 
    and _, t2 = e2 in
	
	match t1, op, t2 with
	  | (Sast.Bool), (And | Or), (Sast.Bool) ->  (* And/or operators *)
		Sast.Binop(e1, op, e2), Sast.Bool (* Boolean *) 
				
      | (Sast.Int), (Lessthan | Greaterthan), (Sast.Int) ->  (* > , < *)
			Sast.Binop(e1, op, e2), Sast.Bool
					
	  | (Sast.Int), (Plus | Minus | Times | Divide), (Sast.Int) ->  (* Arithmetic on ints *)
			Sast.Binop(e1, op, e2), Sast.Int   

	  | (Sast.Per), (Plus | Minus | Times | Divide), (Sast.Per) ->  (* Arithmetic on percents *)
			Sast.Binop(e1, op, e2), Sast.Per   
			
      | _, (Equals | Notequals), _  ->   (* Compare Anything *)
			Sast.Binop(e1, op, e2), Sast.Bool
					
	  | (Sast.Str), Plus, (Sast.Str | Sast.Int | Sast.Bool) ->  (* String Concatenation *) 
			Sast.Binop(e1, op, e2), Sast.Str		
					
	 (* Otherwise Invalid *)
	  | a, op, b -> raise(Failure("Binop "^ (string_of_binop op) ^" has invalid operands "))
	)
	
  | Ast.Notop(v) -> (* check if negate = ! and e1 is a boolean *)
    (
	let e1 = expr env v in
	let _, t1 = e1 in (* Get the type of e1 *)
	match t1 with 
	  | Sast.Bool -> Sast.Notop(e1), Sast.Bool
	  | _ -> raise(Failure("Not operator requires bool operand"))
	)
	
  | Ast.Assign(lhs, rhs) -> 		
    let e1 = expr env rhs			(* check if valid expression *) 
	and id = identify env lhs in	(* check if identifier *)
    (* let _, t1 = e1 (* type of rhs *) in *)
	(*if (types_equal t1 t2) then *)			(* the types need to match? *)
		Sast.Assign(id, e1), Sast.Varidentifier
	(*else
		raise(Failure(string_of_type_t t1^" expression does not match identifier "^string_of_type_t t2))*)
	
  | Ast.Variable(v) -> 				(* If identify function works, this will work *)
	let id = identify env v in
	(*let _, t1 = id (* type of rhs *) in*)
	Sast.Variable(id), Sast.Varidentifier
	
	(* let string_of_identifier = function
    Identifier(s) -> s *)
  | Ast.Call(funccall: Ast.func_call) -> (
		let fc = functioncall env funccall in		(* Evaluate if this is a valid func_call *)
		let funct = find_function env fc.cname in	(* Check to see if said function exists *)
		(* We need to now check that the arguments are valid *)
		(* Do we even need to get types from identifiers somehow? *)
		let formallist = List.map (identify env) funct.formals in
		(*let formalstoidentifiers = List.map (string_of_identifier) formallist in
		let identifierstoexprs = List.map (Litstr)*)
		(*let formalTypes = List.map fst(formallist) in*)
		(* Get the list of actuals and their types *)
		let actualstostrings = List.map (string_of_expr) funccall.actuals in 	
		let stringstoids = List.map (identifier_of_string) actualstostrings in 
		let actuallist = List.map (identify env) stringstoids in
		(*let actualtypes = List.map fst(actuallist) in*)
		
		(* compare the formals and actuals *)
		let rec checktypes list1 list2 = match list1, list2 with
		| [], [] -> true
		| [], _ -> raise(Failure(" there should be no parameters "))
		| _ , [] -> raise(Failure(" there are missing parameters "))
		| _ , _ -> try
		  ( types_equal (List.hd(list1)) (List.hd(list2)) ) && types_equal (List.tl(list1)) (List.tl(list2))
			with Failure("hd") -> raise(Failure(" mismatched types "))
		in
		if (checktypes formallist actuallist) then 
			(Sast.Call (fc)), type_to_t funct.t	(*Problematic Code*)
	    else
			raise(Failure("Arguments for function do not match those given in the definition"))
		)

  (* Component of identifier: identifier has to be slide or variable (component or slide) *)
  | Ast.Component (v, exprlist) ->
  
		let checkedexprs = List.map (expr env) exprlist in
		let id = identify env v in 					(* How are we going to get the type of the identifier? *)
		let funct = find_function env v in
		let typ = funct.t in
		let strings = List.map(string_of_expr) exprlist in
		let ids = List.map(identifier_of_string) strings in

		if ((types_equal typ Comp) || (types_equal typ Slide)) then	
			(* We need to do recursion. Here's the general idea:
			Base step: currentobject = id
			Step 1: currentobject = id[exprlist[0]]
			Step 2: remove exprlist[0]
			Step 3: Go to step 1, break from loop when exprlist has been parsed through *)		
			let rec compfind v exprs comptype = match exprs with
		  | [] -> Sast.Component(id, checkedexprs), Sast.Comptype
		  | x -> 
				let innercomp = List.hd exprs
				and newlist = List.tl exprs in
				let compfunct = find_function env innercomp in
				let comptyp = compfunct.t in
				if ((types_equal comptyp Comp) || (types_equal comptyp Slide)) then
					compfind innercomp newlist comptyp
				else 
					raise(Failure("Can only take component of slide or other components"))
				in compfind v ids typ 
			
		else 
			raise(Failure("Can only take component of slide or other components"))
				
  (*Below code is old
  
  | Ast.Assign(id, e1) -> (* General idea is to make sure the arguments are valid; code may not work though *)
    let vdecl = try
        find_variable env.scope id
    with Not_found ->
        raise (Error("undeclared identifier " ^ id))
    in
    let expreval = try
        find_expr env.scope
        find_variable env.scope e1
    with Not_found ->
        raise (Error("undeclared expression " ^ e1))
    in
    let (_, id_type) = vdecl in (* get the variable's type *)
    let (_, expr_type ) = expreval in
    Sast.Assign(id, e1), id_type, expr_type
*)

let rec stmts (env, stmtlist) stmt =
	let newscope = {
	  parent =  Some env;  (* set parent to newglobalscope parameter *)
	  functions = [];
	  variables = []; 
	} in
	match stmt with 
	
    | Ast.Expr(e) ->  env, Sast.Expr(expr env e)::stmtlist
    (*| Ast.Return(e1) -> Sast.Return(expr env e1) (* I have not written checking for e1 yet *)*)
    | Ast.Block(b) -> env, Sast.Block(snd(List.fold_left stmts(newscope,[]) b))::stmtlist
	| Ast.Return(e) -> 		(* Only funcs can call return, so no error-checking should be fine *)
		let e1 = expr env e in
		(*let _, t1 = e1 in
		if (typeEq t1 returntype )
		  then*) env, Sast.Return(e1)::stmtlist
		(*else raise(Failure("Return type of function body doesn't match: found"^(string_of_datatype t1)^" but expected "^(string_of_datatype returntype)))*)
	
(*	| Ast.Block(s1) ->  (* This block code is modified from Edwards' slides and does not work *)
		let newscope = { S.parent = Some(env.scope); S.variables = []; S.functions = [] } in
         (* New environment: same, but with new symbol tables *)
            let env' = { env with scope = newscope } in
            let s1 = List.map (fun s -> stmt newenv s) s1 in
            newscope.S.variables <- List.rev newscope.S.variables; (* side-effect *)
            Sast.Block(newscope, s1)
    | Ast.If(e, s1, s2) ->  (
        let e1 = expr env e in
		let _, t1 = e1 in 	(* Get the type of e1 *)
		if (types_equal t1 Sast.Bool) then 
			let newscope, thenblock = (stmts(
			Sast.If (e1, stmt env s1, stmt env s2) (* Check then, else *)
		else
			raise(Failure(string_of_type_t t1^"type must be bool"))
	  )*)

    | Ast.If(e, s1, s2) ->
		let e1 = expr env e in
		let _, t1 = e1 in
		if (types_equal t1 Sast.Bool) then 
			let nextscope, thenblock = (stmts(newscope, []) s1) in 
			let _, elseblock = (stmts (newscope, []) s2) in
			try
				nextscope, Sast.If( e1, List.hd(thenblock), List.hd(elseblock) )::stmtlist
			with Failure("hd") -> raise(Failure("If failed"))
		else raise(Failure("predicate of If should be bool but found "^(string_of_type_t t1)))
	  
(*	| Ast.While(e, s1) ->  (
		let e1 = expr env e in
		let _, t1 = e1 in 	(* Get the type of e1 *)
		if (types_equal t1 Sast.Bool) then
			Sast.While (e1, stmt env s1) (* Check body *)
		else
			raise(Failure(string_of_type_t t1^"type must be bool"))
	  )
*)
	| Ast.While(e, s) ->
		let e1 = expr env e in
		let _, t1 = e1 in
		if (types_equal t1 Sast.Bool) then
			let nextscope, loopbody = (stmts(newscope, []) s) in
			try
				nextscope, Sast.While(e1, List.hd(loopbody))::stmtlist
			with Failure("hd") -> raise(Failure("While failed"))
		else raise(Failure("predicate of If should be bool but found "^(string_of_type_t t1)))
		
	| Ast.Declaration(v) ->
		let id = identify env v in
		if (find_variable env id = id) then (* If declaration exists, don't allow duplicate *)
			raise(Failure("Variable already exists"))	(* We could use an identifier to string function *)
		(* else we have to add the variable declaration to the symbol table*)
		else
			ignore (id::(env.variables));			
			env, Sast.Declaration(id)::stmtlist
                
	| Ast.Decassign(v, e) ->
		let id = identify env v in
		if (find_variable env id = id) then (* If declaration exists, don't allow duplicate *)
			raise(Failure("Variable already exists"))	(* We could use an identifier to string function *)	
		else
			ignore (id::(env.variables)); 
			let e = expr env e in
			env, Sast.Decassign(id, e)::stmtlist
		(*let _, t2 = e in
		if (types_equal t1 t2) then	(* do variable types need to match? *)
							(* we have to add the variable declaration to the symbol table; I'll write this later *)
			Sast.Decassign(v, e), t1 (* Declaring a variable and then assigning it something*)
		else	
			raise(Failure(string_of_type_t t1^" expression does not match identifier "^string_of_type_t t2))	*)

(* check to see if func_definition is valid, and return function that is evaluated *)
let check_function env func_definition = match func_definition.body with
	[] -> raise(Failure("Empty functions are invalid")) (* Empty functions not allowed *)
  | x ->
        (*let return_type = func_definition.t in Which type the function returns: this is unused because design issues *)
        (* Why are we even doing this return stuff *)
		(* let returnidentifier = {
          t = func_definition.t;
          body = func_definition.body; (* "return" ?*)
          name = func_definition.name;
          formals = func_definition.formals;
          paractuals = func_definition.paractuals;
          inheritance = func_definition.inheritance;
} in *)

	let parentscope = {		
		parent = env.parent;
		functions = env.functions;
		variables = (List.map (identify env) func_definition.formals)@(env.variables);	
    } in
	let checked_func_definition = {
		t = func_definition.t;
        name = func_definition.name;
        formals = func_definition.formals;
		inheritance = func_definition.inheritance; 	(* how are we going to deal with inheritance? *)
	    paractuals = func_definition.paractuals;
        body = fst(x, (List.fold_left stmts(parentscope, []) func_definition.body ));  
      } in
	checked_func_definition

(* Add a list of func_definitions to the symbol table *)
let add_func_definitions env funcdefs =
	let addfunc scope funcdef =  
	  { parent = scope.parent;
		functions = funcdef::scope.functions;
		variables = scope.variables }
	in
	let global = List.fold_left addfunc(env) funcdefs (* new scope containing all func_definitions *)
	in  
	let subscope = {
		parent =  Some global;  
		functions = [];
		variables = []; 
	}
	in (List.map (check_function subscope) funcdefs), global  (* newFdecls, newScope *)
	
(* Add a list of identifiers to the symbol table *)
let add_identifiers env identifiers =
	let addid scope id =  
	  { parent = scope.parent;
		functions = scope.functions;
		variables = id::scope.variables }
	in
	let global = List.fold_left addid(env) identifiers (* new scope containing all func_definitions *)
	in  
	let subscope = {
		parent =  Some global;  
		functions = [];
		variables = []; 
	}
	in let check_identifier env identifier = (* Check to see if identifier is valid: namely, does it exist in the symbol table already? *)
	if (types_equal (find_variable env identifier) identifier) then
		raise(Failure("Existing variable declaration"))
	else
		identifier
	in (List.map (check_identifier subscope) identifiers), global  (* newFdecls, newScope *)

(* Run program with input: Ast.Program, Symbol_Table and output: Sast.Program *)
(* type program = identifier list * func_definition list (* global vars, funcs*) *)
(* Add the identifiers (variables) and function definitions to global scope *)
let evalprogram program globalTable =
    let identifiers, funcdefs = program in	(*Get the identifier list and the func_definition list*)
	let ids = add_identifiers globalTable identifiers
	and funcs = add_func_definitions globalTable funcdefs in
	let output = ids, funcs in
	output