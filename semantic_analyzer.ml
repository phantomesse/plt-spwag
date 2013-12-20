(* Author: Richard Chiou
   Contributor: Aditya Majumdar
   Quick to do list:
	functioncall: checks if function call has correct structure
	Component of identifier: This is hard to write
	Call of func_call: Easier to write but still annoying
	Declaration of identifier: need to write add identifier to symbol table
	Decassign of identifier * expr: need to write add to symbol table
	func_def: checks if function definition has valid table
	program: take in an Ast.program and output an Sast.program
		add_identifier: adds identifier to symbol table
		add_function: adds function to symbol table
*)

open Ast
module StringMap = Map.Make(String)

type symbol_table = {
    parent : symbol_table option;
    functions : Ast.func_definition list;
    variables : Ast.identifier list;
}

type translation_environment = {
    scope : symbol_table;
}

(* See if t1 and t2 have the same types *)
let types_equal t1 t2 = match t1,t2 with _, _ -> if (t1 = t2)
    then true
		else false

let string_of_type_t = function
      Int -> "Int"
    | Bool -> "Bool"
    | Str -> "Str"
    | Per -> "Per"
	| Slidetype -> "Slidetype"
	| Comptype -> "Comptype"
	| Attrtype -> "Attrtype"
	| Functype -> "Functype"
	| Varidentifier -> "Varidentifier"
	| Null -> ""

let string_of_func_type = function
	  Slide -> "Slide"
	| Comp -> "Comp"
	| Attr -> "Attr"
	| Func -> "Func"
	
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

(*  Evaluate func call: Evaluate identifier to be valid (not slide), evaluate actuals are valid expressions, evaluate mods are statements 
	Component of identifier: identifier has to be slide or variable (component or slide) 
	expr list are strings
*)

(* Handles calls of functions and components 
type func_call = { 
    cname : identifier; (* Name of the function *)
    actuals : expr list; (* Evaluated actual parameters *)
    mods : stmt; (* Additional statements, which could be a block *)
}  *)
(*let functioncall env = function*)

(* Check if valid identifier *)
let identify env = function
    Ast.Identifier(v) -> Sast.Identifier(v)
    (*let vdecl = find_variable env.scope Ast.Identifier in (* Locate a variable by name *)
	Sast.Identifier(vdecl), Sast.Varidentifier*)

(* Check if valid expression*)
let rec expr env = function

    (* Simple evaluation of primitives *)
    Ast.Litint(v) -> Sast.Litint(v), Ast.Int
  | Ast.Litper(v) -> Sast.Litper(v), Ast.Per
  | Ast.Litstr(v) -> Sast.Litstr(v), Ast.Str
  | Ast.Litbool(v) -> Sast.Litbool(v), Ast.Bool
  | Ast.Litnull -> Sast.Litnull, Ast.Null
  
  | Ast.Binop (expr1, op, expr2) ->  (* evaluate operators *)
    (
	let e1 = expr env expr1 and
		e2 = expr env expr2 in		
 
    let _, t1 = e1 (* Get the type of each child *) 
    and _, t2 = e2 in
	
	match t1, op, t2 with
	  | (Bool), (And | Or), (Bool) ->  (* And/or operators *)
			Sast.Binop(e1, op, e2), Ast.Bool (* Boolean *) 
				
      | (Int), (Lessthan | Greaterthan), (Int) ->  (* > , < *)
			Sast.Binop(e1, op, e2), Ast.Bool
					
	  | (Int), (Plus | Minus | Times | Divide), (Int) ->  (* Arithmetic on ints *)
			Sast.Binop(e1, op, e2), Ast.Int   

	  | (Per), (Plus | Minus | Times | Divide), (Per) ->  (* Arithmetic on percents *)
			Sast.Binop(e1, op, e2), Ast.Per   
			
      | _, (Equals | Notequals), _  ->   (* Compare Anything *)
			Sast.Binop(e1, op, e2), Ast.Bool
					
	  | (Str), Plus, (Str | Int) ->  (* String Concatenation *) 
			Sast.Binop(e1, op, e2), Ast.Str		
					
	 (* Otherwise Invalid *)
	  | a, op, b -> raise(Failure("Binop "^ (string_of_binop op) ^" does not work with operands "^ (string_of_type_t a) ^", "^ (string_of_type_t b) ^ "\n"))
	)
	
  | Ast.Notop(v) -> (* check if negate = ! and e1 is a boolean *)
    (
	let e1 = expr env v in
	let _, t1 = e1 in (* Get the type of e1 *)
	match t1 with 
	  | Bool -> Sast.Notop(e1), Ast.Bool
	  | _ -> raise(Failure("Invalid operand ("^string_of_type_t t1^") for not operator"))
	)
	
  | Ast.Assign(lhs, rhs) -> 		
    let e1 = expr env rhs			(* check if valid expression *) 
	and id = identify env lhs in	(* check if identifier *)
    (* let _, t1 = e1 (* type of rhs *) in *)
	(*if (types_equal t1 t2) then *)			(* the types need to match? *)
		Sast.Assign(id, e1), Ast.Varidentifier
	(*else
		raise(Failure(string_of_type_t t1^" expression does not match identifier "^string_of_type_t t2))*)
	
  | Ast.Variable(v) -> 				(* If identify function works, this will work *)
	let id = identify env v in
	(*let _, t1 = id (* type of rhs *) in*)
	Sast.Variable(id), Ast.Varidentifier
	
  | Ast.Call(funccall) -> (
	 (* Evaluate if this is a valid func_call: 
		cname : identifier; (* Name of the function *)
		actuals : expr list; (* Evaluated actual parameters *)
		mods : stmt; (* Additional statements, which could be a block *) *)
		(*let id = functioncall env funccall in *)
		let funct = find_function env funccall.cname in	(* Check to see if said function exists *)
		(* We need to now check that the arguments are valid *)
		(* Do we need to getting types from identifiers somehow? *)
		let formallist = List.map (identify env) funct.formals in
		(*let formalTypes = List.map fst(formallist) in*)
		(* Get the list of actuals and their types *)
		let actuallist = List.map (identify env) funccall.actuals in
		(*let actualtypes = List.map fst(actuallist) in*)
		
		(* check each type from checked list against fdecl param types in scope's function list *)
		let rec checktypes list1 list2 = match list1, list2 with
		| [], [] -> true
		| [], _ -> raise(Failure(" there should be no parameters "))
		| _ , [] -> raise(Failure(" there are missing parameters "))
		| _ , _ -> try
		  ( types_equal (List.hd(list1)) (List.hd(list2)) ) && types_equal (List.tl(list1)) (List.tl(list2))
			with Failure("hd") -> raise(Failure(" mismatched types "))
		in
		if (checktypes formallist checkedlist)
			then Sast.Call (id), funct.t
	    else
			raise(Failure("Arguments for function do not match those given in the definition"))
		)
  
  (* Component of identifier: identifier has to be slide or variable (component or slide) *)
  | Ast.Component (v, exprlist) ->
		let id = identify env v in
		(* How are we going to get the type of the identifier? *)
		
		(* We need to do recursion. Here's the general idea:
		Base step: currentobject = id
		Step 1: currentobject = id[exprlist[0]]
		Step 2: remove exprlist[0]
		Step 3: Go to step 1, break from loop when exprlist has been parsed through
		Hardest part is the error checking, but the recursive function will be annoying as well... *)

		(*let rec returncomp currentcomp exprlist = match exprlist with
		| [] -> (* run some code *)
		| _ ->*)  Sast.Component (id, (expr env exprlist)), Ast.Varidentifier

			
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

(*stmt = (* Statements ; WIP *)
      Block of stmt list (* { ... } *) Taken from Edwards' slides, but it probably doesn't work
    | Expr of expr (* foo = bar + 3; *) Done!
    | Return of expr (* return 42; *) Done!
    | If of expr * stmt * stmt (* if (foo == 42) stmt1 else stmt2 end *) Should work
    | While of expr * stmt (* while (i<10) \n  i = i + 1 \n end \n *) Should work
    | Declaration of identifier (* Declaring a variable *) Partially complete
    | Decassign of identifier * expr (* Declaring a variable and then assigning it something *) Partially complete
	*)

let rec stmt env = function
    | Ast.Expr(e) ->    Sast.Expr(expr env e)
    | Ast.Return(e1) -> Sast.Return(expr env e1) (* I have not written checking for e1 yet *)
	
    (*| Ast.Block(s1) ->  let scope' = { S.parent = Some(env.scope); S.variables = [] }
                        and exceptions' = { excep_parent = Some(env.exception_scope); exceptions = [] }
                        in
                            (* New environment: same, but with new symbol tables *)
                            let env' = { env with scope = scope';
                                         exception_scope = exceptions' } in
                            let s1 = List.map (fun s -> stmt env' s) s1 in
                                scope'.S.variables <- List.rev scope'.S.variables; (* side-effect *)
                                Sast.Block(scope', s1)
	This block code needs to be rewritten *)
    | Ast.If(e, s1, s2) ->  (
        let e1 = expr env e in
		let _, t1 = e1 in 	(* Get the type of e1 *)
		match t1 with 
	  | Bool -> Sast.If (e1, stmt env s1, stmt env s2) (* Check then, else *)
	  | _ -> raise(Failure(string_of_type_t t1^"type must be bool"))
	  )
	  
	| Ast.While(e, s1) ->  (
		let e1 = expr env e in
		let _, t1 = e1 in 	(* Get the type of e1 *)
		match t1 with 
	  | Bool ->	Sast.While (e1, stmt env s1) (* Check body *)
	  | _ -> raise(Failure(string_of_type_t t1^"type must be bool"))
	  )
	  
	| Ast.Declaration(v) ->
		if (find_variable env v = v) then (* If declaration exists, don't allow duplicate *)
			raise(Failure("Existing variable declaration for "^string_of_expr v))	(* We need to write a identifier to string function *)
		(* else we have to add the variable declaration to the symbol table; I'll write this later *)
		else
			let id = identify env v in Sast.Declaration(id)
                
	| Ast.Decassign(v, e) ->
		if (find_variable env v = v) then (* If declaration exists, don't allow duplicate *)
			raise(Failure("Existing variable declaration for "^string_of_expr v))	(* We need to write a identifier to string function *)	
		else
		let id = identify env v 
		and e = expr env e in
		Sast.Decassign(id, e)
		(*let _, t2 = e in
		if (types_equal t1 t2) then	(* variable types need to match *)
							(* we have to add the variable declaration to the symbol table; I'll write this later *)
			Sast.Decassign(v, e), t1 (* Declaring a variable and then assigning it something*)
		else	
			raise(Failure(string_of_type_t t1^" expression does not match identifier "^string_of_type_t t2))	*)

(* Removed parent declaration
let rec parent env = function
    | Ast.Parent(id) -> (* This code is bugged *)
		let vdecl = try find_variable env.scope id
            with Not_found ->
                raise (Failure("undeclared identifier " ^ id))
            in
			let (_, id_type) = vdecl in (* get the variable's type *)
                Sast.Parent(id), id_type
    | Ast.Noparent(v) -> Sast.Noparent(v), Sast.null*)

(* Taken from CGL programming language, modify this to fit ours
let checkFunc scope func_def = match func_def.fbody with
	| [] -> raise(Failure("Empty functions are invalid"))
    | x ->
		let return_type = fdecl.t (* What the function returns *)
		let returnidentifier = {
			vdt = fdecl.t;
			vname = "return";
			value = Ast.Noexpr;
		} in
		let formalToVdecl = function
			| frml -> { vdt = frml.pdt;
				          vname = frml.pname;
									value = Ast.Noexpr }
		in
	  let retScope = {
			parent = curScope.parent;
			functions = curScope.functions;
			variables = (List.map formalToVdecl (fdecl.formals) )@(retVdecl::curScope.variables);
    } in
		let checkedFdecl = 
		{
		  fdt = fdecl.fdt;
		  fname = fdecl.fname;
		  formals = fdecl.formals;
		  fbody = fst(x, (List.fold_left processStatement(retScope, []) fdecl.fbody ));   (* UGLY HACK (x) *)
		} in
		checkedFdecl
*)

(*let add_func_definition scope func = 
| "SETUP", fdecl_list ->
	(
	  (* LOAD all funcs in list into symbol table (b/c all funcs should be able to "find" i.e. call each other) *)
	    let addFunc scope fdecl =  
				{ parent = scope.parent;
				  functions = fdecl::scope.functions;
				  variables = scope.variables }
			in
			let newGlobal = List.fold_left addFunc(scope) fdecls (* new scope containing all funcs in setup *)
		  in  
	    let subScope = {
			  parent =  Some newGlobal;  (* set parent to newglobalscope parameter *)
			  functions = [];
			  variables = []; 
		  }
		  in
		  (* MAP through each function and check *)
		  (List.map (processFdecl subScope) fdecl_list), newGlobal  (* newFdecls, newScope *)
   )
| _ , [] ->   [], scope  (* Non-"SETUP" bname should have empty fdecls list *)
| _ , _  ->   raise(Failure("Functions can only be declared in SETUP (at beginning)")) 
*)



(* Run our program *)
(* Input: Ast.Program, Symbol_Table *)
(* Output: Sast.Program *)
(* This is WIP *)

(* type program = identifier list * func_definition list (* global vars, funcs*) *)
(* Add the identifiers (variables) and function definitions to global scope *)

let evalprogram program globalTable =
	let run, _ = List.fold_left processBdecl(globalTable, []) program in
	run