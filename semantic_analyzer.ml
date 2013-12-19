(* Authors: Richard Chiou and Aditya Majumdar *)
(* Quick to do list:
	func_call
	Component of identifier
	Call of func_call
	Declaration of identifier: add identifier to symbol table
	Decassign of identifier * expr: add to symbol table
	func_definition: Aditya wrote one, but it's heavily bugged at the moment
	program: take in an Ast.program and output an Sast.program
		and associated functions to inspect the identifier and func_definition lists
		These associated functions will be extremely long and painful to write
*)

open Ast
open Sast
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
let rec find_function scope name =
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
		raise(Failure("Function "^name^" not found in global scope"))

(*  Evaluate func call: Evaluate identifier to be valid (not slide), evaluate actuals are valid expressions, evaluate mods are statements 
	Component of identifier: identifier has to be slide or variable (component or slide) 
	expr list are strings
*)

(* Check if valid identifier *)
let rec identify env = function
    Ast.Identifier(v) -> Sast.Identifier(v)
    (*let vdecl = find_variable env.scope Ast.Identifier in (* Locate a variable by name *)
	Sast.Identifier(vdecl), Sast.Varidentifier*)

(* Check if valid expression*)
let rec expr env = function

    (* Simple evaluation of primitives *)
    Ast.Litint(v) -> Sast.Litint(v), Sast.Int
  | Ast.Litper(v) -> Sast.Litper(v), Sast.Per
  | Ast.Litstr(v) -> Sast.Litstr(v), Sast.Str
  | Ast.Litbool(v) -> Sast.Litbool(v), Sast.Bool
  | Ast.Litnull() -> Sast.Litnull(), Sast.Null
  
  | Ast.Binop (expr1, op, expr2) ->  (* evaluate operators *)
    (
	let e1 = expr env expr1 and
		e2 = expr env expr2 in		
 
    let _, t1 = e1 (* Get the type of each child *) 
    and _, t2 = e2 in
	
	match t1, op, t2 with
	  | (Bool), (And | Or), (Bool) ->  (* And/or operators *)
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
	
  | Ast.Notop(v) -> (* check if negate = ! and e1 is a boolean *)
    (
	let e1 = expr env v in
	let _, t1 = e1 in (* Get the type of e1 *)
	match t1 with 
	  | Bool -> Sast.Notop(e1), Sast.Bool
	  | _ -> raise(Failure("Invalid operand ("^string_of_type_t t1^") for not operator"))
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
  
  (* Component of identifier: identifier has to be slide or variable (component or slide) *)
	  
  (* Following are problematic: 
  | Component of identifier * expr list (* identifier["child"]["child"] etc. to fetch component *)
  | Call of func_call (* Calling a function, unique in that it can contain statements *)

	Below code is old
  
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

(* Below section modified from chartlan of f2012

  TODO: Inheritance?

We have:                        They have:
t: func_type                    returntype : Types.t
name: identifier                fname: string
formals : identifier list       formals: variable_decl list
inheritance : parent            ---- ??? ---- ??? ---- ??????????
paractuals : expr list          locals: variable_decl list
body : stmt list                body: stmt list

*)
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

(*let rec func_definition = function
    in let trans_func env (f:Sast.func_definition) =  
    let sf = find_function env.scope f.name
    in let functions' = List.filter (fun f -> f.f_name != sf.f_name) env.scope.functions
    in let scope' = {parent = Some(env.scope); variables = []; functions = []}
    in let env' = {env with scope = scope'}
    in let env' = List.fold_left add_local env' (f.formals)
    in let formals' = env'.scope.variables
    in let env' = List.fold_left add_local env' (f.paractuals)
    in let remove v =
      not (List.exists (fun fv -> fv.v_name = v.v_name) formals')
    in let locals' = List.filter remove env'.scope.variables 
    in let body' = List.map (fun f -> trans_stmt env' f) (f.body)
    in let new_f = {
      sf with 
      fformals = formals';
      flocals = locals';
      fbody = body';
      parsed = true;
    }
    in let funcs = new_f :: functions'
    in let scope' = {env.scope with functions = funcs}
    in {env with scope = scope'}
    in let validate_func f =
    let is_return = function
        Sast.Return(e) -> true
      | _ -> false
    in let valid_return = function
        Sast.Return(e) -> if assign_allowed f.t (snd e) then
                            true
                          else
                             raise (Failure(   "Invalid return type " 
                            
                             ))
      | _ -> false
    in let returns = List.filter is_return f.f_body
    in let returns_valid = List.for_all valid_return returns
    in let return_count = List.length returns
    in if (return_count = 0 && f.f_name <> "print" && f.f_name <>"printarray"&& f.f_name <> "printstring" ) then
      raise (Failure( " must return something" ))
    else
      f
   in let make_print t = 
    {
      f_t = Sast.Int;
      f_name = if (t = Sast.Str) then "printstring" else "print";
      f_formals = [{
        v_name = "val";
        v_type = t;
            v_size=1;
        
      }];
      f_locals = [];
      f_body = [];
      parsed = false;
    }
*)
(* Above section modified from asttosast file of chartlan from f2012 *)

(* Run our program *)
(* Input: Ast.Program, Symbol_Table *)
(* Output: Sast.Program *)
(* This is WIP *)

(* type program = identifier list * func_definition list (* global vars, funcs*) *)
(* Check the identifier list and the func_definition list before running the program *)

(*let evalprogram program globalTable = *)