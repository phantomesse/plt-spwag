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

(* We need to write find_variable and a find_function functions *)
(* This find_variable function is taken from Edwards' slides and will probably be heavily modified *)
(*let rec find_variable (scope: symbol_table) name =
    try
		List.find (fun (s, _, _, _) -> s = name) scope.variables
    with Not_found ->
        match scope.parent with
        Some(parent) -> find_variable parent name
    | _ -> raise Not_found
*)
(* This find_function function is taken from a past project and will probably be heavily modified *)
(*let rec find_function (scope: symbol_table) name =
	let rec getGlobalScope scope = match scope.parent with	(* Functions are defined at the highest level? *)
		| None -> scope
		| Some(parent) -> (getGlobalScope parent)
	in
	try
		List.find (fun {s, _, _, _} -> s = name) (getGlobalScope scope).functions
	with Not_found -> (* This block lists the valid functions; we should look at this carefully *)
		let build_string tmpString nextString = tmpString^" \n"^nextString in
		let func_names_string = List.fold_left build_string("") (List.map (fun {fdt=_; fname=n; formals=_; fbody=_} -> n ) (getGlobalScope scope).functions) in
		let num_funcs = List.length (getGlobalScope scope).functions in
		raise(Failure("Function "^name^" not found in global scope, funcs found were "^(string_of_int num_funcs)^func_names_string))
*)


(*Parent: check if Parent has

List of symbol table
Evalute identifier to be valid

Evalutee func call: Evaluate identifier to be valid (not slide), evaluate actuals are valid expressions,
evaluate mods are statements 

check identifier is in symbol table,
check expression
go into symbol table and assigned something if i

Component of identifier: identifier has to be slide or variable (component or slide) 
expr list are strings
*)

(* Identifier *)
let identify env = 
    Ast.Identifier(v) ->
	let vdecl = try
		find_variable env.scope v (* Locate a variable by name *)
	with Not_found ->
		raise (Failure("undeclared identifier " ^ v))
	in
	let _, t1 = vdecl in
	Sast.Identifier(v), t1

let rec expr env = function

    (* Simple evaluation of primitives *)
    Ast.Litint(v) -> Sast.Litint(v), Sast.Int
  | Ast.Litper(v) -> Sast.Litper(v), Sast.Per
  | Ast.Litstr(v) -> Sast.Litstr(v), Sast.Str
  | Ast.Litbool(v) -> Sast.Litbool(v), Sast.Bool

  | Ast.Binop (expr1, op, expr2) ->  (* evaluate operators *)
	let e1 = expr env expr1 and
		e2 = expr env expr2 in		
 
    let _, t1 = e1 (* Get the type of each child *) 
    and _, t2 = e2 in
	
	match t1, op, t2 with
	  | (Bool), (Ast.And | Ast.Or), (Bool) ->  (* And/or operators *)
			Sast.Binop(e1, op, e2), Sast.Bool (* Boolean *) 
				
      | (Int), (Ast.Lessthan | Ast.Greaterthan), (Int) ->  (* > , < *)
			Sast.Binop(e1, op, e2), Sast.Bool
					
	  | (Int), (Ast.Plus | Ast.Minus | Ast.Times | Ast.Divide), (Int) ->  (* Arithmetic on ints *)
			Sast.Binop(e1, op, e2), Sast.Int   

	  | (Per), (Plus | Minus | Times | Divide), (Per) ->  (* Arithmetic on percents *)
			Sast.Binop(e1, op, e2), Sast.Per   
			
      | _, (Equals | Notequals), _  ->   (* Compare Anything *)
			Sast.Binop(e1, op, e2), Sast.Bool
					
	  | (Str), Plus, (Str | Int) ->  (* String Concatenation *) 
			Sast.Binop(e1, op, e2), Sast.Str

	  | (Str | Int), Plus, (Str | Int) ->  (* String Concatenation *) 
			Sast.Binop(e1, op, e2), Sast.Str		
					
	 (* Otherwise Invalid *)
	  | tA, op, tB -> raise(Failure("Binop "^ (string_of_binop op) ^" has improper operands, found "^ (string_of_type_t tA) ^", "^ (string_of_type_t tB) ^ "\n"))
		
  | Ast.Notop(v) -> (* check if negate = ! and e1 is a boolean *)
	let e1 = expr env v in
	let _, t1 = e1 in (* Get the type of e1 *)
	match t1 with 
	  | Bool -> Sast.Notop(e1), Ast.Bool
	  | _ -> raise(Failure("Invalid operand ("^string_of_type_t t1^") for not operator"))

  | Ast.Assign(lhs, rhs) -> 		
    let e1 = expr env rhs			(* check if valid expression *) 
	and id = identify env lhs in	(* check if identifier *)
    let _, t1 = e1 (* type of rhs *)
	and _, t2 = id (* type of lhs *) in
	if (t1 = t2) then				(* the types need to match? *)
		Sast.Assign(lhs, rhs), t2
	else
		raise(Failure(string_of_type_t t1^" expression does not match identifier "^string_of_type_t t2))
	
  | Ast.Variable(v) -> 				(* If identify function works, this will work *)
	let id = identify env v in
	let _, t1 = id (* type of rhs *) in
	Sast.Variable(v), t1
  
  (* check Variable of identifier: find type of identifier
	Component of identifier: identifier has to be slide or variable (component or slide) *)
	  
  (* Following are problematic: 
  | Component of identifier * expr list (* identifier["child"]["child"] etc. to fetch component *)
  | Call of func_call (* Calling a function, unique in that it can contain statements *)

	Below code is old
  | Ast.Variable(s) -> (* although this is named Variable, can also be the name of a slide/function *)
	  let newVdecl = find_variable scope s in (* is s a valid variable? *)
		Sast.Variable(newVdecl.vname), newVdecl.vdt   (* look for it as a variable with find_variable *)
  
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
    | Ast.Block(s1) ->  let scope' = { S.parent = Some(env.scope); S.variables = [] }
                        and exceptions' = { excep_parent = Some(env.exception_scope); exceptions = [] }
                        in
                            (* New environment: same, but with new symbol tables *)
                            let env' = { env with scope = scope';
                                         exception_scope = exceptions' } in
                            let s1 = List.map (fun s -> stmt env' s) s1 in
                                scope'.S.variables <- List.rev scope'.S.variables; (* side-effect *)
                                Sast.Block(scope', s1)
    | Ast.If(e, s1, s2) ->  
        let e1 = expr env e in
		let _, t1 = e1 in 	(* Get the type of e1 *)
		if (t1 = Bool) then
			Sast.If (e, stmt env s1, stmt env s2) (* Check then, else *)
		else raise(Failure(string_of_type_t t1^"type must be bool"))

    | Ast.While(e, s1) ->   
		let e = expr env e in
		let _, t1 = e1 in 	(* Get the type of e1 *)
		if (t1 = Bool) then
			Sast.While (e, stmt env s1) (* Check body *)
		else raise(Failure(string_of_type_t t1^"type must be bool"))
		
	| Ast.Declaration(v) ->
		let id = identify env v in
		let _, t1 = id in
		if (find_variable env id = true) then (* If declaration exists, don't allow duplicate *)
			raise(Failure("Existing variable declaration for "^v))
		(* else we have to add the variable declaration to the symbol table; I'll write this later *)
		else
			Sast.Declaration(v), t1;
                
	| Ast.Decassign(v, e) ->
		let id = identify env v 
		and e = expr env e in
		let _, t1 = v in
		let _, t2 = e in
		if (find_variable env id = true) then (* If declaration exists, don't allow duplicate *)
			raise(Failure("Existing variable declaration for "^v))		
		if (t1 = t2) then	(* variable types need to match *)
							(* we have to add the variable declaration to the symbol table; I'll write this later *)
			Sast.Decassign(v, e), t1 (* Declaring a variable and then assigning it something*)
		else	
			raise(Failure(string_of_type_t t1^" expression does not match identifier "^string_of_type_t t2))	

let rec parent env = function
    | Ast.Parent(id) -> (* This code is bugged *)
		let vdecl = try find_variable env.scope id
            with Not_found ->
                raise (Error("undeclared identifier " ^ id))
            in
			let (_, id_type) = vdecl in (* get the variable's type *)
                Sast.Parent(id), id_type
    | Ast.Noparent(v) -> Sast.Noparent(v), Sast.null

(* Run our program *)
(* Input: Ast.Program, Symbol_Table *)
(* Output: Sast.Program *)
(* This is WIP *)

let evalprogram program globalTable = 
    let _ = check_order program in
    let endScope, _ = List.fold_left process(globalTable, []) program in

    endScope
