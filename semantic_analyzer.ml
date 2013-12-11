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

(* Taken from Edwards' slides *)
let rec find_variable (scope: symbol_table) name =
    try
	List.find (find (fun (s, _, _, _) -> s = name) scope.variables
    with Not_found ->
    	match scope.parent with
	    Some(parent) -> find_variable parent name
	| _ -> raise Not_found


let rec expr env = function

    (* An integer constant: convert and return Int type *)
    Ast.Litint(v) -> Sast.Litint(v), Sast.Types.Int

  | Ast.Litper(v) -> Sast.Litper(v), Sast.Types.Per

  | Ast.Litstr(v) -> Sast.Litstr(v), Sast.Types.Str

  | Ast.Litbool(v) -> Sast.Litbool(v), Sast.Types.Bool

  | Ast.Noexpr(v) -> Sast.Noexpr(v), Sast.Types.null

  | Ast.Call(v) -> Sast.Call(v), Sast.Types.funct_type

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
	let (_, id_type) = vdecl in ( *get the variable's type *)
	let (_, expr_type ) = expreval in
	Sast.Assign(id, e1), id_type, expr_type

  | Ast.Binop(e1, op, e2) ->
  	let e1 = expr env e1	(* Check left and right children *)
	and e2 = expr env e2 in

	let _, t1 = e1		(* Get the type of each child *)
	and _, t2 = e2 in

	if op <> Ast.Equals && op <> Ast.Notequals then
	    (* Most operators require both left and right to be integer *)
	    (require_integer e1 "Left operand must be integer";
	     require_integer e2 "Right operand must be integer")
	else
	    if not (weak_eq_type t1 g2) then
	    	(* Equality operators just require type to be "close" *)
		error ("Type mismatch in comparison: left is " ^
		    Printer.string_of_sast_type t1 ^ "\" right is \"" ^ 
		    Printer.string_of_sast_type t2 ^ "\""
		    ) loc;
		    
	Sast.Binop (e1, op, e2), Sast.Types.Int

let rec stmt env = function

    Ast.Expr(e) -> Sast.Expr(expr env e)

  | Ast.Block(s1) -> 
   	
	let scope' = { S.parent = Some(env.scope); S.variables = [] }
	and exceptions' =
	    { excep_parent = Some(env.exception_scope); exception = [] }
	in

	(* New environment: same, but with new symbol tables *)
	let env' = { env with scope = scope';
		     exception_scope = exceptions' } in

	let s1 = List.map (fun s -> stmt env' s) s1 in
	scope'.S.variables <-
	    List.rev scope'.S.variables; (* side-effect *)

        Sast.Block(scope', s1)

  | Ast.If(e, s1, s2) ->   (* We need to write check_expr and require_bool *)

	let e = check_expr env e in
	require_bool e "Predicate of if must be boolean";
	
	Sast.If (e, stmt env s1, stmt env s2) (* Check then, else *)
  
  | Ast.Return(e1) -> Sast.Return(expr env e1)

  | Ast.While(e, s1) ->
  	let e = check_expr env e in
	require_bool e "Predicate of while must be boolean";

	Sast.While (e, stmt env s1) (* Check body *)

let rec parent env = function
    Ast.Parent(id) ->	(* Probably we need to fix it *)
    	let vdecl = try
	    find_variable env.scope id
	with Not_found ->
	    raise (Error("undeclared identifier " ^ id))
	in
	let (_, id_type) = vdecl in ( *get the variable's type *)
	Sast.Parent(id), id_type

  | Ast.Noparent(v) -> Sast.Noparent(v), Sast.Types.null



