open Sast
open Ast

let sast_to_ast_id = function Ast.Identifier(s) -> Sast.Identifier(s)

let inject (vars, funcs) =
	
	let convert_expr (expr_in:Ast.expr) = 
		(Sast.Litnull, Sast.Int)
	
	and convert_stmt (stmt_in:Ast.stmt) = 
		Sast.Block([])
	in
	let convert_func (fdef:Ast.func_definition) = 
		{Sast.t=fdef.t;
		name= sast_to_ast_id fdef.name;
		formals = List.rev (List.fold_left (fun l (Ast.Identifier(s)) -> Sast.Identifier(s)::l ) [] fdef.formals);
		inheritance = (match fdef.inheritance with None -> None | Some(Ast.Identifier(s)) -> Some(Sast.Identifier(s))); 
		paractuals = List.rev (List.fold_left (fun l e -> (convert_expr e)::l ) [] fdef.paractuals);
		body = List.rev (List.fold_left (fun l s -> (convert_stmt s)::l ) [] fdef.body);}
	in

	let sast_vars = List.rev (List.fold_left (fun l (Ast.Identifier(s)) -> Sast.Identifier(s)::l ) [] vars) in
	let sast_funcs = List.rev (List.fold_left (fun l f -> (convert_func f)::l ) [] funcs) in
	(sast_vars, sast_funcs)


