(* Author: Yunhe (John) Wang *)
(* Useful for testing etc. *)

open Sast
open Ast

let sast_to_ast_id = function Ast.Identifier(s) -> Sast.Identifier(s)

let inject (vars, funcs) =
	
	let rec convert_expr = function
		Ast.Binop(e1, o, e2) -> (Sast.Binop((convert_expr e1), o, (convert_expr e2)), Sast.Null)
	    | Ast.Notop(e) -> (Sast.Notop(convert_expr e), Sast.Null)
	    | Ast.Litint(i) -> (Sast.Litint(i), Sast.Null)
	    | Ast.Litper(i) -> (Sast.Litper(i), Sast.Null)
	    | Ast.Litstr(s) -> (Sast.Litstr(s), Sast.Null)
	    | Ast.Litbool(b) -> (Sast.Litbool(b), Sast.Null)
		| Ast.Litnull -> (Sast.Litnull, Sast.Null)
	    | Ast.Assign(Identifier(i), e) -> (Sast.Assign(Sast.Identifier(i), convert_expr e), Sast.Null)
	    | Ast.Variable(Identifier(v)) -> (Sast.Variable(Sast.Identifier(v)), Sast.Null)
	    | Ast.Component(Identifier(v), elist) -> (Sast.Component(Sast.Identifier(v), 	
			List.rev (List.fold_left (fun l e -> (convert_expr e)::l ) [] elist)), Sast.Null)
	    | Ast.Call(c) ->(Sast.Call(
		{Sast.cname= sast_to_ast_id c.cname; 
		actuals= List.rev (List.fold_left (fun l e -> (convert_expr e)::l ) [] c.actuals);
		mods= convert_stmt c.mods;}), Sast.Null)
	
	and convert_stmt = function
		Ast.Block(stmts) -> Sast.Block(List.rev (List.fold_left (fun l s -> (convert_stmt s)::l ) [] stmts))
    	| Ast.Expr(e) -> Sast.Expr(convert_expr e)
    	| Ast.Return(e) -> Sast.Return(convert_expr e)
    	| Ast.If(e,s1,s2) -> Sast.If(convert_expr e, convert_stmt s1, convert_stmt s2)
    	| Ast.While(e,s) -> Sast.While(convert_expr e, convert_stmt s)
    	| Ast.Declaration(Identifier(s)) -> Sast.Declaration(Sast.Identifier(s))
    	| Ast.Decassign(Identifier(s), e) -> Sast.Decassign(Sast.Identifier(s), convert_expr e)
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


