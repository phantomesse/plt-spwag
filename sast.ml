(* Null type *)
type t = Int | Per | Str | Bool | Null | Func_type

type null = Null

(* Identifiers *)
type identifier = 
    Identifier of string

(* Operators *)
type operator = Plus | Minus | Times | Divide | Assign | Equals | Notequals | Lessthan | Greaterthan

(* Function types *)
type func_type = Slide | Comp | Attr | Func

(* For inheritance of components *)
type parent = 
    Parent of identifier 
    | Noparent of null

(* Handles calls of functions, components, *)
type func_call = { 
    cname : identifier; (* Name of the function *)
    actuals : expr list; (* Evaluated actual parameters *)
    mods : stmt (* Additional statements *)
}  

(* Expressions *)
and expr_detail = 
    | Binop of expr * operator * expr(* a + b *)
    | Litint of int (* 42 *)
    | Litper of int (* 42% *)
    | Litstr of string(* “foo” *)
    | Litbool of bool (* true *)
    | Assign of identifier * expr (* Assign of Identifier : foo - 42 *) 
    | Variable of identifier   (* Variable of Identifier *)
    | Component of identifier * expr list (* Identifier * expr list *)
    | Call of func_call
    | Noexpr of null (* for double newlines : do we even need this? *)

and expr = expr_detail * t

(* Calls and executes function. Follows a control flow detailed in the LRM/Revisions doc *)
and stmt = (* Statements ; WIP *)
    | Block of stmt list (* { ... } *)
    | Expr of expr (* foo = bar + 3; *)
    | Return of expr (* return 42; *)
    | If of expr * stmt * stmt (* if (foo == 42) stmt1 else stmt2 end *)
    | While of expr * stmt (* while (i<10) \n  i = i + 1 \n end \n *)
    | Declaration of identifier (*Declaration of identifier *)
    | Decassign of identifier * expr (* Declaring a variable and assigning it to something else  *)

(* A function alters the control flow. This is how you define a function:
     define type identifier (parameters)
        # variable declaration (subcomponents, subattributes defined maybe?)
	# body
     end
*)

(*A variable has a name and value *) 
type var_declaration = {
    name : identifier; (* identifier *)
    value : expr;
}

type func_definition = { (* Handles declarations of functions, components, attributes, slides *)
    t: func_type; (* e.g. slide, component, attribute, func *)
    name : identifier; (* Name of the function *)
    formals : identifier list; (* Name of the formal parameters *)
    inheritance : parent; (* Name of any parent components, ie box, or null *)
    paractuals : expr list; (* This represents the actuals passed to the parent *)
    body : stmt list; (* Conditional, Return Statements, Function Declarations/Calls, etc. *)
}

(* The program itself *)
type program = identifier list * func_definition list (* global vars, funcs*)

(* ----------------------------------------------------------------------  *)

(* The following are needed to output the SAST for testing *)
let string_of_identifier = function
    Identifier(s) -> s

let rec string_of_call call = 
    (string_of_identifier call.cname) ^ "(" ^
    String.concat ", " (List.map string_of_expr call.actuals) ^ ")"
    ^ (match call.mods with 
    Block([]) -> ""
    | Block(stmts) -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}" 
    | something -> "ERROR!!!!!!" )
and string_of_expr = function
    Binop(e1, o, e2) ->
        string_of_expr e1 ^ " " ^
        (match o with
      Plus -> "+" | Minus -> "-" | Times -> "*" | Divide -> "/"
        | Equals -> "==" | Notequals -> "!="
        | Lessthan -> "<" | Greaterthan -> ">") ^ " " ^
        string_of_expr e2
    | Litint(l) -> string_of_int l
    | Litper(l) -> (string_of_int l) ^ "%"
    | Litstr(s) -> "\"" ^ s ^ "\""
    | Litbool(b) -> if b then "true" else "false"
    | Assign(v, e) -> (string_of_identifier v) ^ " = " ^ string_of_expr e
    | Variable(v) -> (string_of_identifier v) 
    | Component(v, e) ->  (string_of_identifier v) ^ 
        String.concat "" (List.map (fun ex -> "[" ^ (string_of_expr ex) ^ "]") e)
    | Call(f) -> string_of_call f
and string_of_stmt = function
    | Block(stmts) ->
        "{" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
    | Expr(expr) -> string_of_expr expr ^ "\n";
    | Return(expr) -> "return " ^ string_of_expr expr ^ "\n";
    | If(e, s, Block([])) -> "if " ^ string_of_expr e ^ "\n" ^ string_of_stmt s
    | If(e, s1, s2) ->  "if " ^ string_of_expr e ^ "\n" ^
        string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
    | While(e, s) -> "while " ^ string_of_expr e ^ "\n " ^ string_of_stmt s
    | Declaration(i) -> "var " ^ (string_of_identifier i) ^ "\n"
    | Decassign(i, e) -> "var " ^ (string_of_identifier i) ^ "= " ^ (string_of_expr e) ^ "\n"

let string_of_vdecl = function
    Identifier(s) -> "var " ^ s ^ "\n"

let string_of_function_type = function
    Slide -> "define slide" 
    | Comp -> "define comp" 
    | Attr -> "define attr"
    | Func -> "define func"

let string_of_inheritance i p = 
    " isa " ^ (string_of_identifier (match i with Parent(id) -> id | Noparent(n) -> Identifier("ERROR!!"))) ^ 
    "(" ^ String.concat ", " (List.map string_of_expr p) ^ ")" 

let string_of_fdecl fdecl =
    (string_of_function_type fdecl.t) ^ " " ^
    (string_of_identifier fdecl.name) ^ 
    "(" ^ String.concat ", " (List.map string_of_identifier fdecl.formals) ^ ")" ^ 
    (match fdecl.t with 
        Comp -> (string_of_inheritance fdecl.inheritance fdecl.paractuals)
        | _ -> ""
    ) ^
    "\n{\n" ^ 
    String.concat "" (List.map string_of_stmt fdecl.body) ^
    "}\n"

let string_of_program (vars, funcs) =
    String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
    String.concat "\n" (List.map string_of_fdecl funcs)