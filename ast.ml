(* Authors: Richard Chiou, Yunhe (John) Wang, and Aditya Majumdar *)

(* Identifiers *)
type identifier = Identifier of string
		
(* Operators *)
type operator = Plus | Minus | Times | Divide | Equals | Notequals | Lessthan | Greaterthan | Or | And

(* Function types *)
type func_type = Slide | Comp | Attr | Func

(* Handles calls of functions and components *)
type func_call = { 
    cname : identifier; (* Name of the function *)
    actuals : expr list; (* Evaluated actual parameters *)
    mods : stmt; (* Additional statements, which could be a block *)
}  

(* Expressions *)
and expr =
      Binop of expr * operator * expr (* a + b *)
    | Notop of expr (* !a only applies to booleans *)
    | Litint of int (* 42 *)
    | Litper of int (* 42% *)
    | Litstr of string (* “foo” *)
    | Litbool of bool (* true *)
    | Assign of identifier * expr (* foo - 42 *) 
    | Variable of identifier (* although this is named Variable, can also be the name of a slide/function *)
    | Component of identifier * expr list (* identifier["child"]["child"] etc. to fetch component *)
    | Call of func_call (* Calling a function, unique in that it can contain statements*)

(* Calls and executes function. Follows a control flow detailed in the LRM/Revisions doc *)
and stmt = (* Statements ; WIP *)
      Block of stmt list (* { ... } *)
    | Expr of expr (* foo = bar + 3; *)
    | Return of expr (* return 42; *)
    | If of expr * stmt * stmt (* if (foo == 42) stmt1 else stmt2 end *)
    | While of expr * stmt (* while (i<10) \n  i = i + 1 \n end \n *)
    | Declaration of identifier (* Declaring a variable *)
    | Decassign of identifier * expr (* Declaring a variable and then assigning it something*)

(* Function definition that makes up the basic structure of the program*)
type func_definition = { (* Handles declarations of functions, components, attributes, slides *)
    t: func_type; (* e.g. slide, component, attribute, func *)
    name : identifier; (* Name of the function *)
    formals : identifier list; (* Name of the formal parameters *)
    inheritance : identifier option; (* Name of any parent components, ie box, or null *)
    paractuals: expr list; (* This represents the actuals passed to the parent *)
    body : stmt list; (* Conditional, Return Statements, Function Declarations/Calls, etc. *)
}

(* The program itself *)
type program = identifier list * func_definition list (* global vars, funcs*)

(* The following are needed to output the ast for testing *)
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
      Notop(e) -> "!(" ^ (string_of_expr e) ^ ")"
    | Binop(e1, o, e2) ->
        "(" ^ string_of_expr e1 ^ " " ^
        (match o with
      Plus -> "+" | Minus -> "-" | Times -> "*" | Divide -> "/"
        | Equals -> "==" | Notequals -> "!="
        | Lessthan -> "<" | Greaterthan -> ">"
        | Or -> "||" | And -> "&&" ) ^ " " ^
        string_of_expr e2 ^ ")"
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
      Block(stmts) -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
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
    " isa " ^ (string_of_identifier (match i with Some(id) -> id | None -> Identifier("ERROR!!"))) ^ 
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
