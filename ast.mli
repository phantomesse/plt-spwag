(* Null type *)
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
    name : identifier; (* Name of the function *)
    actuals : expr list; (* Evaluated actual parameters *)
    mods : stmt list; (* Additional statements *)
}  

(* Expressions *)
and expr = 
    | Binop of expr * operator * expr (* a + b *)
    | Litint of int (* 42 *)
    | Litper of int (* 42% *)
    | Litstr of string (* “foo” *)
    | Litbool of bool (* true *)
    | Assign of identifier * expr (* foo - 42 *) (*we should examine this*)
    | Call of string * expr list
    | Noexpr of null (* for double newlines *)

(* Calls and executes function. Follows a control flow detailed in the LRM/Revisions doc *)
and stmt = (* Statements ; WIP *)
    | Block of stmt list (* { ... } *)
    | Expr of expr (* foo = bar + 3; *)
    | Return of expr (* return 42; *)
    | If of expr * stmt * stmt (* if (foo == 42) stmt1 else stmt2 end *)
    | While of expr * stmt (* while (i<10) \n  i = i + 1 \n end \n *)

(* A function alters the control flow. This is how you define a function:
     define type identifier (parameters)
       # variable declaration (subcomponents, subattributes defined maybe?)
# body
     end
*)

(*A variable has a name and value *) 
type var_declaration = {
    name : identifier;
    value : expr;
}

type func_definition = { (* Handles declarations of functions, components, attributes, slides *)
    t: func_type; (* e.g. slide, component, attribute, func *)
    name : identifier; (* Name of the function *)
    formals : identifier list; (* Name of the formal parameters *)
    inheritance : parent; (* Name of any parent components, ie box, or null *)
    body : stmt list; (* Conditional, Return Statements, Function Declarations/Calls, etc. *)
}

(* The program itself *)
type program = identifier list * func_definition list (* global vars, funcs*)
