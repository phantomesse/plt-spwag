type operator = Plus | Minus | Times | Divide | Assign | Equals | Notequals | Lessthan | Greaterthan

type func_type = Slide | Comp | Attr | Func

type null = Null

type parent = (* For inheritance of components *)
    Parent of identifier 
    | Noparent of null

type identifier = (* Identifiers *)
    Identifier of string

type expr = (* Expressions *)
    | Binop of expr * operator * expr (* a + b *)
    | Litint of int (* 42 *)
    | Litper of int (* 42% *)
    | Litstr of string (* “foo” *)
    | Litbool of boolean (* true *)
    | Assign of identifier * expr (* foo - 42 *) (*we should examine this*)
    | Call of func_call
    | Noexpr of null (* for double newlines *)
    
(* We need to figure out how to incorporate percents and null into the ast*)

type stmt = (* Statements ; WIP *)
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

type func_definition = { (* Handles declarations of functions, components, attributes, slides *)
    type: func_type; (* e.g. slide, component, attribute, func *)
    name : identifier; (* Name of the function *)
    formals : identifier list; (* Name of the formal parameters *)
    inheritance : parent; (* Name of any parent components, ie box, or null *)
    locals : identifier list; (* Locally Defined Variables *)
    body : stmt list; (* Conditional, Return Statements, Function Declarations/Calls, etc. *)
}


(* Calls and executes function. Follows a control flow detailed in the LRM/Revisions doc *)
type func_call = { (* Handles calls of functions, components, *)
    name : identifier; (* Name of the function *)
    actuals : expr list; (* Evaluated actual parameters *)
    mods : stmt list; (* Additional statements *)
}

type program = identifier list * func_decl list (* global vars, funcs*)