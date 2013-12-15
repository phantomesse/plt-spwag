%{ 
open Ast 
open Linecounter
let parse_error msg = Printf.eprintf "%s at line %d \n" msg !linecount 
%}

%token LPAREN RPAREN PLUS MINUS TIMES DIVIDE ASSIGN EOF EQUALS NOTEQUALS LESSTHAN GREATERTHAN NOT OR AND COMMA NEWLINE
%token ATTR COMP FUNC DEF ELSE END IF IMPORT ISA NULL RETURN SLIDE VAR WHILE LBRACE RBRACE LBRACK RBRACK
%token <int> LITERAL PERCENT
%token <string> ID STRING
%token TRUE FALSE

%nonassoc NOELSE
%nonassoc IF ELSE
%right ASSIGN
%left EQUALS NOTEQUALS
%left COMMA
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

program: /* global vars, functions */
    /* nothing */              { [], [] }
    | program NEWLINE          { $1 }
    | program VAR ID NEWLINE   {(Identifier($3) :: fst $1), snd $1 }
    | program func_decl        { fst $1, ($2 :: snd $1) }

func_decl:
    | DEF SLIDE ID LPAREN formals_opt RPAREN NEWLINE LBRACE stmt_list RBRACE NEWLINE
      {{
        t = Slide;
        name = Identifier($3);
        formals = $5;
        inheritance = Noparent(Null);
        paractuals = [];
        body = List.rev $9
      }}
    | DEF COMP ID LPAREN formals_opt RPAREN ISA ID LPAREN actuals_opt RPAREN
      NEWLINE LBRACE stmt_list RBRACE NEWLINE
      {{
        t = Comp;
        name = Identifier($3);
        formals = $5;
        inheritance = Parent(Identifier($8));
        paractuals = $10;
        body = List.rev $14
      }}
    | DEF ATTR ID LPAREN formals_opt RPAREN NEWLINE LBRACE stmt_list RBRACE NEWLINE
      {{
        t = Attr;
        name = Identifier($3);
        formals = $5;
        inheritance = Noparent(Null);
        paractuals = [];
        body = List.rev $9
      }}
    | DEF FUNC ID LPAREN formals_opt RPAREN NEWLINE LBRACE stmt_list RBRACE NEWLINE
      {{
        t = Func;
        name = Identifier($3);
        formals = $5;
        inheritance = Noparent(Null);
        paractuals = [];
        body = List.rev $9
      }}

formals_opt:
     /* nothing */                  { [] }
    | formal_list                   { List.rev $1 }

formal_list:
      ID                            { [Identifier($1)] }
    | formal_list COMMA ID          { Identifier($3) :: $1 }

actuals_opt:
     /* nothing */                  { [] }
    | actuals_list                  { List.rev $1 }

actuals_list:
      expr                          { [$1] }
    | actuals_list COMMA expr       { $3 :: $1 }

ids_list:
      LBRACK expr RBRACK            { [$2] }
    | ids_list LBRACK expr RBRACK   { $3 :: $1 }

mods_opt:
    /* nothing */                   { Block([]) }
    | LBRACE stmt_list RBRACE       { Block(List.rev $2) }

stmt_list:
    | NEWLINE                       { [] }
    | stmt_list NEWLINE             { $1 }
    | stmt_list stmt                { $2 :: $1 }

stmt:
    | expr NEWLINE                                  { Expr($1) }
    | RETURN expr NEWLINE                           { Return($2) }
    | LBRACE stmt_list RBRACE NEWLINE               { Block(List.rev $2) }
    | IF expr NEWLINE stmt %prec NOELSE             { If($2, $4, Block([])) }
    | IF expr NEWLINE stmt ELSE stmt                { If($2, $4, $6) }
    | WHILE expr NEWLINE stmt                       { While($2, $4) }
    | VAR ID NEWLINE                                { Declaration(Identifier($2))}
    | VAR ID ASSIGN expr NEWLINE                    { Decassign(Identifier($2), $4) }
    
expr:
    | expr PLUS expr       { Binop($1, Plus, $3) }
    | expr MINUS expr      { Binop($1, Minus, $3) }
    | expr TIMES expr      { Binop($1, Times, $3) }
    | expr DIVIDE expr     { Binop($1, Divide, $3) }
    | ID ASSIGN expr       { Assign(Identifier($1), $3) }
    | LITERAL              { Litint($1) }
    | STRING               { Litstr($1) }
    | PERCENT              { Litper($1) }
    | ID                   { Variable(Identifier($1)) }
    | TRUE                 { Litbool(true) }
    | FALSE                { Litbool(false) }
    | ID ids_list          { Component(Identifier($1), List.rev $2) }
    | ID LPAREN actuals_opt RPAREN mods_opt 
      {Call({
        cname = Identifier($1);
        actuals = $3;
        mods = $5;
      })}

