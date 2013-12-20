/* Yunhe (John) Wang, Lauren Zou, Aftab Khan */

%{
    open Ast
    open Linecounter
    let parse_error msg = Printf.eprintf "%s at around line %d \n" msg !linecount
%}

%token LPAREN RPAREN PLUS MINUS TIMES DIVIDE ASSIGN EOF EQUALS NOTEQUALS LESSTHAN GREATERTHAN NOT OR AND COMMA NEWLINE
%token ATTR COMP FUNC DEF ELSE END IF IMPORT ISA NULL RETURN SLIDE VAR WHILE LBRACE RBRACE LBRACK RBRACK
%token <int> LITERAL PERCENT
%token <string> ID STRING
%token TRUE FALSE

%nonassoc NOELSE
%nonassoc ELSE
%left COMMA
%right ASSIGN
%left OR
%left AND
%left EQUALS NOTEQUALS
%left LESSTHAN GREATERTHAN
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT
%left LBRACK RBRACK
%left LPAREN RPAREN

%start program
%type <Ast.program> program

%%

program: /* global vars, functions */
      /* nothing */            { [], [] }
    | program NEWLINE          { $1 }
    | program VAR ID NEWLINE   {(Identifier($3) :: fst $1), snd $1 }
    | program func_decl        { fst $1, ($2 :: snd $1) }

func_decl:
      DEF SLIDE ID LPAREN RPAREN NEWLINE LBRACE stmt_list RBRACE NEWLINE
      {{
        t = Slide;
        name = Identifier($3);
        formals = [];
        inheritance = None;
        paractuals = [];
        body = List.rev $8
      }}
    | DEF COMP ID LPAREN formals_opt RPAREN ISA ID LPAREN actuals_opt RPAREN
      NEWLINE LBRACE stmt_list RBRACE NEWLINE
      {{
        t = Comp;
        name = Identifier($3);
        formals = $5;
        inheritance = Some(Identifier($8));
        paractuals = $10;
        body = List.rev $14
      }}
    | DEF ATTR ID LPAREN formals_opt RPAREN NEWLINE LBRACE stmt_list RBRACE NEWLINE
      {{
        t = Attr;
        name = Identifier($3);
        formals = $5;
        inheritance = None;
        paractuals = [];
        body = List.rev $9
      }}
    | DEF FUNC ID LPAREN formals_opt RPAREN NEWLINE LBRACE stmt_list RBRACE NEWLINE
      {{
        t = Func;
        name = Identifier($3);
        formals = $5;
        inheritance = None;
        paractuals = [];
        body = List.rev $9
      }}

formals_opt:
      /* nothing */                 { [] }
    | formal_list                   { List.rev $1 }

formal_list:
      ID                            { [Identifier($1)] }
    | formal_list COMMA ID          { Identifier($3) :: $1 }

actuals_opt:
      /* nothing */                 { [] }
    | actuals_list                  { List.rev $1 }

actuals_list:
      expr                          { [$1] }
    | actuals_list COMMA expr       { $3 :: $1 }

ids_list:
      LBRACK expr RBRACK            { [$2] }
    | ids_list LBRACK expr RBRACK   { $3 :: $1 }

mods_opt:
      /* nothing */                 { Block([]) }
    | LBRACE stmt_list RBRACE       { Block(List.rev $2) }

stmt_list:
      NEWLINE                       { [] }
    | stmt_list NEWLINE             { $1 }
    | stmt_list stmt                { $2 :: $1 }

stmt:
      expr NEWLINE                                  { Expr($1) }
    | RETURN expr NEWLINE                           { Return($2) }
    | LBRACE stmt_list RBRACE NEWLINE               { Block(List.rev $2) }
    | IF expr NEWLINE stmt %prec NOELSE             { If($2, $4, Block([])) }
    | IF expr NEWLINE stmt ELSE NEWLINE stmt        { If($2, $4, $7) }
    | WHILE expr NEWLINE stmt                       { While($2, $4) }
    | VAR ID NEWLINE                                { Declaration(Identifier($2))}
    | VAR ID ASSIGN expr NEWLINE                    { Decassign(Identifier($2), $4) }
    
expr:
      NOT expr                       { Notop($2) }
    | expr PLUS expr         { Binop($1, Plus, $3) }
    | expr MINUS expr        { Binop($1, Minus, $3) }
    | expr TIMES expr        { Binop($1, Times, $3) }
    | expr DIVIDE expr       { Binop($1, Divide, $3) }
    | expr EQUALS expr       { Binop($1, Equals, $3) }
    | expr NOTEQUALS expr    { Binop($1, Notequals, $3) }
    | expr LESSTHAN expr     { Binop($1, Lessthan, $3) }
    | expr GREATERTHAN expr  { Binop($1, Greaterthan, $3) }
    | expr OR expr           { Binop($1, Or, $3) }
    | expr AND expr          { Binop($1, And, $3) }
    | ID ASSIGN expr         { Assign(Identifier($1), $3) }
    | LITERAL                { Litint($1) }
    | STRING                 { Litstr($1) }
    | PERCENT                { Litper($1) }
    | ID                     { Variable(Identifier($1)) }
    | TRUE                   { Litbool(true) }
    | FALSE                  { Litbool(false) }
    | NULL                     { Litnull }
    | ID ids_list            { Component(Identifier($1), List.rev $2) }
    | ID LPAREN actuals_opt RPAREN mods_opt 
      {Call({
        cname = Identifier($1);
        actuals = $3;
        mods = $5;
      })}
    | LPAREN expr RPAREN   { $2 }
