%{ open Ast %}

%token LPAREN RPAREN PLUS MINUS TIMES DIVIDE ASSIGN EOF EQUALS NOTEQUALS LESSTHAN GREATERTHAN NOT OR AND COMMA NEWLINE
%token ATTR COMP FUNC DEF ELSE END IF IMPORT ISA NULL RETURN SLIDE VAR WHILE LBRACE RBRACE
%token <int> LITERAL PERCENT
%token <string> ID
%token <bool> TRUE FALSE

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
    | /* nothing */             { [], [] }
    | program NEWLINE func_decl { fst $1, ($3 :: snd $1) }

func_decl:
    | DEF SLIDE ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
      {{
        t = Slide;
        name = Identifier($3);
        formals = $5;
        inheritance = Noparent(Null);
        body = List.rev $8
      }}
    | DEF COMP ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
      {{
        t = Comp;
        name = Identifier($3);
        formals = $5;
        inheritance = Noparent(Null);
        body = List.rev $8
      }}
    | DEF ATTR ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
      {{
        t = Attr;
        name = Identifier($3);
        formals = $5;
        inheritance = Noparent(Null);
        body = List.rev $8
      }}
    | DEF FUNC ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
      {{
        t = Func;
        name = Identifier($3);
        formals = $5;
        inheritance = Noparent(Null);
        body = List.rev $8
      }}

formals_opt:
    | /* nothing */         { [] }
    | formal_list           { List.rev $1 }

formal_list:
    | ID                    { [Identifier($1)] }
    | formal_list COMMA ID  { Identifier($3) :: $1 }

stmt_list:
    | /* nothing */         { [] }
    | stmt_list stmt        { $2 :: $1 }

stmt:
    | expr NEWLINE                         { Expr($1) }
    | RETURN expr                          { Return($2) }
    | LBRACE stmt_list RBRACE              { Block(List.rev $2) }
    | IF expr stmt %prec NOELSE            { If($2, $3, Block([])) }
    | IF expr stmt ELSE stmt               { If($2, $3, $5) }
    | WHILE LPAREN expr RPAREN stmt        { While($3, $5) }

expr:
    | expr PLUS expr       { Binop($1, Plus, $3) }
    | expr MINUS expr      { Binop($1, Minus, $3) }
    | expr TIMES expr      { Binop($1, Times, $3) }
    | expr DIVIDE expr     { Binop($1, Divide, $3) }
    | VAR ID ASSIGN expr   { Assign(Identifier($2), $4) }
    | ID ASSIGN expr       { Assign(Identifier($1), $3) }
    | LITERAL              { Litint($1) }
    | PERCENT              { Litper($1) }
    | ID                   { Litstr($1) }
    | TRUE                 { Litbool($1) }
    | FALSE                { Litbool($1) }
