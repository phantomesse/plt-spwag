{ open Parser }

(* Add interpretation for string literals *)

rule token =
    parse [' ' '\r'  '\t' ]                          { token lexbuf }
    | "##"                                           { multi_line_comment lexbuf }
    | '#'                                            { single_line_comment lexbuf }
    | '"'                                            { text_string lexbuf }
    | '+'                                            { PLUS }
    | '-'                                            { MINUS }
    | '*'                                            { TIMES }
    | '/'                                            { DIVIDE }
    | '='                                            { ASSIGN }
    | "=="                                           { EQUALS }
    | "!="                                           { NOTEQUALS }
    | '<'                                            { LESSTHAN }
    | '>'                                            { GREATERTHAN }
    | '!'                                            { NOT }
    | '|'                                            { OR }
    | '&'                                            { AND }
    | '('                                            { LPAREN }
    | ')'                                            { RPAREN }
    | '{'                                            { LBRACE }
    | '}'                                            { RBRACE }
    | '['											 { LBRACK }
	| ']'											 { RBRACK }
	| ','                                            { COMMA }
    | '\n'                                           { NEWLINE }
    | "attr"                                         { ATTR }
    | "comp"                                         { COMP }
    | "func"                                         { FUNC }
    | "define"                                       { DEF }
    | "else"                                         { ELSE }
    | "end"                                          { END }
    | "false"                                        { FALSE }
    | "if"                                           { IF }
    | "import"                                       { IMPORT }
    | "isa"                                          { ISA }
    | "null"                                         { NULL }
    | "return"                                       { RETURN }
    | "slide"                                        { SLIDE }
    | "true"                                         { TRUE }
    | "var"                                          { VAR }
    | "while"                                        { WHILE }
	| ['a'-'z']['a'-'z' '0'-'9' '-']*  as idstr      { ID(idstr) }
    | ['0'-'9']+ as lit                              { LITERAL(int_of_string lit) }
    | ['0'-'9']+'%' as lit                           { PERCENT(int_of_string (String.sub lit 0 ((String.length lit)-1))) }
    | eof                                            { EOF }
    | _ as char                                      { raise (Failure("illegal character " ^ Char.escaped char)) }
and multi_line_comment = parse
      "##" { token lexbuf } (* End-of-comment *)
    | _ { multi_line_comment lexbuf } (* Eat everything else *)
and single_line_comment = parse
      '\n' { token lexbuf } (* End-of-single-line-comment *)
    | _ { single_line_comment lexbuf } (* Eat everything else *)
and text_string = parse
      '"' { token lexbuf } (* End of string literal *)
    | [^ '"']* as str { STRING(str) } (* zero or more characters that is not end of string *)
