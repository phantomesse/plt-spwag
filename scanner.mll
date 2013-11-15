{ open Parser }

rule token =
    parse [' ' '\r']                                 { token lexbuf }
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
    | ','                                            { COMMA }
    | '\t'                                           { TAB }
    | '\n'                                           { NEWLINE }
    | "attr"                                         { ATTR }
    | "comp"                                         { COMP }
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
    | eof                                            { EOF }
    | _ as char                                      { raise (Failure("illegal character " ^ Char.escaped char)) }
and multi_line_comment = parse
    | "##" { token lexbuf } (* End-of-comment *)
    | _ { multi_line_comment lexbuf } (* Eat everything else *)
and single_line_comment = parse
    | '\n' { token lexbuf } (* End-of-single-line-comment *)
    | _ { single_line_comment lexbuf } (* Eat everything else *)
and text_string = parse
    | '"' { token lexbuf } (* End of string *)
    | _ { text_string lexbuf } (* Eat everything else *)
