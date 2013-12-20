(* Author: Lauren Zou *)

open Ast
open Sast
open Ir
open Ir.Element
open Ir.Slide
module StringMap = Map.Make(String)

let string_of_property property value =
    if String.length value > 0 then
        "            " ^ property ^ ": " ^ value ^ ";\n"
    else
        ""
;;

let string_of_css style classname =
    "        #" ^ classname ^ " {\n" ^

    (*string_of_property "display", string_of_bool elem.display ^*)
    
    string_of_property "left" style.position_x ^
    string_of_property "top" style.position_y ^

    string_of_property "margin-top" style.margin_top ^
    string_of_property "margin-bottom" style.margin_bottom ^
    string_of_property "margin-left" style.margin_left ^
    string_of_property "margin-right" style.margin_right ^

    string_of_property "padding-top" style.padding_top ^
    string_of_property "padding-bottom" style.padding_bottom ^
    string_of_property "padding-left" style.padding_left ^
    string_of_property "padding-right" style.padding_right ^

    string_of_property "color" style.text_color ^
    string_of_property "background-color" style.background_color ^

    string_of_property "font-family" style.font ^
    string_of_property "font-size" style.font_size ^
    (* Need to handle font decoration with italics, bold, and underline *)

    string_of_property "border-width" style.border ^
    string_of_property "border-color" style.border_color ^

    string_of_property "width" style.width ^
    string_of_property "height" style.height ^

    "        }"
;;

let string_of_slide_css style classname =
    "        #" ^ classname ^ " {\n" ^

    (*string_of_property "display", string_of_bool elem.display ^*)
    
    string_of_property "padding-top" style.padding_top ^
    string_of_property "padding-bottom" style.padding_bottom ^
    string_of_property "padding-left" style.padding_left ^
    string_of_property "padding-right" style.padding_right ^

    string_of_property "color" style.text_color ^
    string_of_property "background-color" style.background_color ^

    string_of_property "font-family" style.font ^
    string_of_property "font-size" style.font_size ^
    (* Need to handle font decoration with italics, bold, and underline *)

    string_of_property "border-width" style.border ^
    string_of_property "border-color" style.border_color ^

    "        }"
;;

let rec get_css_from_element (element, element_id) =
    string_of_css element.Element.style element_id ^

    (* Get the CSS from the elements of this element *)
    String.concat "" (List.map get_css_from_element (StringMap.fold (fun id element l -> (element, element_id ^ "-" ^ id)::l) element.Element.elements []))
;;

let get_css_from_slide slide =
    (* Get the CSS from the slide *)
    string_of_slide_css slide.Slide.style slide.Slide.id ^ "\n\n" ^

    (* Get the CSS from the elements of this slide *)
    String.concat "\n\n" (List.map get_css_from_element (StringMap.fold (fun id element l -> (element, slide.Slide.id ^ "-" ^ id)::l) slide.Slide.elements []))
;;

let string_of_text text = 
    if String.length text > 0 then
        "            " ^ text ^ "\n"
    else
        ""
;;

let string_of_image image = 
    if String.length image > 0 then
        "            <img src=\"" ^ image ^ " />\n"
    else
        ""
;;

let string_of_next_prev next_prev slide =
    if String.length slide > 0 then
        "        <a class=\"" ^ next_prev ^ "\" href=\"#" ^ slide ^ "\"></a>\n"
    else
        ""
;;

let rec get_html_from_element (element, element_id) =
    "        <div id=\"" ^ element_id ^ "\" class=\"box\">\n" ^

    string_of_text element.text ^
    string_of_image element.image ^

    (* Get the HTML from the elements of this element *)
    String.concat "\n" (List.map get_html_from_element (StringMap.fold (fun id element l -> (element, element_id ^ "-" ^ id)::l) element.Element.elements [])) ^ "\n" ^

    "        </div>"
;;

let get_html_from_slide slide =
    "    <div id=\"" ^ slide.Slide.id ^ "\" class=\"slide\">\n" ^

    (* Get the HTML from each element of this slide *)
    String.concat "\n\n" (List.map get_html_from_element (StringMap.fold (fun id element l -> (element, slide.Slide.id ^ "-" ^id)::l) slide.Slide.elements [])) ^ "\n\n" ^

    (* Next and prev *)
    string_of_next_prev "prev" slide.Slide.prev ^
    string_of_next_prev "next" slide.Slide.next ^

    "    </div>"
;;

let string_of_identifier = function
    Identifier(s) -> s
;;

let string_of_operator operator = match operator with
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Divide -> "/"
    | Equals -> "=="
    | Notequals -> "!="
    | Lessthan -> "<"
    | Greaterthan -> ">"
    | Or -> "||"
    | And -> "&&"

let rec string_of_expression (expression_detail, expression_type) = match expression_detail with
    | Binop (expr1, operator, expr2) -> string_of_expression expr1 ^ " " ^ string_of_operator operator ^ " " ^ string_of_expression expr2
    | Notop expr -> "!(" ^ string_of_expression expr ^ ")"
    | Litint integer -> string_of_int integer
    | Litper integer -> string_of_int integer ^ "%"
    | Litbool boolean -> string_of_bool boolean
    | Litstr str -> str
    | Litnull -> "null"
    | Assign (identifier, expr) -> string_of_identifier identifier ^ " = " ^ string_of_expression expr
    | Variable identifier -> string_of_identifier identifier
    | Component (identifier, exprlist) -> "TODO" (* identifier["child"]["child"] etc. to fetch component *)
    | Call func_call -> string_of_identifier func_call.cname ^ "(" ^ String.concat ", " (List.map (fun expr -> string_of_expression expr) func_call.actuals) ^ ");"
;;

let rec translate_statements_to_javascript statement = match statement with
    | Block stmt -> String.concat "\n            " (List.map translate_statements_to_javascript stmt)
    | Expr expr -> string_of_expression expr
    | Return expr -> "return " ^ string_of_expression expr ^ ";"
    | If (expr, stmt1, stmt2) -> "if (" ^ string_of_expression expr ^ ") {\n" ^ translate_statements_to_javascript stmt1 ^ "\n} else {\n" ^ translate_statements_to_javascript stmt2 ^ "\n}\n" (* if (foo == 42) stmt1 else stmt2 end *)
    | While (expr, stmt) -> "while (" ^ string_of_expression expr ^ ") {\n" ^ translate_statements_to_javascript stmt ^ "\n}\n"
    | Declaration identifier -> "var " ^ string_of_identifier identifier ^ ";"
    | Decassign (identifier, expr) -> "var " ^ string_of_identifier identifier ^ " = " ^ string_of_expression expr ^ ";"
;;

let translate_to_javascript script =
    (* Function definition *)
    "        function " ^ string_of_identifier script.name ^ "(" ^

    (* Formals *)
    String.concat ", " (List.map (fun identifier -> string_of_identifier identifier) script.formals) ^ ") {\n"^

    (* Statements *)
    "            " ^
    String.concat "\n            " (List.map translate_statements_to_javascript script.body) ^ "\n" ^

    "        }"
;;

let compile (slides, identifiers, scripts) =
    "<!DOCTYPE html>\n\n"^
    "<html>\n\n"^
    "<head>\n" ^

    (* If we have time, we should abstract out the config paths *)
    "    <link rel=\"stylesheet\" type=\"text/less\" href=\"../../config/config.css\">\n" ^

    "    <style type=\"text/css\">\n" ^

    (* Abstract out all of the CSS *)
    String.concat "\n" (List.map get_css_from_slide slides) ^ "\n" ^

    "    </style>\n" ^

    "</head>\n\n" ^
    "<body>\n" ^

    (* HTML components such as slides and elements *)
    String.concat "\n" (List.map get_html_from_slide slides) ^ "\n\n" ^

    "    <script type=\"text/javascript\" src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js\"></script>\n" ^
    "    <script type=\"text/javascript\" src=\"../../config/config.js\"></script>\n\n" ^

    "    <script>\n" ^

    (* Javascript goes here *)
    String.concat "\n" (List.map translate_to_javascript scripts) ^ "\n" ^
    
    "    </script>\n\n" ^

    "</body>\n\n" ^
    "</html>"
