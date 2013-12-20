(* Author: Lauren Zou *)

open Ast
open Sast
open Ir
open Ir.Element
open Ir.Slide
module StringMap = Map.Make(String)

(* Inserts the appropriate number of tabs *)
let rec tab number =
    if number == 0 then ""
    else "    " ^ tab (number - 1)

(* Translates a CSS property given the property and its value *)
let string_of_css_property property value =
    if String.length value > 0 then
        tab 3 ^ property ^ ": " ^ value ^ ";\n"
    else ""
;;

(* Translates a CSS display property *)
let string_of_css_display_property display = match display with
    | true -> ""
    | false -> tab 3 ^ "display: hidden;\n"

(* Translates a CSS font decoration property *)
let string_of_css_font_decoration decoration = match decoration with
    | "" -> ""
    | "italic" -> tab 3 ^ "font-style: italic;\n"
    | "bold" -> tab 3 ^ "font-weight: bold;\n"
    | "underline" -> tab 3 ^ "text-decoration: underline;\n"
    | _ -> ""

(* Translates the CSS of an element given the CSS style and id *)
let string_of_element_css (style:Element.css) id =
    tab 2 ^ "#" ^ id ^ " {\n" ^

    string_of_css_display_property style.display ^
    
    string_of_css_property "left" style.position_x ^
    string_of_css_property "top" style.position_y ^

    string_of_css_property "margin-top" style.margin_top ^
    string_of_css_property "margin-bottom" style.margin_bottom ^
    string_of_css_property "margin-left" style.margin_left ^
    string_of_css_property "margin-right" style.margin_right ^

    string_of_css_property "padding-top" style.padding_top ^
    string_of_css_property "padding-bottom" style.padding_bottom ^
    string_of_css_property "padding-left" style.padding_left ^
    string_of_css_property "padding-right" style.padding_right ^

    string_of_css_property "color" style.text_color ^
    string_of_css_property "background-color" style.background_color ^

    string_of_css_property "font-family" style.font ^
    string_of_css_property "font-size" style.font_size ^
    string_of_css_font_decoration style.font_decoration ^

    string_of_css_property "border-width" style.border ^
    string_of_css_property "border-color" style.border_color ^

    string_of_css_property "width" style.width ^
    string_of_css_property "height" style.height ^

    tab 2 ^ "}\n"
;;

(* Translates the CSS of a slide given the CSS style *)
let string_of_slide_css (style:Slide.slide_css) classname =
    tab 2 ^ "#" ^ classname ^ " {\n" ^
    
    string_of_css_property "padding-top" style.padding_top ^
    string_of_css_property "padding-bottom" style.padding_bottom ^
    string_of_css_property "padding-left" style.padding_left ^
    string_of_css_property "padding-right" style.padding_right ^

    string_of_css_property "color" style.text_color ^
    string_of_css_property "background-color" style.background_color ^

    string_of_css_property "font-family" style.font ^
    string_of_css_property "font-size" style.font_size ^
    string_of_css_font_decoration style.font_decoration ^

    string_of_css_property "border-width" style.border ^
    string_of_css_property "border-color" style.border_color ^

    tab 2 ^ "}\n"
;;

(* Retrieves the styles from an element and its children elements *)
let rec get_css_from_element (element, element_id) =
    "\n" ^ string_of_element_css element.Element.style element_id ^

    (* Get the CSS from the elements of this element *)
    String.concat "\n" (List.map get_css_from_element (StringMap.fold (fun id element l -> (element, element_id ^ "-" ^ id)::l) element.Element.elements []))
;;

(* Retrieves the styles from a slide and its children elements *)
let get_css_from_slide slide =
    (* Get the CSS from the slide *)
    "\n" ^ string_of_slide_css slide.Slide.style slide.Slide.id ^ 

    (* Get the CSS from the elements of this slide *)
    String.concat "\n" (List.map get_css_from_element (StringMap.fold (fun id element l -> (element, slide.Slide.id ^ "-" ^ id)::l) slide.Slide.elements []))
;;

(* Translates a string of text to text *)
let string_of_text text tab_level = 
    if String.length text > 0 then
        tab (tab_level + 1) ^ text ^ "\n"
    else ""
;;

(* Translates an image url to an <img> tag *)
let string_of_image image tab_level = 
    if String.length image > 0 then
        tab (tab_level + 1) ^ "<img src=\"" ^ image ^ " />\n"
    else ""
;;

(* Translates next and prev slides into <a> tags *)
let string_of_next_prev next_prev slide =
    if String.length slide > 0 then
        tab 2 ^ "<a class=\"" ^ next_prev ^ "\" href=\"#" ^ slide ^ "\"></a>\n"
    else ""
;;

(* Retrieves the HTML from an element and its children elements *)
let rec get_html_from_element (element, element_id, tab_level) =
    tab tab_level ^ "<div id=\"" ^ element_id ^ "\" class=\"box\">\n" ^

    string_of_text element.text tab_level ^
    string_of_image element.image tab_level ^

    (* Get the HTML from the elements of this element *)
    String.concat "\n\n" (List.map get_html_from_element (StringMap.fold (fun id element l -> (element, element_id ^ "-" ^ id, tab_level + 1)::l) element.Element.elements [])) ^

    tab tab_level ^ "</div>\n"
;;

(* Retrieves the HTML from a slide and its children elements *)
let get_html_from_slide slide =
    tab 1 ^ "<div id=\"" ^ slide.Slide.id ^ "\" class=\"slide\">\n" ^

    (* Get the HTML from each element of this slide *)
    String.concat "\n\n" (List.map get_html_from_element (StringMap.fold (fun id element l -> (element, slide.Slide.id ^ "-" ^id, 2)::l) slide.Slide.elements [])) ^

    (* Next and prev *)
    string_of_next_prev "prev" slide.Slide.prev ^
    string_of_next_prev "next" slide.Slide.next ^

    tab 1 ^ "</div>"
;;

(* Translates an identifier into a string *)
let string_of_identifier = function
    Identifier(s) -> s
;;

(* Translates an operator into a string *)
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

(* Translates an expression into a string *)
let rec string_of_expression (expression_detail, expression_type) = match expression_detail with
    | Binop (expr1, operator, expr2) ->
        string_of_expression expr1 ^ " " ^ string_of_operator operator ^ " " ^ string_of_expression expr2
    | Notop expr -> "!(" ^ string_of_expression expr ^ ")"
    | Litint integer -> string_of_int integer
    | Litper integer -> string_of_int integer ^ "%"
    | Litbool boolean -> string_of_bool boolean
    | Litstr str -> str
    | Litnull -> "null"
    | Assign (identifier, expr) ->
        string_of_identifier identifier ^ " = " ^ string_of_expression expr
    | Variable identifier -> string_of_identifier identifier
    | Component (parent, children) ->
        "$('#" ^ string_of_identifier parent ^ " " ^
        String.concat " " (List.map (fun child -> "#" ^ string_of_expression child) children) ^ "')"
    | Call func_call ->
        string_of_identifier func_call.cname ^ "(" ^ String.concat ", " (List.map (fun expr -> string_of_expression expr) func_call.actuals) ^ ");"
;;

(* Translates a statement into a string *)
let rec string_of_statement statement tab_level = match statement with
    | Block stmt ->
        String.concat "\n" (List.map (fun statement -> string_of_statement statement tab_level) stmt)
    | Expr expr ->
        tab tab_level ^ string_of_expression expr ^ ";"
    | Return expr ->
        tab tab_level ^ "return " ^ string_of_expression expr ^ ";"
    | If (expr, stmt1, stmt2) ->
        tab tab_level ^ "if (" ^ string_of_expression expr ^ ") {\n" ^ (string_of_statement stmt1 (tab_level + 1)) ^ "\n" ^
        tab tab_level ^ "} else {\n" ^ (string_of_statement stmt2 (tab_level + 1)) ^ "\n" ^
        tab tab_level ^ "}\n"
    | While (expr, stmt) ->
        tab tab_level ^ "while (" ^ string_of_expression expr ^ ") {\n" ^ (string_of_statement stmt (tab_level + 1)) ^ "\n" ^
        tab tab_level ^ "}\n"
    | Declaration identifier ->
        tab tab_level ^ "var " ^ string_of_identifier identifier ^ ";"
    | Decassign (identifier, expr) ->
        tab tab_level ^ "var " ^ string_of_identifier identifier ^ " = " ^ string_of_expression expr ^ ";"
;;

(* Translates the script into JavaScript *)
let get_javascript script =
    (* Function definition *)
    tab 2 ^ "function " ^ string_of_identifier script.name ^ "(" ^

    (* Formals *)
    String.concat ", " (List.map (fun identifier -> string_of_identifier identifier) script.formals) ^ ") {\n"^

    (* Statements *)
    String.concat "\n" (List.map (fun statement -> string_of_statement statement 3) script.body) ^ "\n" ^

    tab 2 ^ "}"
;;

(* Translates literals into strings *)
let string_of_literal literal = match literal with
    | Litint integer -> string_of_int integer
    | Litper integer -> string_of_int integer ^ "%"
    | Litstr str -> str
    | Litbool boolean -> string_of_bool boolean
    | Litcomp (parent, children) ->
        "$('#" ^ string_of_identifier parent ^ " " ^
        String.concat " " (List.map (fun child -> "#" ^ child) children) ^ "')"
    | Litslide identifier -> string_of_identifier identifier
    | Litnull -> "null"
;;

(* Translates onclick functionality *)
let string_of_onclick js_call id = match js_call with
    | None -> ""
    | Some(onclick) -> 
        tab 2 ^ "$('#" ^ id ^ "').bind('click', " ^ string_of_identifier onclick.cname ^ "(" ^
        String.concat ", " (List.map string_of_literal onclick.actuals) ^
        "));\n"
;;

(* Translates onpress functionality *)
let string_of_onpress js_call id = match js_call with
    | None -> ""
    | Some(onpress) ->
        tab 2 ^ "$('#" ^ id ^ "').keypress(function(e) {\n" ^
        tab 3 ^ "if (e.keycode == '" ^ fst onpress ^ "') {\n" ^
        tab 4 ^ string_of_identifier (snd onpress).cname ^ "(" ^
        String.concat ", " (List.map string_of_literal (snd onpress).actuals) ^
        ");\n" ^
        tab 3 ^ "}\n" ^
        tab 2 ^ "});\n"
;;

(* Gets the onclick of an element *)
let rec get_element_onclick (element, element_id) =
    string_of_onclick element.Element.onclick element_id ^

    (* Get onclick of children elements *)
    String.concat "\n\n" (List.map get_element_onclick (StringMap.fold (fun id element l -> (element, element_id ^ "-" ^ id)::l) element.Element.elements []))
;;

(* Gets the onclick and onpress of a slide *)
let get_slide_onclick_onpress slide =
    string_of_onclick slide.Slide.onclick slide.Slide.id ^
    string_of_onpress slide.Slide.onpress slide.Slide.id ^

    (* Get onclick of children elements *)
    String.concat "\n\n" (List.map get_element_onclick (StringMap.fold (fun id element l -> (element, slide.Slide.id ^ "-" ^id)::l) slide.Slide.elements []))
;;

let compile (slides, identifiers, scripts) =
    "<!DOCTYPE html>\n\n"^
    "<html>\n\n"^
    "<head>\n" ^

    (* If we have time, we should abstract out the config paths *)
    tab 1 ^ "<link rel=\"stylesheet\" type=\"text/less\" href=\"../../config/config.css\">\n" ^

    tab 1 ^ "<style type=\"text/css\">\n" ^

    (* Abstract out all of the CSS *)
    String.concat "\n" (List.map get_css_from_slide slides) ^ "\n" ^

    tab 1 ^ "</style>\n" ^

    "</head>\n\n" ^
    "<body>\n" ^

    (* HTML components such as slides and elements *)
    String.concat "\n" (List.map get_html_from_slide slides) ^ "\n\n" ^

    tab 1 ^ "<script type=\"text/javascript\" src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js\"></script>\n" ^
    tab 1 ^ "<script type=\"text/javascript\" src=\"../../config/config.js\"></script>\n\n" ^

    tab 1 ^ "<script>\n" ^

    (* Handle onclicks and onpresses *)
    String.concat "\n" (List.map get_slide_onclick_onpress slides) ^ "\n" ^

    (* Javascript functions *)
    String.concat "\n" (List.map get_javascript scripts) ^ "\n" ^
    
    tab 1 ^ "</script>\n\n" ^

    "</body>\n\n" ^
    "</html>\n"
