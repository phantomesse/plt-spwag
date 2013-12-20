(* Author: Lauren Zou *)

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

let string_of_css elem =
    (*"        ." ^ elem.cclass ^ " {\n" ^*)

    (*string_of_property "display", string_of_bool elem.display ^*)
    
    string_of_property "left" elem.position_x ^
    string_of_property "top" elem.position_y ^

    string_of_property "margin-top" elem.margin_top ^
    string_of_property "margin-bottom" elem.margin_bottom ^
    string_of_property "margin-left" elem.margin_left ^
    string_of_property "margin-right" elem.margin_right ^

    string_of_property "padding-top" elem.padding_top ^
    string_of_property "padding-bottom" elem.padding_bottom ^
    string_of_property "padding-left" elem.padding_left ^
    string_of_property "padding-right" elem.padding_right ^

    string_of_property "color" elem.text_color ^
    string_of_property "background-color" elem.background_color ^

    string_of_property "font-family" elem.font ^
    string_of_property "font-size" elem.font_size ^
    (* Need to handle font decoration with italics, bold, and underline *)

    string_of_property "border-width" elem.border ^
    string_of_property "border-color" elem.border_color ^

    string_of_property "width" elem.width ^
    string_of_property "height" elem.height ^

    "        }"
;;

let get_css_from_element element =
    "this is an element" ^ element.Element.id
;;

let get_css_from_slide slide =
    String.concat "" (List.map get_css_from_element (StringMap.fold (fun id element l -> element::l) slide.Slide.elements []))
;;

let compile (slides, identifiers, scripts) =
    "<!DOCTYPE html>\n\n"^
    "<html>\n\n"^
    "<head>\n" ^

    (* If we have time, we should abstract out the config paths *)
    "    <link rel=\"stylesheet\" type=\"text/less\" href=\"../../../config/config.css\">\n" ^

    "    <style type=\"text/css\">\n" ^

    (* *)
    (*String.concat "" (List.map string_of_css styles) ^ "\n" ^*)
    String.concat "" (List.map get_css_from_slide slides) ^ "\n" ^

    "    </style>\n" ^

    "</head>\n\n" ^
    "<body>\n" ^

    (* HTML components such as slides and elements go here *)

    "    <script type=\"text/javascript\" src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js\"></script>\n" ^
    "    <script type=\"text/javascript\" src=\"../../../config/config.js\"></script>\n" ^

    "    <script>\n" ^

    (* Javascript goes here *)
    
    "    </script>\n" ^

    "</body>\n\n" ^
    "</html>"
