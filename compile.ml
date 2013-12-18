open Ir

let string_of_css css =
    (*"." ^ css.clazz ^ "\n" ^*)
    "{" ^
    "\ttop: " ^ css.position_x ^ "\n" ^
    "\twidth: " ^ css.width ^ "\n" ^ 
    "\theight: " ^ css.height ^ "\n" ^
    "}"

(*clazz : string;
    
    display : bool;
    
    position_x : string;
    position_y : string;
    
    margin_top : string;
    margin_bottom : string;
    margin_left : string;
    margin_right : string;
    
    padding_top : string;
    padding_bottom : string;
    padding_left : string;
    padding_right : string;

    text_color: string;
    background_color : string;

    font : string;
    font_size : string;
    font_decoration : string;

    border : string;
    border_color : string;

    width : string;
    height : string;*)

let compile (styles, slide_styles, slides, funcs) =
    "<!DOCTYPE html>\n\n"^
    "<html>\n"^
    "<head>\n" ^
    "\t<link rel=\"stylesheet\" type=\"text/less\" href=\"../../../config/config.css\">" ^ (* If we have time, we should abstract out the config paths *)

    "\t<style type=\"text/css\">\n" ^
    String.concat "" (List.map string_of_css styles) ^ "\n" ^

    "\t</style>" ^

    "</head>" ^
    "<body>" ^


    "</body>" ^
    "</html>"