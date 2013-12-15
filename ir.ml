(* 
    ir.mli is the immediate represenation between the abstract syntax tree (ast.ml) and the actual compiled code, which is an HTML file with embedded CSS and JavaScript.

    All functions that generate static code are resolved into IR components, while all dynamic code are represented by the SAST for javascript generation.

    An empty string "" corresponds to a null value.
*)

(*open Sast*) (* The sast is needed for javascript functions *)

type css = {
    id : string;
    clazz : string;
    
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
    height : string;
}

type javascript = {
    id : string;
    clazz : string;
    call : string;              (* on-click, on-press *)
    execute : string list;      (* List of functions to execute *)
}

type element = {
    id : string;                (* Id of the element *)
    clazz : string;             (* Inheritance of this element; default is "box" *)
    image : string;             (* Image inside the element (optional) *)
    text : string;              (* Text inside the element (optional) *)
    elements : string list;     (* Map of element ids *)
}

type slide = {
    id : string;                (* Id of the slide *)
    next : string;              (* Id of the next slide *)
    prev : string;              (* Id of the previous slide *)
    elements : string list;     (* Map of element ids *)
}

(* These functions must be functions! Cannot be components, attributes, etc. *)
type program = slide list (* * Sast.func_definition list *)