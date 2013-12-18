(*
    ir.mli is the immediate represenation between the sast.ml and the actual compiled code, which is an HTML file with embedded CSS and JavaScript.

    All functions that generate static code are resolved into IR components, while all dynamic code are represented by the SAST for javascript generation.

    An empty string "" corresponds to a null value. For nonstring, option is used.
*)
open Sast 
module StringMap = Map.Make(String)

(* This css is either located with an element, or applies to a class of elements (comp definition) *)
type css = {
    clazz : string;
    
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

    text_color : string;
    background_color : string;

    font : string;
    font_size : string;
    font_decoration : string;

    border : string;
    border_color : string;

    width : string;
    height : string;
}

(* Elements with a slide or element *)
type element = {
    id : string;                          (* Unique id of a component WITHIN its outer component, concatenate with hyphens to obtain css id*)
    clazz : string;                       (* This class specifies all attributes applied to this element via the component definition *)
    image : string;                       (* Image inside the element (optional) *)
    text : string;                        (* Text inside the element (optional) *)
    style : css;                          (* CSS as applied to this particular element with this id *)
    onclick : Sast.func_call option;      (* Name of javascript function to apply on click, empty string means none *)
    elements : element StringMap.t;       (* Map of element id (string) -> element *)
}

(* Possible CSS that can apply to slides *)
type slide_css = {
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
}

(* This is a slide*)
type slide = {
    id : string;                                  (* Id of the slide = name of the slide function*)
    next : string;                                (* Id of the next slide = name of the slide function that is next *)
    prev : string;                                (* Id of the previous slide = name of the slide function that is prev *)
    image : string;                               (* URL of any background image *)
    style : slide_css;                            (* CSS as applied to the slide in general *)
    onclick : Sast.func_call;                     (* Name of javascript function to apply on click *)
    onpress : (string * Sast.func_call) option;   (* Key to press, name of javascript function to apply on press *)
    elements : element StringMap.t;               (* Map of element id (string) -> element *)
}

(* Slide list is the list of slides, with its child elements, with their child elements, etc. *)
(* func defintion list is a list of all the functions (not attr/comp/slide) to evaluate javascript *)
(* css list is a list of css that applies to CLASSES (or CLAZZES), these are generated from component definitions (and not component calls) *)
type program = css list * slide list *  Sast.func_definition list
