(* Authors: Yunhe (John) Wang, Lauren Zou, Aftab Khan *)

(*
    ir.mli is the immediate represenation between the sast.ml and the actual compiled code, which is an HTML file with embedded CSS and JavaScript.

    All functions that generate static code are resolved into IR components, while all dynamic code are represented by the SAST for javascript generation.

    An empty string "" corresponds to a null value. For nonstring, option is used.
*)
open Sast 
module StringMap = Map.Make(String)

(* All expressions must be evaluated before passing to javascript *)
type literal = 
  | Litint of int (* 42 *)
  | Litper of int (* 42% *)
  | Litstr of string (* “foo” *)
  | Litbool of bool (* true *)
  | Litcomp of Sast.identifier * string list (* identifier["child"]["child"] etc. to fetch component *)
  | Litslide of Sast.identifier (* identifier that is the name of a slide *)
  | Litnull

(* This is a call to whatever function is called onclick or onpress*)
type js_call = {
	cname : Sast.identifier;	(* Name of function passed to javascript, can only be of func type *)
	actuals: literal list; (* The actual parameters of the function passed, can only be literals *)
}

(* This is the template for all possible js function definitions *)
type js_definition = {
	name : Sast.identifier;	(* Name of the function *)
	formals : Sast.identifier list; (* Formal parameters *)
	body : Sast.stmt list;	(* Body of javascript definition *)
}

(* This css is either located with an element, or applies to a class of elements (comp definition) *)
module Element = struct
type css = {
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
    element_id : string;                          (* Unique id of a component WITHIN its outer component, concatenate with hyphens to obtain css id*)
    image : string;                               (* Image inside the element (optional) *)
    text : string;                                (* Text inside the element (optional) *)
    style : css;                                  (* CSS as applied to this particular element with this id *)
    (*onclick : js_call option;*)                     (* Name of javascript function to apply on click, empty string means none *)
    mutable elements : element StringMap.t;       (* Map of element id (string) -> element *)
}
end

module Slide = struct
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
    slide_id : string;                                    (* Id of the slide = name of the slide function*)
    next : string;                                        (* Id of the next slide = name of the slide function that is next *)
    prev : string;                                        (* Id of the previous slide = name of the slide function that is prev *)
    image : string;                                       (* URL of any background image *)
    style : slide_css;                                    (* CSS as applied to the slide in general *)
    (*onclick : js_call option;                             (* Name of javascript function to apply on click *)
    onpress : (string * js_call) option;*)                  (* Key to press, name of javascript function to apply on press *)
    mutable elements : Element.element StringMap.t;       (* Map of element id (string) -> element *)
}
end

(* Slide list is the list of slides, with its child elements, with their child elements, etc. *)
(* identifier list is a list of the global variables, these start out null at javascript run time *)
(* js definition list is a list of all the functions (not attr/comp/slide) to evaluate javascript *)
type program = Slide.slide list * Sast.identifier list * js_definition list
