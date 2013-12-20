(* Authors: Lauren Zou *)

open Ast
open Sast
open Ir
open Ir.Element
open Ir.Slide

let _ = 
let hw_text_css = {    
    display=true;

    position_x="";
    position_y="";

    margin_top="";
    margin_bottom="";
    margin_left="";
    margin_right="";

    padding_top="40px";
    padding_bottom="20px";
    padding_left="0px";
    padding_right="0px";

    text_color="";
    background_color="";

    font="";
    font_size="";
    font_decoration="";

    border="";
    border_color="";

    width="330px";
    height="";
} in

let hw_text = {
    id="hello-world-text";
    image="";
    text="Hello world!";
    style=hw_text_css;
    onclick=None;
    elements=StringMap.empty
} in

let main_slide_css = {
    padding_top="";
    padding_bottom="";
    padding_left="";
    padding_right="";

    text_color="";
    background_color="";

    font="";
    font_size="";
    font_decoration="";

    border="";
    border_color="";
} in

let element_map = StringMap.empty in
let element_map = StringMap.add "hello-world-text" hw_text element_map in

let myjs = {
    name=Sast.Identifier("a_js_function"); (* Name of the function *)
    formals=[ Sast.Identifier("hello"); Sast.Identifier("cool"); Sast.Identifier("awesome") ]; (* Formal parameters *)
    body=[Sast.Declaration(Sast.Identifier("foo"))];  (* Body of javascript definition *)
} in

let main_slide = {
    id="main";
    next="";
    prev="";
    image="";
    style=main_slide_css;
    onclick=None;
    onpress=None;
    elements=element_map
} in

let program = ([ main_slide ], [], [myjs]) in

let html = Compile.compile program in
print_string html