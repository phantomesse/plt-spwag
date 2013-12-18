open Ir

let _ = 
let hw_text_css = {
    clazz="box-hello-world";
    
    display=false;

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
    clazz="box";
    image="";
    text="Hello world!";
    style=hw_text_css;
    (*onclick="";*)
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

let main_slide = {
    id="main";
    next="";
    prev="";
    image="";
    style=main_slide_css;
    (*onclick="";
    onpress="";*)
    elements=element_map
} in

let program = ([ hw_text_css ], [], [ main_slide ], []) in

let html = Compile.compile program in
print_string html