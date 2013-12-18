open Ast
open Sast
(*open Irgenerator*)

let idList = [ Identifier("slide"); Identifier("hello-world-text"); Identifier("hello-world-image") ]

let mainSlide = {
    t = Slide;
    name = Identifier("main");
    formals = [];
    inheritance = Noparent(Null);
    paractuals =  [];
    body = [];
}

let funList = [ mainSlide ]

let program = (idList, funList)

let ir = Irgenerator.blah