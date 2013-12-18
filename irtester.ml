open Ast
open Sast

let _ =

	let idList = [ Identifier("slide"); Identifier("hello-world-text"); Identifier("hello-world-image") ] in

	let mainSlide = {
    	t = Slide;
    	name = Identifier("main");
    	formals = [];
    	inheritance = Noparent(Null);
    	paractuals =  [];
    	body = [];
	} in

	let funList = [ mainSlide ] in

	let program = (idList, funList) in

		let blah = Irgenerator.generate program
	    	in print_string blah