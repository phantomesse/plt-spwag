(* 
	Slide list is an intermediate representation that is very near the
	code that is to be generated, except it is not quite html

	All functions that generate static code are resolved, while all that
	are dynamic code are represented by the SAST ready for javascript
	generation.

	This also splits the code into distinct, mutually exclusive pages for
	easier output
*)

(* This represents an element within a page *)
(* An empty string "" corresponds to a null value *)
type element = {
	id : string;			(* User defined id, combine with index path for full *)
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
	width: string;
	height: string;
	image: string;
	text: string;
	elements: element list;
}

(* This is a page *)
(* An empty string "" corresponds to a null value *)
type page = {
	id : string; 			(* Name of the slide function *)
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
	next : string;			(* The string is the string id *)
	prev : string;			(* The string is the string id *)
	image: string;			
	elements: element list;
}

(* The overall program is just a list of pages *)
(* This is a list of pages, and a list of javascript function definitions *)
(* These functions must be functions! Cannot be components, attributes, etc. *)
type program = page list * sast.func_definition list 
