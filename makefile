OBJS = ast.cmo sast.cmo linecounter.cmo parser.cmo scanner.cmo irgenerator.cmo compile.cmo spwag.cmo

spwag: $(OBJS)
	ocamlc -o spwag $(OBJS)

scanner.ml: scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli: parser.mly
	ocamlyacc parser.mly

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

.PHONY: clean
clean:
	rm -f parser.ml parser.mli scanner.ml *.cmo *.cmi spwag

# Generated by ocamldep *.ml *.mli
ast.cmo :
ast.cmx :
compile.cmo : ir.cmo
compile.cmx : ir.cmx
ir.cmo :
ir.cmx :
irgenerator.cmo : ir.cmo
irgenerator.cmx : ir.cmx
linecounter.cmo :
linecounter.cmx :
parser.cmo : linecounter.cmo ast.cmo parser.cmi
parser.cmx : linecounter.cmx ast.cmx parser.cmi
scanner.cmo : parser.cmi linecounter.cmo
scanner.cmx : parser.cmx linecounter.cmx
semantic_analyzer.cmo : sast.cmi ast.cmo
semantic_analyzer.cmx : sast.cmi ast.cmx
spwag.cmo : scanner.cmo parser.cmi irgenerator.cmo compile.cmo ast.cmo
spwag.cmx : scanner.cmx parser.cmx irgenerator.cmx compile.cmx ast.cmx
parser.cmi : ast.cmo
sast.cmi :
