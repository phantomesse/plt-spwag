OBJS = ast.cmo linecounter.cmo parser.cmo scanner.cmo spwag.cmo

all: ast.cmo linecounter.cmo parser.cmi parser.cmo scanner.cmo spwag

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
