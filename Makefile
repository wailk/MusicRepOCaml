
COMPILER= ocamlc
FLAGS= 
SOURCES= midi.ml projet.ml 
LIBS = graphics.cma


all:  clean projet

projet:  clean
	$(COMPILER) $(FLAGS) $(LIBS) $(SOURCES) -o projet

test: all
	./projet
clean:
	rm -f projet *.cmi *.cmo *.mid
