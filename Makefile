OCAMLC=ocamlc

LIBS=unix.cma

# See: http://owen.sj.ca.us/~rk/howto/slides/make/slides/makesuff.html
#.SUFFIXES:
#.SUFFIXES: .mli .cmi .ml .cmo

.PHONY: clean

%.cmi: %.mli
	$(OCAMLC) -I lib -c $*.mli

%.cmo: %.ml %.cmi
	$(OCAMLC) -I lib -c $*.ml

clean:
	rm lib/*.cmi
	rm lib/*.cmo
