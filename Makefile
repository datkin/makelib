OCAMLC=ocamlc
MKTOP=ocamlmktop

# The -warn-error flag only applies to warnings that are enabled.
OCAML_FLAGS=-w +A -warn-error +A

LIBS=unix.cma

# See: http://owen.sj.ca.us/~rk/howto/slides/make/slides/makesuff.html
# Also: http://owen.sj.ca.us/~rk/howto/slides/make/slides/makemacro.html

# Don't delete intermediate files (ie .cmi files)
.SECONDARY:

.PHONY: clean

lib/process.cmo: lib/util.cmo

lib/path.cmo: lib/util.cmo

lib/module.cmi: lib/path.cmi
lib/module.cmo: lib/util.cmo lib/path.cmo

lib/ocamldep.cmi: lib/module.cmi
lib/ocamldep.cmo: lib/process.cmo lib/module.cmo

lib/file.cmi: lib/path.cmi
lib/file.cmo: lib/path.cmo

%.cmi: %.mli
	$(OCAMLC) $(OCAML_FLAGS) -I lib -c $*.mli

%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAML_FLAGS) -I lib -c $*.ml

top: lib/file.cmo lib/ocamldep.cmo
	$(MKTOP) -o lib/toplevel $(LIBS) lib/util.cmo lib/path.cmo lib/file.cmo lib/module.cmo lib/process.cmo lib/ocamldep.cmo
	echo "Remember #directory "lib""

clean:
	rm lib/*.cmi
	rm lib/*.cmo
