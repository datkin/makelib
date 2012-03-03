# Useful Make docs: http://www.gnu.org/software/make/manual/make.html

OCAMLC=ocamlc
MKTOP=ocamlmktop

# The -warn-error flag only applies to warnings that are enabled.
OCAML_FLAGS=-w +A -warn-error +A

LIBS=unix.cma

# All modules listed in order.
# Add extensions, with, eg, $(MODULES:=.cmo)
MODULES = \
  util \
  path \
  file \
  module \
  process \
  ocamldep \
  graph

OBJS=$(addprefix lib/, $(MODULES:=.cmo))

# See: http://owen.sj.ca.us/~rk/howto/slides/make/slides/makesuff.html
# Also: http://owen.sj.ca.us/~rk/howto/slides/make/slides/makemacro.html

# Don't delete intermediate files (ie .cmi files)
.SECONDARY:

.PHONY: clean test

lib/process.cmo: lib/util.cmo

lib/path.cmo: lib/util.cmo

lib/module.cmi: lib/path.cmi
lib/module.cmo: lib/util.cmo lib/path.cmo

lib/ocamldep.cmi: lib/module.cmi
lib/ocamldep.cmo: lib/process.cmo lib/module.cmo

lib/file.cmi: lib/path.cmi
lib/file.cmo: lib/path.cmo

lib/graph.cmo: lib/util.cmo

%.cmi: %.mli
	$(OCAMLC) $(OCAML_FLAGS) -I lib -c $*.mli

%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAML_FLAGS) -I lib -c $*.ml

test: $(OBJS)
	$(OCAMLC) $(OCAML_FLAGS) -I lib unix.cma lib/util.cmo test/test.ml

top: $(OBJS)
	$(MKTOP) -o lib/toplevel $(LIBS) $(OBJS)
	echo "Remember #directory "lib""

clean:
	rm lib/*.cmi
	rm lib/*.cmo
