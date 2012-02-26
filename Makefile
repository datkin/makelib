OCAMLC=ocamlc

LIBS=unix.cma

BUILD_DIR=.build

$(BUILD_DIR):
	mkdir $(BUILD_DIR)

# See http://owen.sj.ca.us/~rk/howto/slides/make/slides/makesuff.html
.SUFFIXES:
.SUFFIXES: .mli .cmi .ml .cmo

.mli.cmi:
#	$(OCAMLC) -I $(BUILD_DIR) -c -o $(BUILD_DIR)/$* lib/$*.mli
	$(OCAMLC) -I $(BUILD_DIR) -c -o $(BUILD_DIR)/`basename $*` $*.mli

.ml.cmo: $*.cmi
	$(OCAMLC) -I $(BUILD_DIR) -c -o $(BUILD_DIR)/$* $*.ml
