libs = merge.cmx prioqueue.cmx unionfind.cmx print.cmx distrib.cmx graph.cmx prim.cmx kruskal.cmx boruvka.cmx aldousbroder.cmx draw.cmx wilson.cmx
.PHONY: all clean


all : test acm acu stats

test : $(libs) test.ml
	ocamlopt -o $@ $^

acm : $(libs) acm.ml
	ocamlopt -o $@ $^

acu : $(libs) acu.ml
	ocamlopt -o $@ $^

stats : $(libs) stats.ml
	ocamlopt -o $@ $^


### Structures de données

prioqueue.cmx : prioqueue.mli prioqueue.ml
	ocamlopt -c $^

heap.cmx : heap.mli heap.ml
	ocamlopt -c $^

unionfind.cmx : unionfind.mli unionfind.ml
	ocamlopt -c $^


### Autres

%.cmx : %.ml
	ocamlopt -c $^


### Clean

clean :
	rm *.cm* *.o acm acu test stats || true