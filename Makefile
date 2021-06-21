libs = merge.cmx prioqueue.cmx unionfind.cmx print.cmx distrib.cmx tree.cmx graph.cmx prim.cmx kruskal.cmx boruvka.cmx aldousbroder.cmx draw.cmx wilson.cmx plot.cmx markovroute.cmx markovpath.cmx
.PHONY: all clean


all : test acm acu stats rct rhp


test acm acu stats rct rhp : % : $(libs) %.ml
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