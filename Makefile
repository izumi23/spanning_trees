test : print.cmx distrib.cmx graph.cmx test.ml
	ocamlopt -o test print.cmx distrib.cmx graph.cmx test.ml

acm : print.cmx distrib.cmx graph.cmx prim.cmx kruskal.cmx boruvka.cmx acm.ml
	ocamlopt -o acm merge.cmx prioqueue.cmx unionfind.cmx print.cmx distrib.cmx graph.cmx prim.cmx kruskal.cmx boruvka.cmx acm.ml

kruskal.cmx : merge.cmx print.cmx unionfind.cmx graph.cmx kruskal.ml
	ocamlopt -c kruskal.ml

boruvka.cmx : print.cmx unionfind.cmx graph.cmx boruvka.ml
	ocamlopt -c boruvka.ml

prim.cmx : print.cmx prioqueue.cmx graph.cmx prim.ml
	ocamlopt -c prim.ml

# aldousbroder : aldousbroder.ml
# 	ocamlopt -c aldousbroder.ml


%.ml :
	@true

%.mli :
	@true

### Structures de données

prioqueue.cmx : prioqueue.mli prioqueue.ml
	ocamlopt -c prioqueue.mli prioqueue.ml

heap.cmx : heap.mli heap.ml
	ocamlopt -c heap.mli heap.ml

unionfind.cmx : unionfind.mli unionfind.ml
	ocamlopt -c unionfind.mli unionfind.ml


### Utilitaires

graph.cmx : graph.ml
	ocamlopt -c graph.ml

print.cmx : print.ml
	ocamlopt -c print.ml

merge.cmx : merge.ml
	ocamlopt -c merge.ml

distrib.cmx : distrib.ml
	ocamlopt -c distrib.ml


### Clean

clean :
	rm *.cm* *.o acm test || true


# kruskal : merge print unionfind graph kruskal.ml
# 	ocamlopt -o kruskal merge.cmx print.cmx unionfind.cmx graph.cmx kruskal.ml

# boruvka : print unionfind graph boruvka.ml
# 	ocamlopt -o boruvka print.cmx unionfind.cmx graph.cmx boruvka.ml
#
# prim : print prioqueue graph prim.ml
#	ocamlopt -o prim print.cmx prioqueue.cmx graph.cmx prim.ml
#
#test : print test.ml
#	ocamlfind opt -o test -linkpkg -package owl print.cmx test.ml
