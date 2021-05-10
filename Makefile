test : print graph prim kruskal boruvka test.ml
	ocamlopt -o test merge.cmx prioqueue.cmx unionfind.cmx print.cmx graph.cmx prim.cmx kruskal.cmx boruvka.cmx test.ml

kruskal : merge print unionfind graph kruskal.ml
	ocamlopt -c kruskal.ml

boruvka : print unionfind graph boruvka.ml
	ocamlopt -c boruvka.ml

prim : print prioqueue graph prim.ml
	ocamlopt -c prim.ml

# aldousbroder : aldousbroder.ml
# 	ocamlopt -c aldousbroder.ml


### Structures de données

prioqueue : prioqueue.mli prioqueue.ml
	ocamlopt -c prioqueue.mli prioqueue.ml

heap : heap.mli heap.ml
	ocamlopt -c heap.mli heap.ml

unionfind : unionfind.mli unionfind.ml
	ocamlopt -c unionfind.mli unionfind.ml


### Utilitaires

graph : graph.ml
	ocamlopt -c graph.ml

print : print.ml
	ocamlopt -c print.ml

merge : merge.ml
	ocamlopt -c merge.ml


### Clean

clean :
	rm *.cm* *.o *.exe


# kruskal : merge print unionfind graph kruskal.ml
# 	ocamlopt -o kruskal merge.cmx print.cmx unionfind.cmx graph.cmx kruskal.ml

# boruvka : print unionfind graph boruvka.ml
# 	ocamlopt -o boruvka print.cmx unionfind.cmx graph.cmx boruvka.ml
#
# prim : print prioqueue graph prim.ml
#	ocamlopt -o prim print.cmx prioqueue.cmx graph.cmx prim.ml
