libs = merge.cmx prioqueue.cmx unionfind.cmx print.cmx distrib.cmx tree.cmx graph.cmx prim.cmx kruskal.cmx boruvka.cmx aldousbroder.cmx draw.cmx wilson.cmx plot.cmx markovroute.cmx markovpath.cmx game.cmx
.PHONY: all clean
CC = ocamlfind opt
pkg = -linkpkg -package graphics


all : test acm acu stats rct rhp


test acm acu stats rct rhp : % : $(libs) %.ml
	$(CC) $(pkg) -o $@ $^



### Structures de données

prioqueue.cmx : prioqueue.mli prioqueue.ml
	$(CC) -c $^

heap.cmx : heap.mli heap.ml
	$(CC) -c $^

unionfind.cmx : unionfind.mli unionfind.ml
	$(CC) -c $^


### Jeu avec interface graphique

game.cmx : game.ml
	$(CC) $(pkg) -c game.ml


### Autres

%.cmx : %.ml
	$(CC) -c $^


### Clean

clean :
	rm *.cm* *.o acm acu test stats || true