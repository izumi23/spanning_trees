libs = merge.cmx prioqueue.cmx unionfind.cmx print.cmx distrib.cmx tree.cmx graph.cmx prim.cmx kruskal.cmx boruvka.cmx aldousbroder.cmx draw.cmx wilson.cmx plot.cmx markovroute.cmx
libs-rhp = markovpath.cmx gamegraphics.cmx game.cmx

.PHONY: all clean
CC = ocamlfind opt
pkg = -linkpkg -package graphics -package unix



rhp : $(libs-rhp) rhp.ml
	$(CC) $(pkg) -o $@ $^


all : test acm acu stats rct rhp

test acm acu stats rct : % : $(libs) %.ml
	$(CC) -o $@ $^



### Structures de données

prioqueue.cmx : prioqueue.mli prioqueue.ml
	$(CC) -c $^

heap.cmx : heap.mli heap.ml
	$(CC) -c $^

unionfind.cmx : unionfind.mli unionfind.ml
	$(CC) -c $^


### Jeu avec interface graphique

game.cmx : gamegraphics.cmx game.ml
	$(CC) $(pkg) -c game.ml

gamegraphics.cmx : gamegraphics.ml
	$(CC) $(pkg) -c gamegraphics.ml


### Autres

%.cmx : %.ml
	$(CC) -c $^


### Clean

clean :
	rm *.cm* *.o acm acu test stats rct rhp || true