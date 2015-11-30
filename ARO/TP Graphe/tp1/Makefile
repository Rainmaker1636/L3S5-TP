
CC = gcc

## Options pour la compilation du C
#CFLAGS  = -Wall -Werror -ansi -pedantic -std=c99


all: testAnalyseur Part2

%.o: %.c %.h
	${CC} ${CFLAGS} -c $<

testAnalyseur:testAnalyseur.c graphe.c
	${CC} -Wall -g -std=c99 testAnalyseur.c graphe.c -o testAnalyseur

Part2:Part2.c graphe.c courte_chaine.c
	${CC} -Wall -g -std=c99 Part2.c graphe.c courte_chaine.c -o Part2

%.grt:%.grp testAnalyseur Part2
	@echo "\n ******************* Test Analyseur ************\n"
	@./testAnalyseur $<
	@echo "\n ******************* Test Partie 2 **************"
	@./Part2 $<

%.ps:%.dot
	dot -Tps $< -o $@

%.e:%.ps
	@evince $< 2> /dev/null &



.PHONY: all clean

clean:
	rm -f *.o *~ testAnalyseur Part2 *.ps
