
CC = gcc

## Options pour la compilation du C
#CFLAGS  = -Wall -Werror -ansi -pedantic -std=c99


all: testAnalyseur

%.o: %.c %.h
	${CC} ${CFLAGS} -c $<

testAnalyseur:testAnalyseur.c graphe.c
	${CC} -Wall -g -std=c99 testAnalyseur.c graphe.c -o testAnalyseur

Part2:Part2.c graphe.c
	${CC} -Wall -g -std=c99 Part2.c graphe.c -o Part2

%.grt:%.grp testAnalyseur
	./testAnalyseur $<



.PHONY: all clean 

clean:
	rm -f *.o *~ testAnalyseur
