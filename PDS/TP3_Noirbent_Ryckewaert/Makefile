### Modèle pour vos Makefile

## Compilateur C et éditeur de liens
CC      = gcc

## Options pour la compilation du C
CFLAGS  = -Wall -Werror -ansi -pedantic
#  Spécifie la version de la norme POSIX à respecter
CFLAGS += -D_XOPEN_SOURCE=500
#  Active les informations de débogage
CFLAGS += -g

## Options de l’édition de liens
LDFLAGS  = -g

## Première cible
all: exo16 exo18


## Compilation séparée
#  Le .o doit être recompilé dès que le .c ou le .h (s'il existe) change
%.o: %.c %.h
	${CC} ${CFLAGS} -c $<

## Édition de liens
#  Définissez une règle par binaire que vous voulez créer
#  La commande devrait en général être identique pour chaque binaire
tail: tail.o
	${CC} ${LDFLAGS} -o $@ $^

exo16: exo16.o
	${CC} ${LDFLAGS} -o $@ $^

exo18: exo18.o
	${CC} ${LDFLAGS} -o $@ $^

## Liste des pseudo-cibles
#  Ces cibles ne correspondent pas à des fichiers que l'on veut créer,
#  juste à des séquences que l'on veut pouvoir déclencher
.PHONY: all clean realclean launchtest

clean:
	rm *.o || true
	rm *~ || true
	rm exo16 || true
	rm exo18 || true

realclean: clean
	rm exemple || true
