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
all: mcat-lib mcat-fsync mcat-scd mcat-scs creer_dat


## Compilation séparée
#  Le .o doit être recompilé dès que le .c ou le .h (s'il existe) change
%.o: %.c %.h
	${CC} ${CFLAGS} -c $<

## Édition de liens
#  Définissez une règle par binaire que vous voulez créer
#  La commande devrait en général être identique pour chaque binaire
mcat-lib : mcat-lib.o
	${CC} ${LDFLAGS} -o $@ $^

mcat-fsync : mcat-fsync.o
	${CC} ${LDFLAGS} -o $@ $^

mcat-scd : mcat-scd.o
	${CC} ${LDFLAGS} -o $@ $^


## Liste des pseudo-cibles
#  Ces cibles ne correspondent pas à des fichiers que l'on veut créer,
#  juste à des séquences que l'on veut pouvoir déclencher
.PHONY: all clean realclean launchtest

creer_dat:
	./mcat.sh

clean:
	rm *.o || true
	rm *~ || true
	rm mcat-lib || true
	rm mcat-scs || true
	rm mcat-fsync || true
	rm mcat-scd || true
	rm *.dat || true

realclean: clean
	rm exemple || true
