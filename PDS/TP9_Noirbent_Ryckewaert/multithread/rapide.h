#ifndef _RAPIDE_H_
#define _RAPIDE_H_

#include "tri.h"
#include "pile.h"

extern base_t *tableau;
extern unsigned long seuil_bloc_long;

void rapide(pos_t taille, unsigned int nb_threads);

typedef struct arg_s {
	pile * p_bloc;
	pile * p_threads;
} arg_t;

#endif
