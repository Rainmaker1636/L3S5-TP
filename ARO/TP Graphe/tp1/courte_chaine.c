#include <sys/wait.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "graphe.h"

int d[MAX_SOMMETS];
tNumeroSommet pred[MAX_SOMMETS];

void plus_courte_chaine( //Page 69 support
