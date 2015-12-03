#include <sys/wait.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "graphe.h"

#define BLEU 0
#define VERT 1
#define ROUGE 2

int d[MAX_SOMMETS];
tNumeroSommet pred[MAX_SOMMETS];

void plus_courte_chaine(tGraphe graphe,tNumeroSommet sommetActuel){
	/* Déclarations */
	int nbVi, i;
	int coulSommet[MAX_SOMMETS];
	int d[MAX_SOMMETS];
	tNumeroSommet pred[MAX_SOMMETS];
	/* Initialisations */
	printf("Recherche à partir du sommet %d.\n", sommetActuel);
	tFileSommets file = fileSommetsAlloue();
  for (i=0;i<MAX_SOMMETS;i++){
		coulSommet[i] = BLEU;
		d[i] = -1;
		pred[i] = -1;
	}
	while (! fileSommetsEstVide(file)){
		fileSommetsDefile(file);
	}
	coulSommet[sommetActuel] = VERT;
	d[sommetActuel] = 0;
	fileSommetsEnfile(file, sommetActuel);
	/* Début de la boucle */
	while (! fileSommetsEstVide(file)){
		sommetActuel = fileSommetsDefile(file);
		nbVi=grapheNbVoisinsSommet(graphe, sommetActuel);
		for(i=0;i<nbVi;i++){
			if (coulSommet[i] == BLEU){
				coulSommet[i] = VERT;
				fileSommetsEnfile(file, i);
				d[i] = d[sommetActuel] +1;
				pred[i] = sommetActuel;
			  printf("sommet %d : distance = %d, prédécesseur = %d.\n", i, d[i], pred[i]);
			}
		}
		coulSommet[sommetActuel] = ROUGE;
	}
	/* Libération de la mémoire */
	fileSommetsLibere(file);
}
