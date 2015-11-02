/* Ce program prend un nom de graphe en entrée, le charge,
   et affiche :
	- la liste des sommets qui n'ont pas de voisins
	- la  liste  des  sommets  qui  ont  le  plus  de  voisins
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "graphe.h"
void
max
(int *a,int b){
	if (b>a)
		*a=b;
}

int main(int argc, char *argv[]) {

  tGraphe graphe;

// Variables supplémentaires
int nbSommet=0;
int i;
int maxVoisin=0;
tNomSommet nom;

  if (argc<2) {
    halt("Usage : %s FichierGraphe\n", argv[0]);
  }

  
  graphe = grapheAlloue();
  
  grapheChargeFichier(graphe,  argv[1]);
  
/* Début code modifié */
nbSommet=grapheNbSommets(graphe);
for (i=0,i<nbSommet,i++){
	int nbVi=grapheNbVoisinsSommet(graphe, i);
	if (nbVi==0){
		grapheRecupNomSommet(graphe, i, nom);
		printf("%s n'a pas de voisin.\n", nom);
	} else {
		max(&maxVoisin,nbVi);
	}
}
if (maxVoisin != 0){
	for (i=0,i<nbSommet,i++){
	int nbVi=grapheNbVoisinsSommet(graphe, i);
	if (nbVi==maxVoisin){
		grapheRecupNomSommet(graphe, i, nom);
		printf("%s n'a pas de voisin.\n", nom);
	}
}

/* Fin code modifié */

  grapheLibere(graphe);

  exit(EXIT_SUCCESS);
}
