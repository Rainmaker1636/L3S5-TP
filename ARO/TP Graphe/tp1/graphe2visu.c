#include <sys/wait.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "graphe.h"

/* Début fonctions intermediaires */
void dotOriente(graphe, fic){
  int i,j,h;
  /* i : num sommet d'origine, j : indice successeur, h : num sommet de destination */
  int nbSommet=grapheNbSommets(graphe);
  tNomSommet origine, destination;
  fprintf(fic, "digraph {\n");
  for (i=0;i<nbSommet;i++){
    int nbVi=grapheNbVoisinsSommet(graphe, i);
    grapheRecupNomSommet(graphe, i, origine);
    if (nbVi==0){
      fprintf(fic, "%s;\n", origine);
    } else {
      int nbSi=grapheNbSuccesseursSommet (graphe, i);
      for (j=0;j<nbSi;j++){
	h=grapheSuccesseurSommetNumero (graphe, i, j);
	grapheRecupNomSommet(graphe, h, destination);
	fprintf(fic, "%s -> %s;\n", origine, destination);
      }
    }
  }
  fprintf(fic, "}\n");
}

void dotNonOriente(graphe, fic){
  
}

/* Fin fonctions intermediaires */

void graphe2visu(tGraphe graphe, char *outfile) {
  FILE *fic;
  char commande[80];
  char dotfile[80]; /* le fichier dot pour creer le ps */
  int ret;
  
  /* on va creer un fichier pour graphviz, dans le fichier "outfile".dot */
  strcpy(dotfile, outfile);
  strcat(dotfile, ".dot");
  fic = fopen(dotfile, "w");
  if (fic==NULL){
    halt ("Ouverture du fichier %s en ecriture impossible\n", dotfile);
  }
  
  /*
    on parcourt le graphe pour en tirer les informations
    necessaires pour graphviz.
    Pour ecrire dans le fichier, on utilise fprintf (qui s’utilise
    comme printf mais on mettant en plus fic comme premier parametre).
    Ex :
    fprintf(fic, "graph {\n");
    ou
    fprintf(fic, " %s -> %s\n", origine, destination);
  */


  /* Début code modifié */

  if (estOriente(graphe)){
    dotOriente(graphe, fic); 
  } else {
    dotNonOriente(graphe, fic);
  }
  
  /* Fin code modifié */
  
  fclose(fic);
  sprintf(commande, "dot -Tps %s -o %s", dotfile, outfile);
  ret = system(commande);
  if (WEXITSTATUS(ret)){
    halt("La commande suivante a echoue\n%s\n", commande);
  }
}

