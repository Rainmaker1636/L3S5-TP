Noirbent Marion
Ryckewaert Valentin
GR2

Le fichier mcat.sh créer les fichiers .dat et lance la génération de la courbe.
Il est lancé dans le makefile

Exercice 31 : 

#bufsiz	#real user sys
4096	 0.01 0.00 0.01

On constate grace aux tests que le temps atteint un minimum en 4096. Le temps d'execution est alors ideal (real user et system).
La taille choisie pour la suite sera donc 4096

Exercice 32

#real user sys
 0.70 0.69 0.00

On constate que le temps système a été réduit mais que les autres ont énormément augmentés. On peut donc supposer que l'utilisation du buffer réduit le temps d'execution de la commande.
