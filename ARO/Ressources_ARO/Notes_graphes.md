
##Graphe
simple : ne passe pas deux fois par le même arc
élémentaire : ne passe pas deux fois par le même sommet

connexe : existe un chemin de tout sommet vers tout sommet


##Tri topologique
''Sur des graphes acycliques uniquement''
(et utile uniquement si orientés)

Réécriture linéaire du graphe, lecture uniquement de la droite vers la gauche

Si départ du mauvais sommet -> relance le parcour sur un autre sommet bleu
sans effacer ce qui a été fait

Notion de visiteur : lancement d'une fonction lors d'un événement particulier
(ici lors du passage d'un sommet en rouge)

Utiliser la composante conexe pour éviter de retester les sommets bleus


#Chemin de valeur minimal
(P74, 4.6)
chemin de valeurs minimal : noté pi (grec)

Bellman
fonctionne sur les graphes sans circuit, peut fonctionner avec les arcs ayant une valeur négative
faire 1er parcours pour définir les sommets accessibles

Dijksta
fonctionne en présence de circuit, avec des arcs à valeurs positives uniquement
fonctionne par estimation -> modifie pi(x) pendant le traitement
algo glouton : raisonne localement, sans remise en cause -> choix dans la meilleurs direction locale

/!\ complexité 
- recherche sommet vert, recherche minimum -> tas binaire
risque de n², mais possible O(m log n)

