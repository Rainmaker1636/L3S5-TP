
##Graphe








##Tri topologique
''Sur des graphes acycliques uniquement''
(et utile uniquement si orientés)

Réécriture linéaire du graphe, lecture uniquement de la droite vers la gauche

Si départ du mauvais sommet -> relance le parcour sur un autre sommet bleu
sans effacer ce qui a été fait

Notion de visiteur : lancement d'une fonction lors d'un événement particulier
(ici lors du passage d'un sommet en rouge)

Utiliser la composante conexe pour éviter de retester les sommets bleus
