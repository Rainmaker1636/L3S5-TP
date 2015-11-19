
##00



##01


##02


##03



##04



##05



##06



##07
fd\[2] -> ici 0 = lecture, 1 = écriture

###fork
Exemple pour observer le fonctionnement du pipe

###entrelacs
Envoie de messages plus petit/grand que pipe_buf pour observer l'écriture atomique

###nonbloc
cration d'un pipe non bloquant
/!\ galère sur les include -> ignoré sur la compilation

###ing-segv
tentative de contourner l'erreur de segmentation
- Masquage fonctionne si le signal est envoyé par le shell (nb : résultat indéfini), mais passe si envoyé par le noyau
- idem avec un ignore de sigaction
- fonctionne plus ou moins si on déclare pouvoir gérer l'erreur mais redéclenche à l'infini (car retourne à l'instruction l'ayant déclenché)

###list
exemple d'une erreur : affichage du 2ème élément d'une liste à 1 élèment


##08
###mmap
Exemple de partage de la mémoire entre les fils
Problème lors de l'incrémentation de la mémoire : écrasement de ce que fait l'autre processus si après le chargement en registre
-> chargement, incrémentation, affectation

avec Sémaphore : beaucoup de temps passé dans le noyau pour départager les processus (mais nécessaire)

###prodcons
Implémentation du problème producteur / consomateur

###philo
Implémentation du probllème des philosophes

##09
###th-1
exemple de mauvaise utilisation

###th-maps
Lancement d'un thread avec affichage de la map -> apparition d'une deuxième pile
1er thread a spid identique au pid
les threads sont dans le même espace mémoire

###th-2
return : tue le processus "lourd", tuant tout ses processus légers
-> join
pas de synchronisation -> imprévisibilité de l'ordonnanceur


###th-maps-errno
bidouille -> préprocesseur remplace l'adresse de errno par une fonction qui retourne l'adresse du errno dans ce thread -> pas vraiment globale

###compteur-incr
mutex
