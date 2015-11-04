
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

###?
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


