## Conseils
à mettre dans le bashrc
ulimit -c unlimited : fichier core de taille non borné
-----
bactrace : affiche l'état de la pile au moment de l'erreur
-> associées ses commandes permettent de retrouver la ligne ayant provoquée une erreur de segmentation
voir aussi outil ddb
-----------
pointeurs : 2 cas
- écriture d'un résultat à un endroit
- pzrtzge d'une variable

##Commande
ps : récupération des pid des processus en cours

time : temps d'execution des commandes
temps dans le noyau, temps dans le code et temps total (/!\ multi-core)

strace (option -fork) : trace les appels systèmes

proc/pid/maps -> map de la mémoire
sed : remplacement
cut : découpage

##Descripteur de fichiers


##Fork
defunct = zombie

wait non bloquant pour récuppérer tout les fis qui s'arrêtent sans en connaitre le nombre

##Signal
A été pensé de façon à gerer lorsqu'il y a plus d'événements que ce que le processeur peut traiter
-> ne traite qu'une seule fois un signal si il est reçu plusieurs fois pendant un blocage, (puis traites un autre signal reçu, pas d'ordre)

pause() : bloque jusqu'à la réception d'un signal

Masquage de signal -> noté comme reçu mais déclenché qu'au retrait du mask

Déclenchement du signal : execute le comportement spécifié, puis reprends là où le programme en était


##Tube
tube = file d'octect
read et write -> tampon mémoire noyau de taille fixe (au moins de taille PIPE_BUF)

pipe : table de descripteur de fichier de taille 2 ( fd\[2] )
	Necessité de tester si descripteur de fichier est en lecture ou écriture
	-> ici 0 = lecture, 1 = écriture
Peut poser des problèmes de cohérence en réseaux car fait par des compilateurs différents

ICP : communication inter-processus
pipe() : crée un tube anonyme
usleep(param) : attente param en millisecondes

fermeture d'une partie du tube -> détectée par le système
	si plus personne ne peut écrire sur le tube, fermeture du tube, si read() récupère eof/signal
	idem plus de lecteur mais erreur : signal SIGPIPE
bonne pratique de fermer les parties du tube non utilisées

ressource : man 7 pipe
	read et write sont bloquant si pas d'octet/tube plein -> synchronisation des processus
	possible de faire non bloquant, utiliser pipe2(options)
	
PIPE_BUF : si on essaye d'écrire moins que pipe buf, écrira tout d'un coup quand il aura la place de le faire (écriture atomique) -> pas de mélangee de message si plusieurs écrivains

strace : trace tout les appels systèmes d'un processus
EAGAIN : réponse à un appel système non bloquant (= repasse plus tard)
fctrl() : permet de manipuler un descripteur de fichier (man 2)
	SETFL : set flag, GETFL :  get flag -> permet d'affecter non bloquant hors de la création par pipe2

shell : commande1 | commande2
	- création du pipe avant le 1er fils (cmd1) sinon impossible de partager
	- 1er fils : fermeture de la lecture du tube
	dup de sa sortie standard par l'écriture sur le tube
	close de l'écriture sur le tube
	exec cmd1
	- 2eme fils idem inversé
	-	Fermeture du tube sur le shell
	
dup() : remplacement d'un file descripteur par un auutre (sortie standard par tube par exemple) (man 2)

appel systeme qui fait le fork : s'appelle clone

##Mémoire partagée
mmap :

gestion fine de la mémoire peut être plus couteux en méoire qu'uune affectation large

Section critique -> Execution en "Exclusion mutuelle" ou "Atommique d'une section de code"
Sémaphore : ensemble de jeton, type sem_t
Edition de lien avecc -pthread

Problème des producteur/consomateurs
1 file (ou un tableau avec pointeur)
2 sémaphores : un "libre" pour les prod, un "occupé" pour les conso
producteur : prend dans libre et rend dans occupé
consommateur : idem inversé

Problème des philosophes
interblocage (ou deadblock)
- possibilité de cassage de symétrie (pour nombre paire de philisophes)
- option pour essayer de prendre pendant un certain temps
- Versions récentes : possibilité de prendre plusieurs sémaphores en une fois

##Threads
posix : pthread
(cwt : bibliothèque sympa sous caml)

Problème fork :
Interaction et calcul indépendant
Recouvrement, fonctions bloquantes

Processus léger, fil d'exécution
id : numéro abstrait
à l'origine étéient des processus

pthread_create -> écrit l'id du thread créé dans son param
pthread_self -> récupere son id

fonction réentrantes -> ne doivent pas modifier de variable globales (à cause du partage de mémoire)
retour des fonctions pthread contiennent leur code d'erreur (err_no étant globale)

pas de hierarchie -> n'importe quel thread peut attendre les autres, peut exiter seul

2 modes de gestion de la fin (pas de mode zombi mais doit être fini) :
- attente du join
- detach (ne permet pas de passer outre un return)

mutex sémaphore à 1 jeton, propriétaire du verrou

futex : fast appel systeme sur un mutex
verrou récursif : proriétaire + nombre de prise en main, mal normalisé

appel systeme qui fait le fork : s'appelle clone, différence avec fork par des options

mutex + condition, condition permet de bloquer jusqu'au signal (wait, signal, broadcast)

attention, passage de variable locale dans les threads -> perte lors de la fin de la fonction

Tony Hoare : travail sur les synchronisation

