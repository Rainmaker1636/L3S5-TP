## Conseils
à mettre dans le bashrc
ulimit -c unlimited : fichier core de taille non borné
-----
bactrace : affiche l'état de la pile au moment de l'erreur
-> associées ses commandes permettent de retrouver la ligne ayant provoquée une erreur de segmentation
voir aussi outil ddb

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


