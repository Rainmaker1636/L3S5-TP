
Exercice 3
ls -l
---------- 1 ryckewaert l3     0 sept. 16 16:39 testlecture.c

$ ./macess -v -x testlecture.c
Error :: Permission denied
$ ./macess -v -r testlecture.c
Error :: Permission denied
$ ./macess -v -w testlecture.c
Error :: Permission denied
$ ./macess -a testlecture.c
Bad option


EACCES
L’accès serait refusé au fichier lui-même, ou il n’est pas permis de parcourir l’un des
répertoires du préfixe du chemin de pathname
(consultez aussi path_resolution(7)).
d--------- 2 uryon uryon  4096 sept. 22 09:34 tests_erreur/
./macess -v tests_erreur/1
Error :: Permission denied

ELOOP
Trop de liens symboliques ont été rencontrés en parcourant pathname.
ln -s 0 1
ln -s 1 2
ln -s 2 0
./macess -v tests_erreur/1
Error :: Too many levels of symbolic links

ENAMETOOLONG
pathname est trop long.
./macess -v tests_erreur/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
Error :: File name too long

ENOENT
Un composant du chemin d’accès pathname n’existe pas ou est un lien symbolique
pointant nulle part.
rm tests_erreur/2
./macess -v tests_erreur/1
Error :: No such file or directory

ENOTDIR
Un élément du chemin d’accès pathname n’est pas un répertoire.
./macess -v macess/1
Error :: Not a directory

EROFS
On demande une écriture sur un système de fichiers en lecture seule
Nécessite un périphérique externe.

4) Le macess+ ne marche pas, il renvoie "invalid argument" dans tous les cas, nous n'avons pas réussi à en trouver la raison.
