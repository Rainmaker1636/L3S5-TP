#! /bin/sh -uf
#
#Campagne de tests mcat

#Commandes à tester pour le tp
MCATSCD=./mcat-scd
MCATSCS=./mcat-scs
MCATLIB=./mcat-lib
MCATSYNC=./mcat-fsync

#Création du fichier en entrée
dd if=/dev/zero of=bigfile count=100k
MCAT_INPUT=bigfile

#Fichiers de data à créer
TIME_FILESCD=mcat-scd.dat
TIME_FILESCS=mcat-scs.dat
TIME_FILELIB=mcat-lib.dat
TIME_FILESYNC=mcat-sync.dat
SIZE_FILE=bufsize.dat

#La commande gnu time
TIME_CMD="/usr/bin/time"
TIME_OPT="-f %e %U %S"

#Initialisation du fichier de résultat
rm -f $TIME_FILESCD && echo "#real user sys" > $TIME_FILESCD
rm -f $TIME_FILESCS && echo "#real user sys" > $TIME_FILESCS
rm -f $TIME_FILELIB && echo "#real user sys" > $TIME_FILELIB
rm -f $TIME_FILESYNC && echo "#real user sys" > $TIME_FILESYNC

#Initialisation du fichier taille de buffer
rm -f $SIZE_FILE && echo "#bufsiz" > $SIZE_FILE


#Création du fichier tailles du buffer
for size in `awk ' BEGIN { for (i=1; i<=8388608; i*=2) print i} '`; do
    export MCAT_BUFSIZ=$size
    echo $MCAT_BUFSIZ >> $SIZE_FILE
done

for size in `awk ' BEGIN { for (i=1; i<=8388608; i*=2) print i} '`; do
    export MCAT_BUFSIZ=$size
    echo $MCAT_BUFSIZ 
	$TIME_CMD "$TIME_OPT" "$MCATSCD" "$MCAT_INPUT" ls > /dev/null 2>> $TIME_FILESCD
done

#fusion des deux fichiers dat en un seul pour gnuplot
paste bufsize.dat mcat-scd.dat > mcat.dat

#Plot de la courbe mcat-scd
gnuplot gnuplot-config.gnu


#Test de mcat-scs avec taille de buffer optimale
$TIME_CMD "$TIME_OPT" "$MCATSCS" "$MCAT_INPUT" ls > /dev/null 2>> $TIME_FILESCS

#Test de mcat-lib (sans buffer)
$TIME_CMD "$TIME_OPT" "$MCATLIB" "$MCAT_INPUT" ls > /dev/null 2>> $TIME_FILELIB

#Test de mcat-sync
$TIME_CMD "$TIME_OPT" "$MCATSYNC" "$MCAT_INPUT" ls > /dev/null 2>> $TIME_FILESYNC





