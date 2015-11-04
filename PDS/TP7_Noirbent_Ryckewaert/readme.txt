Noirbent Marion
Ryckewaert Valentin 
GR2

Le TP fonctionne en entier excepté le --kill
Un petit soucis s'est présenté néamoins, nous avons utilisé getopt pour récupérer les paramètres mais n'avons pas réussi à récupérer des flags longs.
Pour le paramètre :
--and il faut donc taper -a
--or  il faut donc taper -o
--cc  il faut donc taper -c
--kill il faut donc taper -k

Pour lancer la compilation tapez make
Pour nettoyer le dossier tapez make clean
Pour exécuter les tests tapez ./test.sh
Pour tout lancer tapez make test
