/consult jalousie.dl

Q4)femme(mia).femme(yolande).femme(lapin).
Q5) femme(X).
Q6) femme(X),aime(vincent,X).
Q7)	aime(vincent,X),not(femme(X)). 
	aime(vincent,X),homme(X).
Q8) est_jaloux(vincent,X).
Q9) est_jaloux(vincent,_).


/consult graphe.dl

13)On peut difficilement tester combien de fois un cycle est traversé mais datalog mémorise ce qui a déjà été parcourru pour ne pas réemprunter le même chemin.

14)impaire(X,Y).

Partie 4 : SWI
commande swipl
[crisefinanciere].

evite(X,Y).
X = anne,
Y = barbara ;
X = barbara,
Y = cecile ;
X = cecile,
Y = barbara ;
X = anne,
Y = cecile ;
X = anne,
Y = barbara ; <- Déjà obtenu
X = anne,
Y = cecile ;
X = anne,
Y = barbara ;
X = anne,
Y = cecile ; <- Arrêt avec ENTER

DES> evite(X,Y)

{                                           
  evite(anne,barbara),
  evite(anne,cecile),
  evite(barbara,barbara),
  evite(barbara,cecile),
  evite(cecile,barbara),
  evite(cecile,cecile)
}
Info: 6 tuples computed.     

Parie 5 : Parsing
/[phrase1.dl, dictionnaire1.dl,grammaire1.dl].



