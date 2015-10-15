# TP2-AEL

alias cegrep='egrep --color=auto'

##Ex1
1. 
'''
$ cegrep 'nez' tdm2_fichiers/Cyrano.txt
Vous...vous avez un nez...heu...un nez...très grand.
En variant le ton, par exemple, tenez:
Agressif : " Moi, monsieur, si j'avais un tel nez,
Truculent : "çà, monsieur, lorsque vous pétunez,
La vapeur du tabac vous sort-elle du nez
Emphatique : " Aucun vent ne peux, nez magistral
Campagnard : " Hé, ardé! C'est-y un nez? Nanain!
" Le voilà donc ce nez qui des traits de son maître
'''

2. 
'''
$ cegrep '\(([[:alpha:]]|[[:space:]]|[[:punct:]])*\)' tdm2_fichiers/Cyrano.txt
(De Guiche)
(Le Vicomte)
(Cyrano)
(Le Vicomte)
(Cyrano)
(Le Vicomte)
(Cyrano)
'''

3. 
'''
cegrep '([[:space:]]|[[:punct:]])([àâéàêîùûÀÂÉÈÊÎÔÙÛ]|[[:alpha:]]){4}([[:space:]]|[[:punct:]]|$)|^(([àâéàêîùûÀÂÉÈÊÎÔÙÛ]|[[:alpha:]]){4})([[:space:]]|[[:punct:]]|$)' tdm2_fichiers/Cyrano.txt
Personne ne va donc lui répondre?
Attendez! Je vais lui lancer un de ces traits!...
Vous...vous avez un nez...heu...un nez...très grand.
Très.
C'est tout?...
Mais...
On pouvait dire...Oh! Dieu!...bien des choses en somme...
Amical : " Mais il doit tremper dans votre tasse!
Pour boire, faites-vous fabriquer un hanap! "
Curieux : " De quoi sert cette oblongue capsule?
Gracieux : " Aimez-vous à ce point les oiseaux
Que paternellement vous vous préoccupâtes
Truculent : "çà, monsieur, lorsque vous pétunez,
La vapeur du tabac vous sort-elle du nez
Sans qu'un voisin ne crie au feu de cheminée? "
Prévenant : " Gardez-vous, votre tête entraînée
De peur que sa couleur au soleil ne se fane! "
Pédant : " L'animal seul, monsieur, qu'Aristophane
Dut avoir sous le front tant de chair sur tant d'os! "
Cavalier : " Quoi, l'ami, ce croc est à la mode?
Pour pendre son chapeau, c'est vraiment très commode! "
Emphatique : " Aucun vent ne peux, nez magistral
T'enrhumer tout entier, excepté le mistral! "
Admiratif : " Pour un parfumeur, quel enseigne! "
Lyrique : " Est-ce une conque, êtes-vous un triton? "
Naïf : " Ce monument, quand le visite-t-on? "
Respectueux : " Souffrez, monsieur, qu'on vous salue,
Campagnard : " Hé, ardé! C'est-y un nez? Nanain!
C'est queuqu' navet géant ou bien queuqu'melon nain! "
Pratique : " Voulez-vous le mettre en loterie?
Assurément, monsieur, ce sera le gros lot! "
" Le voilà donc ce nez qui des traits de son maître
'''

4. Le 2ème vous ne corespond pas au pattern car l'espace les séparants est déjà pris dans le pattern du vous précedent.

5. 
'''
$ cegrep '^[[:upper:]]([[:lower:]])*[[:space:]]:[[:space:]]"' tdm2_fichiers/Cyrano.txt 
Agressif : " Moi, monsieur, si j'avais un tel nez,
Amical : " Mais il doit tremper dans votre tasse!
Descriptif : " C'est un roc!...c'est un pic!...c'est un cap!
Curieux : " De quoi sert cette oblongue capsule?
Gracieux : " Aimez-vous à ce point les oiseaux
Truculent : "çà, monsieur, lorsque vous pétunez,
Prévenant : " Gardez-vous, votre tête entraînée
Tendre : " Faites-lui faire un petit parasol
Pédant : " L'animal seul, monsieur, qu'Aristophane
Cavalier : " Quoi, l'ami, ce croc est à la mode?
Emphatique : " Aucun vent ne peux, nez magistral
Dramatique : " C'est la Mer Rouge quand il saigne! "
Admiratif : " Pour un parfumeur, quel enseigne! "
Lyrique : " Est-ce une conque, êtes-vous un triton? "
Naïf : " Ce monument, quand le visite-t-on? "
Respectueux : " Souffrez, monsieur, qu'on vous salue,
Campagnard : " Hé, ardé! C'est-y un nez? Nanain!
Militaire : " Pointez contre cavalerie! "
Pratique : " Voulez-vous le mettre en loterie?

$ cegrep -o '^[[:upper:]]([[:lower:]])*[[:space:]]:[[:space:]]"' tdm2_fichiers/Cyrano.txt 
Agressif : "
Amical : "
Descriptif : "
Curieux : "
Gracieux : "
Truculent : "
Prévenant : "
Tendre : "
Pédant : "
Cavalier : "
Emphatique : "
Dramatique : "
Admiratif : "
Lyrique : "
Naïf : "
Respectueux : "
Campagnard : "
Militaire : "
Pratique : "
'''


##Ex2
1. 
'''

'''

2. 
'''

'''

3. 
'''

'''


##Ex3
1. 
'''

'''

2. 
'''

'''

3. 
'''

'''

4. 
'''

'''


