''Marion Noirbent
ARO TP2
Lanceur du modele de campagne de publicité''

##Modèle de base
'''
ampl: model publicite
MINOS 5.51: optimal solution found.
1 iterations, objective 9.8e+07

ampl: display touched, m_achat;
touched = 9.8e+07

m_achat [*] :=
mag  80
 tv  10
;
'''

##Modèle avec personnel
'''
ampl: model publicite
MINOS 5.51: optimal solution found.
2 iterations, objective 9.2e+07
touched = 9.2e+07

m_achat [*] :=
mag  20
 tv  40
;
'''

##Modèle avec radio
'''
ampl: model publicite;
MINOS 5.51: optimal solution found.
2 iterations, objective 9.8e+07
touched = 9.8e+07

m_achat [*] :=
  mag    0
radio  400
   tv   10
;
'''

##Modèle avec minimum
On change la donnée use_min d=associé au média dans le fichier .dat, on ne modifie pas le model.

###mag
'''
ampl: model publicite;
MINOS 5.51: optimal solution found.
2 iterations, objective 9.8e+07
touched = 9.8e+07

m_achat [*] :=
  mag    2
radio  390
   tv   10
;
'''
###radio
(remize à 0 du use_min de mag)
'''
ampl: model publicite;
MINOS 5.51: optimal solution found.
2 iterations, objective 9.8e+07
touched = 9.8e+07

m_achat [*] :=
  mag    0
radio  400
   tv   10
;
'''
