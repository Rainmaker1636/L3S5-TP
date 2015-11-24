set PAPIER;
set USINE;

param cost_prod {USINE} >= 0;
param prod_min {PAPIER} >= 0;
param cap_prod {USINE, PAPIER} >= 0;

var nb_jours {u in USINE};

minimize cost :
sum {u in USINE}
cost_prod[u]*nb_jours[u];

subject to papier_prod{p in PAPIER} :
sum {u in USINE}
nb_jours[u]*cap_prod[u, p] >= prod_min[p];

