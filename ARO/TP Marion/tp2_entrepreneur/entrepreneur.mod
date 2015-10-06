set OBJ;

param poids_max >= 0;
param volume_max >= 0;
param val {OBJ} >= 0;
param poids {OBJ} >= 0;
param volume {OBJ} >= 0;
param dispo {OBJ} >= 0;

var nb_objet {o in OBJ};

maximize profit :
sum {o in OBJ}
    val[o]*nb_objet[o];

subject to poids_limit :
sum {o in OBJ}
    poids[o]*nb_objet[o] <= poids_max;

subject to volume_limite :
sum {o in OBJ}
    volume[o]*nb_objet[o] <= volume_max;
