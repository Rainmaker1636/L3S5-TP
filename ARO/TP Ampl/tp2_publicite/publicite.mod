set MEDIA;

param budget >= 0;
param pers >= 0;
param prix {MEDIA} >= 0;
param use_min {MEDIA} >= 0;
param conso_t {MEDIA} >= 0;
param pers_sem {MEDIA} >= 0;

var m_achat {m in MEDIA} >= use_min[m];

maximize touched :
sum {m in MEDIA}
    conso_t[m]*m_achat[m];

subject to budget_limit :
sum {m in MEDIA}
    prix[m]*m_achat[m] <= budget;

subject to pers_dispo :
sum {m in MEDIA}
    pers_sem[m]*m_achat[m] <= pers;
