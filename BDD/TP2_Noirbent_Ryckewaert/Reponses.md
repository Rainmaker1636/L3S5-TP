#TP2 BDD : Création de base et premières requêtes en SQL
##Ex1
1. Après l'ajout des clé primaires, ils n'est plus possible d'avoir de doublons dans la table.
2. 



##Ex2
1. select fnom as fournisseur from Fournisseurs;
2. select fnom from Fournisseurs where fnom='Paris';
3. select fnom from Fournisseurs,Catalogue where (Catalogue.fid=Fournisseurs.fid) and (prix<20);
4. 
 *select fid from Catalogue where prix>10 AND prix<20 group by fid;
 *(select fid from Catalogue where prix>10 group by fid) INTERSECT (select fid from Catalogue where prix<20 group by fid);
5. ((select anom from Articles where acoul='rouge') UNION (select anom from Articles where acoul='vert')) INTERSECT (select anom from Articles,Catalogue where Articles.aid=Catalogue.aid AND prix<20);
6. 
 *select anom from Articles,Catalogue where Articles.aid=Catalogue.aid;
 *select anom from Articles JOIN Catalogue on Articles.aid=Catalogue.aid;
 *
7.
