Marion Noirbent
TP1 BDD Boutique

##Exo 1 du TD
1. 
'''
RA> project[fnom](fournisseurs);
temp0(FNOM:VARCHAR)

Number of tuples = 5
kiventout:
Big Red Tool and Die:
Perfunctory Parts:
Alien Aircaft Inc.:
Autolux:
'''

2. 
'''
RA> select[fad='Paris'](fournisseurs);
temp0(FID:INTEGER,FNOM:VARCHAR,FAD:VARCHAR)

Number of tuples = 1
1:kiventout:Paris:
'''

3. 
'''
RA> project[fnom](select[fad='Paris'](fournisseurs));
temp1(FNOM:VARCHAR)

Number of tuples = 1
kiventout:
'''

4. 
'''
RA> project[anom](select[acoul='vert'](articles));
temp1(ANOM:VARCHAR)

Number of tuples = 2
7 Segment Display:
Microsd Card USB Reader:
'''

5. 
'''
RA> project[aid](select[prix<=20](catalogue));
temp1(AID:INTEGER)

Number of tuples = 5
3:
8:
1:
7:
9:
'''

6. 
'''
RA> project[fid](select[prix<=20](catalogue));
temp1(FID:INTEGER)

Number of tuples = 3
1:
2:
3:
'''

7. 
'''
RA> project[fid](select[prix<=20 and prix>=10](catalogue));
temp1(FID:INTEGER)

Number of tuples = 3
1:
2:
3:
'''

8. 
'''
RA> project[fid](select[prix<=20](catalogue) intersect select[prix>=10](catalogue));
temp3(FID:INTEGER)

Number of tuples = 3
1:
2:
3:
'''

9. NB : Le fichier de réponse présente les identifiants alors que le poly demande les noms.
'''
RA> project[anom](select[acoul='vert'](articles) union select[acoul='rouge'](articles));
temp3(ANOM:VARCHAR)

Number of tuples = 6
7 Segment Display:
Microsd Card USB Reader:
Left Handed Toaster Cover:
Acme Widget Washer:
Fire Hydrant Cap:
Ferrari:
'''

10. Imprécision du français -> même requête que 9


##Exo 3 du TD

1. 
'''
RA> project[anom](catalogue join articles);
temp1(ANOM:VARCHAR)

Number of tuples = 8
Left Handed Toaster Cover:
Smoke Shifter End:
Acme Widget Washer:
Brake for Crop Circles Sticker:
Anti-Gravity Turbine Generator:
Fire Hydrant Cap:
7 Segment Display:
Ferrari:
'''

2. 
'''
RA> project[anom, prix, fnom](catalogue join articles join fournisseurs);
temp2(ANOM:VARCHAR,PRIX:DECIMAL,FNOM:VARCHAR)

Number of tuples = 17
Left Handed Toaster Cover:36.1:kiventout:
Smoke Shifter End:42.3:kiventout:
Acme Widget Washer:15.3:kiventout:
Acme Widget Washer:20.5:kiventout:
Brake for Crop Circles Sticker:20.5:kiventout:
Anti-Gravity Turbine Generator:124.23:kiventout:
Fire Hydrant Cap:11.7:kiventout:
7 Segment Display:75.2:kiventout:
Left Handed Toaster Cover:16.5:Big Red Tool and Die:
Anti-Gravity Turbine Generator:0.55:Big Red Tool and Die:
Fire Hydrant Cap:7.95:Big Red Tool and Die:
Fire Hydrant Cap:12.5:Perfunctory Parts:
7 Segment Display:1.0:Perfunctory Parts:
Acme Widget Washer:57.3:Alien Aircaft Inc.:
Brake for Crop Circles Sticker:22.2:Alien Aircaft Inc.:
Fire Hydrant Cap:48.6:Alien Aircaft Inc.:
Ferrari:234555.67:Autolux:
'''

3. 
'''
RA> project[fid](select[acoul='rouge'](catalogue join articles));
temp2(FID:INTEGER)

Number of tuples = 5
1:
2:
3:
4:
5:
'''

4. 
'''
RA> project[fnom](select[prix<=20](catalogue join fournisseurs));
temp2(FNOM:VARCHAR)

Number of tuples = 3
kiventout:
Big Red Tool and Die:
Perfunctory Parts:
'''

5. 
'''
RA> project[fid](catalogue) minus project[fid](select[prix<10000](catalogue));
temp3(FID:INTEGER)

Number of tuples = 1
5:
'''

6. 
'''
RA> project[fnom, fad](fournisseurs join (project[fid](catalogue) minus project[fid](select[prix<10000](catalogue))));
temp5(FNOM:VARCHAR,FAD:VARCHAR)

Number of tuples = 1
Autolux:Milano:
'''

7. 
'''
RA> project[aid](select[acoul='vert'](articles)) times project[aid](select[acoul='rouge'](articles));
temp4(temp1.AID:INTEGER,temp3.AID:INTEGER)

Number of tuples = 8
9:1:
9:3:
9:8:
9:11:
10:1:
10:3:
10:8:
10:11:
'''

8. 
'''
RA> project[aid](articles) minus project[aid](catalogue);
temp2(AID:INTEGER)

Number of tuples = 1
10:
'''

9. 
'''
RA> project[anom](articles join (project[aid](articles) minus project[aid](catalogue)));
temp4(ANOM:VARCHAR)

Number of tuples = 1
Microsd Card USB Reader:
'''

10. 
'''
RA> project[fid](select[acoul='vert'](articles join catalogue)) intersect project[fid](select[acoul='rouge'](articles join catalogue));
temp6(FID:INTEGER)

Number of tuples = 2
1:
3:
'''

11. 
'''
RA> project[fnom](select[acoul='noir'](articles) join catalogue join fournisseurs);
temp3(FNOM:VARCHAR)

Number of tuples = 1
kiventout:
'''

12. NB : Le fichier de réponse présente les identifiants des fournisseurs alors que le poly demande ceux des articles.
'''
RA> project[aid](select[aid=aid2 and fid<>fid2](catalogue times rename[fid2, aid2, prix](catalogue)));
temp3(AID:INTEGER)

Number of tuples = 6
1:
4:
5:
7:
8:
9:
'''

Pour vérifier, version avec les deux identifiants :
'''
RA> project[aid, fid](select[aid=aid2 and fid<>fid2](catalogue times rename[fid2, aid2, prix](catalogue)));
temp3(AID:INTEGER,FID:INTEGER)

Number of tuples = 14
1:1:
4:1:
5:1:
7:1:
8:1:
9:1:
1:2:
7:2:
8:2:
8:3:
9:3:
4:4:
5:4:
8:4:
'''

13. NB : Le fichier de réponse présente les identifiants et les noms alors que le poly demande uniquement les noms.
'''
RA> project[fnom](fournisseurs) minus (project[fnom]((select[acoul='noir'](articles) union select[acoul='argente'](articles)) join catalogue join fournisseurs));
temp7(FNOM:VARCHAR)

Number of tuples = 3
Big Red Tool and Die:
Perfunctory Parts:
Autolux:
'''


