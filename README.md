# L3S5-TP
AeL, PDS, ARO, autres...
Glossaires de l’ingénierie des besoins
A=Acteur
CU=Cas d'Utilisateur
C=Class

| Élement                  |Type |Définition                                                                                                                                                                               |
|----------------------------|-----|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Visiteur|A|Un visiteur peut consulter les annonces de MaBrocante|
| Membre |A|Un membre est une personne inscrite sur le site, il peut accéder aux fonctionnalités avancées telles que la création d'une annonce ou l'envoi de messages|
| Responsable  | A|Le responsable aura les mêmes fonctionnalités qu'un modérateur mais pourra définir qui est modérateur, et pourra ajouter/modifier/supprimer des catégories de produits|
|Modérateur|A|Le modérateur doit valider les annonces et les modérer|
| Modérer l'anonce |CU|Modérateur publie l’annonce|
| Contacter membre |CU|Membre contacte les autres membres par son boîte|
| Rechercher annonce |CU|Visiteur emplir les champs et le site afficher les résultats de recherche|
| Créer annonce |CU|Membre remplir les champs et créer un annonce|
| Annonces premium |CU|Administrateur met un annonce payé en avant |
| Supprimer annonce |CU|Membre ou modérateur supprimer un annonce de database|
| Modifier annonce |CU|Membre modifie l’état de son compte|
| Gérer catégorie |CU|Administrateur ajoute nouvel catégorie|
| Inscription |CU|Permet de remplir les informations demandés et de devenir un membre|
| Connection      |CU|Permet de se connecté à son membre compte|
| Gérer profil        |CU|Membre modifie l’état de son compte|
| Signaler annonce    |CU|Membre signale une annonce contrefaçon|
| Signaler message    |CU|Administrateur vérifie l’annonce signalé par membre|
| Vérifier annonce    |CU|Membre ne souhaite plus recevoir de quelques membres| 
| Laisser commentaire |CU|Permet à les membres de laisser commentaire sous l ‘annonce et noter son vendeur|
| Membre |C| Le compte du visiteur s’inscrit a des fonctions de gérer son profil , de gérer annonce et de contacter autres membre ou modérateur| 
| Modérateur|C|Le modérateur a une adresse mail, un nom d'utilisateur et un mot de passe et a accès au fonctions de gestion telles que la modération des annonces et des messages|
| Annonce |C|Une annonce contient des champs en fonction de sa catégorie, un titre, une description, un prix ainsi que des photos|
| Message |C|Le message envoyé entre les membres par leur profils|
| Catégorie|C|Une catégorie a un nom, et des champs de différents types qui permettront une création d'annonce et une recherche plus affinée |
| Demande |C|Sous class de Annonce décrivant une annonce à request|
| Offre |C|Sous class de Annonce décrivant une annonce à vendre|

