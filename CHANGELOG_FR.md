# Historique des versions

## v1.3.0 - 2026-09-20

- Ajout public de l’édition, estimation et diagnostic CFA, SEM et PLS-SEM/PLSc, avec bootstrap et comparaisons de groupes compatibles. La survie propose Kaplan–Meier, RMST, Cox et les risques concurrents compatibles.

- Ajout des exports publics PDF, Word et Excel. La couverture suit la langue de l’interface ; Free affiche logo et institution. HTML/PDF ajoutent les figures des quatre analyses de modèles en fin de rapport, avec la disposition affichée. Les images Free utilisent 300 dpi et un fond transparent ou blanc.

- Le flux personnalisé devient Effets de médiation/modération et les menus de régression redondants sont supprimés. Commandes et exécution sont séparées ; enregistrement, position des dialogues et icônes sont améliorés. Les quatre analyses partagent les chemins pointillés pour p valide ≥ .05.

- Amélioration des tableaux portrait, graphiques résiduels sur une page, notes selon les indicateurs affichés et notes sur la dernière table de régression hiérarchique. Uniformisation des polices et des retours des en-têtes HTML. Analyses, Notes méthodologiques et Historique sont disponibles dans les huit langues de l’interface.

- L’installateur public exclut la méta-analyse et l’ANOVA des traitements répétés sur les mêmes sujets. Pro est prévu ultérieurement ; développement/Pro suit une politique de 600 dpi. La validation est présentée par version, cas et preuves.

- Préparation de la documentation du développement 1.3.0: six rubriques reliées dans huit langues et anciennes consignes d’export mises à jour.

- Documentation de la sélection et segmentation, des résultats cumulés, de HWPX direct en coréen, du choix de contenu Word/HWPX et de la couverture et liste de tableaux HTML.

- Ajout d’IPA: évaluations directes/importance dérivée, ensemble/groupes indépendants/avant-après apparié, références, graphiques et inférence compatible, avec documentation et export en huit langues.

## v1.2.0 - 2026-08-06

### Ajouts

- Ajout de l'accord inter-évaluateurs aux analyses publiques, avec priorité à l'indice d'accord recommandé tout en conservant les indices complémentaires.
- Ajout d'une ANOVA mixte à mesures répétées pour les comparaisons pré/post et multitemporelles entre groupes, avec parcours PP/ITT, résumés ajustés sur les covariables, examen des hypothèses, comparaisons post hoc et exportations HTML/PDF/Excel.
- Ajout du canevas de modèle personnalisé de médiation/modération au groupe public Régression / Modèles.

### Modifications

- Promotion des travaux stabilisés après 1.1.3 dans les métadonnées officielles de la version `1.2.0`.
- Intégration de l'ANOVA à mesures répétées dans Comparaison de groupes et harmonisation de la configuration, de l'examen des hypothèses, des contrôles de sphéricité/Levene, des conseils de normalité et des recommandations avec l'analyse guidée de StatEdu Studio.
- Extension de la validation statistique : corrélation, fidélité, accord inter-évaluateurs, analyse factorielle / PCA, test t / ANOVA, mesures répétées appariées, régression, logistique, longitudinal / panel, régression pénalisée, taille d'échantillon, taille d'effet, éditeur de données, modèles personnalisés et mesures répétées mixtes.
- Renommage du menu coréen des modèles personnalisés pour clarifier la navigation publique.

### Corrections

- Correction du lanceur public Electron empaqueté pour activer par défaut le canevas de modèle personnalisé de médiation/modération dans l'installateur 1.2.0.
- Suppression des lignes de résultats dupliquées lors de la réutilisation de modèles personnalisés ajustés.

## v1.1.3 - 2026-07-12

### Modifications

- Promotion de la version de développement stabilisée dans l'installateur officiel `1.1.3`.
- Maintien du périmètre public conforme à la règle d'empaquetage 1.1.1, en excluant documentation, tests, exemples, sources et autres contenus non nécessaires à l'exécution du runtime R inclus.
- Ajout de traductions pour les nouveaux libellés de dossiers de projets d'analyse latente, indications de fichiers de données, séparateurs DAT et commandes d'analyse communes.

### Corrections

- Correction de l'importation Excel pour maintenir les fichiers sélectionnés interactifs et les charger via l'examen feuille/cellule initiale.
- Limitation des aperçus partagés « Voir les données sélectionnées » aux variables sélectionnées et à 15 lignes.
- Correction des dossiers de projet/sortie Latent Mplus afin de créer les résultats à côté du fichier de données initial lorsque son chemin est disponible.
- Ouverture d'une boîte de dialogue lors de l'enregistrement des paramètres Latent Mplus pour choisir le nom du fichier.
- Alignement à gauche de l'affichage du dossier de projet/sortie Latent Mplus.
- Passage des derniers libellés directs de l'interface par la table i18n commune pour traduire les nouvelles options de données, régression, médiation, modération et analyse latente.
- Maintien de l'exclusion du canevas personnalisé de médiation/modération dans la version officielle.

## v1.1.1 - 2026-07-07

### Corrections

- Publication d'un installateur correctif afin que les mises à niveau Windows remplacent les fichiers empaquetés plutôt que de réutiliser une ancienne installation de même version.
- Renforcement du diagnostic de démarrage Electron, avec délai Shiny prolongé et enregistrement de la sortie du processus R en cas d'échec.
- Correction du chargement des données de bureau, du choix initial du coréen, des fichiers de plan d'échantillonnage complexe et de la conservation des libellés vides.

## v1.1.0 - 2026-07-06

### Ajouts

- Publication de la version publique 1.1.0 avec toutes les analyses sauf le canevas personnalisé de médiation/modération.
- Ajout de restrictions publiques d'enregistrement : HTML actif par défaut ; figures, PDF, Excel et Ajouter un résultat restent visibles mais désactivés.
- Maintien de l'exception publique test t / ANOVA : HTML, figures, PDF, Excel et Ajouter un résultat actifs ; ses collections de résultats peuvent exporter Excel et Word.

### Modifications

- Mise à jour des profils Electron pour produire des installateurs publics StatEdu Studio à partir des versions sémantiques finales.

## v1.0.1 - 2026-06-28

### Modifications

- Stabilisation du changement coréen/anglais dans les menus, configurations, calculateurs, documents et notifications, avec tableaux de résultats conservés en anglais.
- Mise à jour de l'Éditeur de données et des menus d'analyse : catégories regroupées, libellés coréens corrigés et boutons/onglets alignés.
- Amélioration des seuils de normalité test t / ANOVA, des résumés ordonnés de repères post hoc et de la présentation/exportation des tableaux croisés.
- Ajout de commandes du nom de variable produit par les calculateurs et amélioration des panneaux EQ-5D et ASCVD10.
- Ajout de la recherche de mises à jour, des métadonnées d'association `.studio` et de l'icône de fichier `.studio`.
- Ajout d'Aide pour signaler des bogues, demander des fonctions ou analyses, consulter les questions-réponses et rechercher des mises à jour.
- Liaison des demandes d'Aide aux formulaires web StatEdu Studio et orientation des questions-réponses selon la langue de l'interface.
- Mise à jour des captures du guide 1.0 et des ressources documentaires bilingues.

## v1.0.0 - 2026-06-25

### Modifications

- Promotion de la branche stabilisée dans les métadonnées publiques 1.0.0.
- Remplacement des noms bêta Electron par les noms définitifs StatEdu Studio.
- Maintien explicite dans les documents des annonces publiques 1.0 différées, des vérifications DOI/site web et des contrôles qualité de l'application empaquetée.
- Mise à jour des repères post hoc ordonnés pour montrer uniquement les comparaisons directement significatives dans l'ordre des moyennes, sans chaîne transitive telle que `b>a>c` lorsqu'une paire n'est pas significative.
- Ajout d'une orientation PDF selon la largeur des tableaux croisés : tableaux principaux larges en paysage, tableaux étroits ou complémentaires en portrait.
- Ajout de seuils asymétrie/aplatissement sélectionnables pour la normalité test t / ANOVA : 2/5, 2/7 et 3/7, avec 2/7 par défaut.
- Ouverture de la boîte d'enregistrement des paramètres dans le dossier du fichier de données chargé lorsque son chemin est disponible.
- Suppression de l'examen d'importation Excel au démarrage sauf si un chemin valide de fichier Excel est en attente.

## v0.9.42 - 2026-06-23

### Ajouts

- Ajout d'Éditeur de données > Large vers long pour transformer les colonnes de mesures répétées avant l'analyse longitudinale / panel.
- Ajout de validations de transformation large-long et extension des contrôles de recodage.
- Ajout de tests de démarrage Shiny et de fonctionnement élémentaire de la version Electron candidate.
- Ajout d'une validation UTF-8 des documents suivis pour les versions candidates.

### Modifications

- Uniformisation des panneaux, boutons, espacements et visualiseurs de l'Éditeur de données selon le dialogue test t / ANOVA.
- Prise en charge du marquage des valeurs manquantes utilisateur et de leur conversion en NA système.
- Amélioration du recodage et du renommage avec retrait des variables en attente, panneaux cibles alignés et règles plus claires.
- Simplification des paramètres au format `.studio` et mise à jour des dialogues d'enregistrement/chargement.
- Amélioration de la présentation longitudinale / panel et des catégories de référence des prédicteurs catégoriels.
- Renforcement des contrôles de propreté de publication pour fichiers locaux, artefacts générés, métadonnées de version et documentation.
- Restauration du plan produit coréen avec les priorités actuelles de stabilisation 1.0.
- Mise à jour des références des documents coréens utilisateur et méthodes vers 0.9.42, avec détection des références de version courante périmées.
- Marquage du plan distribution/licences/mises à jour 1.0 comme examiné pour la stabilisation 0.9.42.
- Ajout des contrôles de propreté à la suite centrale de stabilisation pour empêcher le suivi accidentel des fichiers préparatoires Electron et artefacts locaux.
- Ajout du suivi et de la validation de préparation de publication : empaquetage, DOI, site web et reports 1.0.
- Ajout d'un script préalable exécutant validation complète de stabilisation, test de démarrage Shiny et test élémentaire de publication Electron.
- Ajout d'un registre des décisions 1.0 : empaquetage, DOI, site web, restrictions d'édition, licences, mises à jour et notes publiques.
- Renforcement du contrat d'interface et des contrôles de placement standard des boutons dans les trois blocs de l'Éditeur de données.
- Ajout d'un protocole qualité manuel pour l'apparence, les données, analyses, exportations et Electron empaqueté des versions candidates.
- Liaison du protocole manuel aux tests élémentaires de publication Electron.
- Mise à jour de l'état de préparation pour consigner la réussite de la vérification préalable.
- Renforcement de la validation des métadonnées concernant les obstacles publics 1.0 non résolus.
- Ajout d'un modèle de compte rendu qualité manuel et de validation des preuves des versions candidates.
- Retrait du suivi des artefacts comparatifs générés dans `outputs/` et interdiction des artefacts de sortie à la racine lors des contrôles de propreté.
- Documentation de la commande complète de vérification préalable Electron empaqueté dans README et la validation des métadonnées.
- Précision dans les conditions de publication que l'URL cible du DOI est `https://studio.statedu.com`.
- Précision que le plan distribution/licences/mises à jour 1.0 est un document de planification, sans affirmer l'implémentation des éditions restreintes, de l'activation, de l'actualiseur ou de l'infrastructure publique d'installation.
- Ajout d'un avertissement README/préparation : le DOI prévu doit être résolu avant toute annonce publique de citation 1.0.
- Renforcement de la validation des avis sources/licences : disponibilité publique du code, avis tiers, rapports de licences et références aux textes inclus.
- Harmonisation de la liste de publication, du README et du guide qualité manuel pour conserver les preuves avec les notes et artefacts de validation.
- Documentation du remplacement obligatoire des noms bêta Electron 0.9.x avant l'installateur public 1.0.
- Harmonisation des dialogues Latent Mplus avec l'usage exclusif des paramètres `.studio`.
- Mise à jour du plan de distribution, de licences et de mises à jour de la version 1.0 pour que les notes bêta historiques ne paraissent pas décrire la version actuelle.
- Ajout d'un contrôle qualité manuel interdisant aux notes publiques et textes visibles d'annoncer des éditions restreintes, activation de licences, mises à jour intégrées ou infrastructure publique non implémentées.
- Remplacement des notifications provisoires du générateur Latent Mplus par des messages explicites de fonction non activée dans cette version et leur validation.
- Reformulation du sous-titre de repli de taille d'effet pour ne pas présenter les calculateurs indisponibles comme des promesses futures.
- Suppression d'un auxiliaire inutilisé d'onglet provisoire Analyse dans l'assemblage des menus.
- Précision de la liste de publication : anciens formats de paramètres absents des dialogues publics, identifiants internes de compatibilité toujours documentés.

## v0.9.41 - 2026-06-20

### Modifications

- Regroupement d'Analyse, Taille d'échantillon et Taille d'effet en catégories statistiques cohérentes de premier niveau.
- Correction des transferts de tableaux croisés pour renvoyer fiablement les variables de colonne ou ligne dans la liste disponible.
- Ajustement des boutons de transfert des panneaux de colonnes et de lignes des tableaux croisés.

## v0.9.40 - 2026-06-20

### Modifications

- Incrémentation des métadonnées de développement après la bêta 0.9.39.
- Adoption de **StatEdu Studio** pour les éléments publics : en-tête, À propos, lanceur, installateur, favicon, logos et noms d'exportation par défaut.
- Ajout de contrôles de compatibilité de marque documentant les anciens identifiants conservés pour DOI, environnement, chemins ou recherche rétrocompatible de données.
- Protection des appels Shiny au démarrage du client contre les erreurs précoces `Shiny.setInputValue` avant disponibilité de la liaison cliente.
- Mise à jour du verrouillage d'audit Electron afin de résoudre la dépendance transitive de développement `undici` sans alerte npm audit.
- Ajout des métadonnées d'auteur Electron et exclusion des archives `StatEdu_Studio_*.zip` du développement local et de la préparation Electron.

## v0.9.39 - 2026-06-18

### Ajouts

- Ajout du parcours indépendant `Analysis > Longitudinal / Panel Models` pour GEE, LMM, GLMM, effets fixes et effets aléatoires de panel.
- Ajout de contrôles d'hypothèses propres au modèle, alternatives recommandées, comparaisons automatiques de sensibilité, estimations publiables, texte de manuscrit, liste SCI et versions logicielles aux résultats longitudinaux / panel.
- Ajout d'un onglet Données manquantes longitudinal / panel avec traitement principal, moteurs effectifs MI/IPW/WGEE de sensibilité et suivi de la méthode dans le rapport.
- Ajout de poids longitudinaux : une variable cible, types échantillonnage/longitudinal/IPW/combiné, troncature, normalisation finale et taille effective d'échantillon.
- Ajout d'exposition / offset facultatif pour les modèles longitudinaux de comptage/taux, avec offsets `log(exposure)` dans les ajustements principaux et de sensibilité.
- Ajout de détails de détection d'inflation de zéros comparant proportions observées et attendues sous Poisson.
- Ajout du parcours actif `Analysis > GLM` pour GLM gaussiens, logistiques binaires, Gamma et de comptage, réutilisant la sélection GEE Poisson/binomiale négative.
- Ajout d'informations GLM orientées SCI : cas complets, choix Poisson/binomiale négative, EPV/séparation/cellules rares logistiques, indépendance, influence, notes de publication, listes de vérification et texte de manuscrit suggéré.
- Ajout d'options GLM par onglets, dont Données manquantes pour cas complets, imputation multiple et pondération par probabilité inverse.
- Ajout de documentation GLM dans le Guide utilisateur, les Méthodes d'analyse et les Notes méthodologiques coréens : famille/lien, sensibilité aux données manquantes, erreurs-types robustes, surdispersion et présentation SCI.
- Ajout de HTML, PDF, Excel et collections de résultats enregistrées pour GLM, avec notes de publication, listes SCI, texte de manuscrit et feuilles des versions logicielles.
- Ajout de HTML, PDF, Excel et collections enregistrées pour les résultats longitudinaux / panel.
- Extension de la validation aux ajustements longitudinaux / panel et GLM, interfaces, catalogues d'hypothèses, exportations HTML/Excel, sensibilité et sections SCI.

### Modifications

- Harmonisation de la configuration longitudinale / panel avec les transferts de test t / ANOVA et affichage conditionnel des options pertinentes.
- Fusion des options Modèle et Termes longitudinales / panel pour régler type, termes temporels fixes et effets aléatoires dans un seul onglet.
- Corrélation de travail GEE échangeable par défaut ; les ajustements AR(1) transmettent désormais l'ordre sujet/temps à `geepack::geeglm`.
- Renommage des ajustements GEE binomiaux négatifs en GLM marginal binomial négatif avec erreurs-types robustes regroupées par sujet, geepack ne fournissant pas de GEE binomial négatif natif.
- Précision que l'ID de groupe facultatif définit un regroupement supplémentaire d'intercepts aléatoires LMM/GLMM et ne s'applique pas à l'ajustement principal GEE/panel choisi.
- Précision du traitement LMM/GLMM par vraisemblance sous MAR avec mesures disponibles, MI/IPW restant des analyses de sensibilité plutôt que l'ajustement principal par défaut.
- Exclusion des modèles de comptage à inflation de zéros et hurdle du module longitudinal / panel par défaut pour éviter de lourdes dépendances facultatives ; l'excès de zéros est signalé comme aide au dépistage.
- Remplacement de l'ébauche Généralisé par GLM fonctionnel : configuration, exécution, coefficients, ajustement, erreurs-types robustes, surdispersion et VIF facultatif.
- Harmonisation de `run_app.R` et `R/app_bootstrap.R` pour partager la liste des paquets requis entre installation au lancement et exécution.
- Fixation d'Electron à 39.8.6 et actualisation du verrouillage après audit des paquets.
- Renforcement du script bêta Electron pour trouver Rscript hors PATH et tolérer l'absence du module facultatif Latent Mplus exclu du paquet.
- Mise à jour du README et des Guide utilisateur, Méthodes d'analyse et Notes méthodologiques coréens pour les nouveaux parcours longitudinal / panel et GLM.

## v0.9.38 - 2026-06-15

### Ajouts

- Ajout des estimations empiriques de taille d'échantillon de médiation de Fritz & MacKinnon (2007) pour une puissance de .80.
- Ajout de références propres aux calculs de médiation Fritz & MacKinnon, Monte Carlo, bootstrap et Sobel.

### Modifications

- Élargissement du bloc de résultats Taille d'échantillon dans les grandes fenêtres, avec conservation des trois blocs à 1280 px de largeur.

## v0.9.37 - 2026-06-12

### Ajouts

- Ajout de dictionnaires Likert coréens et anglais à 4 points correspondant aux dictionnaires à 5 points existants.
- Ajout d'un tutoriel animé avec superpositions d'actions au guide coréen intégré, utilisant les images incluses.

### Modifications

- Amélioration des dictionnaires personnalisés Likert : ouverture par bouton, liste des dictionnaires enregistrés et détails sélectionnés dans un panneau latéral.
- Priorité aux correspondances exactes de niveaux sur les surensembles compatibles, pour reconnaître les réponses à 4 points comme telles.
- Ajustement du minutage et des positions des superpositions du guide pour chargement, test t / ANOVA et examen des résultats.

### Corrections

- Conservation du défilement lors de l'ouverture ou de la sélection des dictionnaires Likert enregistrés.
- Amélioration des largeurs des colonnes de sélection et de nom de détection Likert.

## v0.9.36 - 2026-06-11

### Ajouts

- Ajout d'un gestionnaire modifiable de dictionnaires Likert personnalisés : examen, détails, modification et suppression.
- Extension de la validation à la logistique, au recodage, à l'analyse factorielle/PCA, aux corrélations, tests appariés, E/S de données et historique des résultats.

### Modifications

- Amélioration des tableaux orientés B5 en logistique, analyse factorielle, PCA, fidélité, tests appariés/répétés, corrélation et résultats enregistrés.
- Amélioration de l'importation Excel en déplaçant l'aperçu de feuille dans le panneau principal et en simplifiant les commandes.
- Attribution aux variables Likert converties du type de mesure demandé après conversion.

### Corrections

- Correction du déplacement des boutons de sélection des listes de l'Éditeur de données après importation Excel.
- Correction de l'actualisation de la détection automatique des valeurs manquantes et Likert après conversion/importation.
- Correction du placement des tableaux, références, affichage VIF et options d'intervalles de confiance en logistique hiérarchique.
- Correction de l'ordre des colonnes de charges factorielles/PCA et des en-têtes compacts en B5 portrait.

## v0.9.35 - 2026-06-10

### Ajouts

- Ajout du parcours développeur Latent Mplus comme module EasyFlow facultatif, avec étapes Données, Configuration et Résultats.
- Ajout de l'enregistrement/chargement des rôles latents, des conditions de sous-ensemble, de la conservation de l'ordre choisi et de la consultation du progrès dans Résultats.
- Acheminement des sorties latentes sous le dossier des données chargées : résultats, fichiers temporaires Mplus, journaux, tableaux Excel et figures de 600 dpi.
- Ajout de l'affichage de graphiques natifs Mplus sélectionnés et de variantes colorées des profils d'indicateurs.

### Modifications

- Harmonisation des tableaux et figures latents avec un cadre B5 : alignement à gauche, tableaux compacts et figures mises à l'échelle B5 portrait.
- Amélioration de la séquence chargement/paramètres/réinitialisation de Données et report de l'enregistrement du serveur latent jusqu'à l'ouverture d'un onglet latent.
- Masquage des tableaux propres à LCA pour les résultats LPA et retrait des tables internes de clés de classes BCH de Résultats.
- Mise à jour de l'empaquetage bêta Electron pour exclure Latent Mplus, réservé au développement, de la préparation publique.

### Corrections

- Correction de l'examen des hypothèses et des tableaux d'aperçu test t / ANOVA afin de renseigner les contrôles d'hypothèses.
- Réinitialisation des rôles/résultats latents au chargement d'un nouveau fichier, tout en conservant les paramètres YAML explicitement restaurés.
- Conservation du défilement de la table des variables latentes lors de l'affectation des rôles.
- Déplacement des messages de progression latente de Configuration vers Résultats pendant l'analyse.

## v0.9.34 - 2026-06-09

### Modifications

- Amélioration des tableaux appariés et répétés : options de résumé, alignement statistique, libellés de tailles d'effet, avertissements et examen des hypothèses.
- Correction de l'ordre des variables répétées appariées pour respecter la sélection de l'utilisateur.
- Conservation des onglets d'options appariées visibles, avec libellés de variables répétées actifs uniquement à partir de trois mesures.
- Clarification des notes de mesures répétées pour ne pas présenter lambda de Wilks et Greenhouse-Geisser comme une méthode combinée.
- Mise à jour des métadonnées de citation avec le DOI enregistré d'EasyFlow Statistics.

## v0.9.33 - 2026-06-06

### Modifications

- Extension des diagnostics ANCOVA : Levene par défaut, Brown-Forsythe / Breusch-Pagan / White facultatifs, détails d'homogénéité des pentes, cas complets, graphiques de linéarité résiduelle et sensibilité à l'influence.
- Ajout de commandes ANCOVA permettant de conserver la sélection automatique ou d'afficher les avertissements tout en gardant le modèle standard.
- Amélioration des tableaux communs : police 9 pt, largeur fixe portrait/paysage, aperçu écran à 1.5x, repères post hoc communs, libellés ES et en-têtes post hoc sur deux lignes.
- Réorganisation des options ANCOVA Hypothèses / Modèle / Sortie et harmonisation des espacements, retraits et notes.

## v0.9.32 - 2026-06-03

### Modifications

- Amélioration de la présentation ANCOVA, notamment gestion plus précise de la largeur et statistiques de test alignées à droite.
- Ajout de conversions LMM de style SPSS : eta carré partiel depuis F/dl global et dz par paire fondé sur la covariance.
- Ajout de conversions de taille d'effet GLMM pour effets fixes logit binaires, comptages à lien logarithmique et résultats gaussiens.
- Mise à jour du Guide utilisateur, des Méthodes d'analyse et des Notes méthodologiques coréens pour les tailles d'effet ANCOVA, LMM, GEE et GLMM.
- Amélioration des entrées de Taille d'effet pour calculer les effets LMM globaux et par paire à partir de l'un ou l'autre ensemble disponible.

## v0.9.31 - 2026-06-02

### Modifications

- Ajout de fichiers d'enregistrement/ouverture de l'historique avec marqueur de type `.efs-result`.
- Séparation des paramètres enregistrés dans `.efs-settings`, avec validation du type.
- Modification d'Ajouter un résultat pour conserver l'instantané actuellement affiché plutôt que reconstruire la sortie.
- Uniformisation des largeurs de tableaux et des règles paysage entre analyses, historique, HTML, PDF et Word.
- Amélioration des aperçus de modèle et avertissements en corrélation, tests appariés, analyse factorielle, fidélité et régression logistique.
- Suppression de la couverture Word pour commencer les résultats enregistrés directement par les méthodes et résultats.
- Ajout d'ANCOVA avec sélection automatique standard, robuste HC3, sur rangs ou avec interaction, et exportation HTML, PDF, Excel et historique.

## v0.9.30 - 2026-06-02

### Modifications

- Amélioration des calculateurs de taille d'échantillon et d'effet en retirant des menus d'effet les parcours qui ne calculent pas de taille d'effet.
- Ajout de calculs de taille d'échantillon en arrière-plan avec suivi et arrêt.
- Ajout d'entrées de corrélation non structurée LMM et d'estimation des degrés de liberté SEM/CFA par dénombrement du modèle.
- Uniformisation de la mise en évidence de l'effectif requis avec libellés `n` explicites et puissance par défaut de 0.95.
- Extension du Guide utilisateur, des Méthodes d'analyse et des Notes méthodologiques coréens pour effectif, puissance et taille d'effet, avec formules et références.
- Actualisation des Méthodes d'analyse coréennes pour les résultats 0.9.30, y compris l'aperçu du modèle en test t/ANOVA, tests appariés, appariés non paramétriques et corrélation.
- Inclusion de MathJax local pour afficher les formules des Notes méthodologiques hors ligne.

## v0.9.29 - 2026-06-01

### Ajouts

- Ajout des menus supérieurs indépendants Taille d'échantillon et Taille d'effet après Analyse.
- Ajout de calculateurs référencés d'effectif, puissance et taille d'effet pour test t, ANOVA / ANCOVA, GEE, LMM, tests non paramétriques, proportions, chi carré, McNemar, régression, survie et autres planifications.
- Ajout de validations ciblées des fonctions de calcul d'effectif, puissance atteinte et taille d'effet.

### Modifications

- Refonte des écrans d'effectif et d'effet selon les trois blocs communs de configuration des analyses.
- Réorganisation des menus Taille d'échantillon et Taille d'effet par famille de plan d'étude.
- Mise en évidence des tailles d'effet du test t correspondant à la méthode choisie, avec effets convertibles et sans valeurs intermédiaires étrangères à la taille d'effet.

## v0.9.28 - 2026-05-30

### Corrections

- Mise au premier plan des dialogues Windows de données/paramètres via un propriétaire WinForms toujours au-dessus, plutôt que le sélecteur R natif.
- Placement des tableaux de charges factorielles et PCA juste après leur aperçu à l'écran, en HTML/PDF et en Excel.
- Correction d'À propos > Licences libres : résolution des avis tiers depuis l'application incluse et regroupement par paquets EFS directs, dépendances incluses, paquets R de base/recommandés et runtime R.
- Remplacement du nettoyage de port Windows par `netstat` / `taskkill` pour éviter un blocage au démarrage pendant la fermeture d'une instance sur le port 7894.

## v0.9.27 - 2026-05-29

### Modifications

- Raccourcissement des noms d'exportation visibles de `EasyFlow_Statistics_...` au préfixe `EFS_...` pour résultats, données, paramètres et fichiers générés.

### Ajouts

- Ajout d'À propos > Historique des versions pour consulter le journal inclus dans l'application de bureau.

## v0.9.26 - 2026-05-29

### Ajouts

- Ajout d'un import Excel en deux étapes avec feuille, cellule initiale A1, gestion de la ligne d'en-tête et aperçu avant chargement.
- Conservation des options d'import Excel dans les paramètres enregistrés pour la réouverture des fichiers.

## v0.9.25 - 2026-05-29

### Corrections

- Harmonisation d'Ajouter un résultat et de Word en régression avec les coefficients visibles : références catégorielles, libellés de valeurs et résumé d'ajustement sur une ligne conservés.
- Utilisation d'une section Word paysage pour les grands tableaux de coefficients afin de mieux reproduire l'affichage.

## v0.9.24 - 2026-05-29

### Corrections

- Correction des tableaux test t / ANOVA et non paramétriques pour conserver trois décimales des p comme `.008` et des effets comme `.022` lorsque la note porte le même chiffre final.

## v0.9.23 - 2026-05-29

### Corrections

- Remplacement du sélecteur PowerShell par le dialogue Windows natif R `choose.files()` pour ouvrir fiablement les données depuis Electron installé.

## v0.9.22 - 2026-05-29

### Corrections

- Priorité à un dialogue Windows natif avant Tcl/Tk pour réduire les sélecteurs cachés derrière Electron ou absents.
- Maintien des filtres Excel, SAS, Stata, CSV, DAT et SPSS visibles dans le sélecteur.

## v0.9.21 - 2026-05-29

### Ajouts

- Ajout de l'importation Excel ancien `.xls`, SAS `.sas7bdat` / `.xpt` et Stata `.dta`.
- Mise à jour du sélecteur, des textes de Données et des validations d'E/S pour les nouveaux formats.

## v0.9.20 - 2026-05-29

### Modifications

- Réduction du contenu initial Shiny en ne générant les corps des onglets Éditeur de données, Calculateur, Analyse et À propos qu'à leur ouverture.

## v0.9.19 - 2026-05-29

### Modifications

- Réduction du temps de démarrage installé en n'attachant que Shiny et DT au lancement initial.
- Suppression des recherches redondantes de paquets du runtime inclus au démarrage Electron ; disponibilité toujours vérifiée à la construction et aux tests de publication.
- Raccourcissement de l'intervalle de vérification Shiny et ajout de diagnostics séparés de durée de chargement BrowserWindow.

## v0.9.18 - 2026-05-29

### Ajouts

- Ajout de la rangée standard de cinq commandes d'enregistrement aux résultats logistiques.
- Ajout des exportations logistiques HTML, PDF, Excel et collection de résultats enregistrée.

## v0.9.17 - 2026-05-29

### Corrections

- Modification de Corrélation > Corrélations avancées pour remplacer les méthodes principales par les corrélations latentes, au lieu d'une sortie séparée dupliquée.
- Affichage de Polyserial dans le tableau principal Méthodes pour les paires continues-ordinales/binaires admissibles lorsque les corrélations latentes sont activées.

## v0.9.16 - 2026-05-29

### Corrections

- Correction des retours à la ligne des notes de p et de tailles d'effet dans test t / ANOVA et tests non paramétriques autonomes.

## v0.9.15 - 2026-05-29

### Corrections

- Correction des repères de notes en ligne de test t / ANOVA pour aligner les tailles d'effet tout en omettant le zéro initial.

## v0.9.14 - 2026-05-29

### Modifications

- Ajout de la licence GPL, de l'offre de code source et des pages À propos pour les avis sources/licences du bureau inclus.
- Ajout d'avis OSS générés, rapport de licences, collection des textes et tests élémentaires de publication des installateurs Electron/R.
- Ajout de rapports d'élagage du runtime R inclus et de versions exactes Electron/electron-builder.
- Réduction du surcoût de démarrage installé et ajout de diagnostics de durée.
- Suppression de la protection de fermeture de session Shiny au démarrage qui pouvait laisser l'application sur un écran gris désactivé.
- Reconstruction de l'installateur bêta Windows 0.9.14.

## v0.9.13 - 2026-05-28

### Modifications

- Activation de l'exportation Word prise en charge et uniformisation des règles Word, PDF et Excel.
- Ajout d'un Word destiné à la publication : couverture, méthodes, tableaux principaux seulement, notes, exposants et B5 portrait par défaut, paysage réservé aux tableaux larges.
- Amélioration des largeurs, en-têtes, colonnes post hoc et statistiques de pied pour PDF/Word des analyses appariées, répétées, test t/ANOVA, corrélation, régression, hiérarchique et logistique.
- Amélioration d'Excel pour préserver la structure affichée avec titres, en-têtes à deux niveaux, bordures, notes fusionnées et largeurs fixes.
- Agrandissement de la fenêtre initiale Electron et stabilisation de l'alignement des actions de régression après affichage des résultats.
- Reconstruction de l'installateur bêta Windows 0.9.13.

## v0.9.12 - 2026-05-27

### Modifications

- Amélioration des mises en page PDF et de publication pour analyses appariées, appariées répétées, appariées non paramétriques, analyse factorielle, PCA, logistique et test t / ANOVA.
- Amélioration des espacements des tableaux, de l'alignement des aperçus et de la taille du filigrane bêta des rapports.
- Précision des textes pour centrer les résumés sur méthodes, N, hypothèses et décisions concises.
- Uniformisation du PDF en A4 et de Word en B5, avec tableaux ajustés à la largeur imprimable et règles d'alignement affichées conservées.
- Extension de l'ajustement paysage PDF aux colonnes de tailles d'effet appariées répétées et aux coefficients hiérarchiques.
- Uniformisation d'Excel pour reproduire en-têtes à deux niveaux, titres, bordures, largeurs fixes et notes fusionnées visibles.
- Activation du bouton Word de Résultats pour les éditions autorisant l'exportation.
- Harmonisation des polices Word d'en-tête/corps, activation du style visible du bouton Word, moyennes/écarts-types appariés à deux décimales et réunion des tableaux d'aperçu/hypothèses/diagnostics des tests appariés mixtes.
- Conservation des en-têtes doubles Word, exclusion des logos du corps, sections paysage pour tableaux répétés/hiérarchiques larges et resserrement des marges/colonnes PDF appariées, répétées et hiérarchiques.
- Présentation directe de N, méthode et motif dans les aperçus test t / ANOVA, avec en-têtes dépendante/modèle à deux niveaux pour les régressions multimodèles.
- Ajustement des tableaux appariés répétés PDF à la largeur paysage, lignes d'en-tête de premier niveau hiérarchiques et limitation des sections paysage Word pour conserver B5 portrait par défaut.
- Élargissement des colonnes post hoc/tolérance, agrandissement des couvertures PDF, regroupement des effets appariés sous en-tête double, restauration des exposants/notes Word et fusion des pieds répétés de régression.
- Réduction de la police des notes Word, conservation de toutes les notes affichées, exposants des repères d'en-tête hiérarchiques et fusion des statistiques de pied une fois par modèle.
- Étiquetage des exécutions limitées au Bloc 1 depuis l'écran hiérarchique comme régressions ordinaires lors de l'ajout à la collection.
- Export Word limité aux tableaux principaux prêts à publier, chacun sur sa page, figures à leur taille affichée sans agrandissement et paysage seulement pour tableaux appariés/hiérarchiques larges et matrices de corrélation d'au moins 10 variables.
- Centrage des statistiques de pied de régression Word et ligne supérieure forte au-dessus de F(p), avec homoscédasticité résiduelle en une seule entrée x²(p).
- Agrandissement initial Electron, ajout de couverture et méthodes Word, deux figures de régression par page et exclusion des détails post hoc test t/ANOVA de l'export Word des tableaux pour publication.
- Amélioration des espacements Word, largeurs fréquences/descriptifs, résumés de régression, transitions paysage et tailles des figures pour réduire retours, blancs et pages vides.
- Stabilisation des actions de régression après rendu, renforcement du filtrage post hoc Word, élargissement des colonnes n(%)/M±SD combinées et IQR, et resserrement des grandes tables de corrélation.

## v0.9.11 - 2026-05-27

### Modifications

- Ajout d'aperçus compacts du modèle et des hypothèses pour tests appariés, test t / ANOVA, régression et logistique.
- Déplacement des diagnostics détaillés vers des tableaux dédiés, en centrant l'aperçu sur N, méthode et motifs concis.
- Ajout d'onglets d'options en analyse factorielle, PCA et test t / ANOVA, avec état conservé et espacement amélioré.
- Activation de f2 par défaut en régression, avec sr2 non coché.

## v0.9.10 - 2026-05-27

### Modifications

- Ajout de l'empaquetage bêta Electron avec runtime R inclus, lanceur de fenêtre de bureau, métadonnées d'installation et icônes EasyFlow.
- Amélioration des imports CSV/Excel coréens par essai des encodages courants et normalisation des noms et textes importés.
- Conservation des types binaires, catégoriels et ordinaux révisés lors de l'enregistrement/chargement pour utiliser les mêmes types dans Données et Analyse.
- Limitation des colonnes Fréquences / Descriptifs aux statistiques adaptées aux types sélectionnés.
- Amélioration du filigrane bêta et des éléments provisoires Word pour l'exportation.

## v0.9.9 - 2026-05-27

### Modifications

- Refonte du recodage de la même variable autour d'une étape `Add` en attente et d'un `Apply` final, pour examiner les règles avant modification des données.
- Ajout de règles en attente modifiables avec sélection, suppression, types par défaut automatiques et inférence du type de mesure produit.
- Ajout du recodage de valeurs uniques : catégories observées, marqueurs manquants, avertissements sans correspondance et conversions depuis/vers `NA`.
- Amélioration des commandes de catégorisation, opérateurs de plage, alignement, boutons et espacement de Recoder une variable.

## v0.9.8 - 2026-05-26

### Modifications

- Ajout d'À propos avec Aperçu, Guide utilisateur, Méthodes d'analyse, Notes méthodologiques et informations de l'application.
- Extension de la documentation coréenne : utilisation, méthodes implémentées, notes, paquets/runtime, critères et références.
- Clarification de l'homoscédasticité résiduelle en régression et des libellés de tendance des tableaux croisés.
- Documentation de 20,000 réplications bootstrap, avec maintien de 50,000 comme recommandation.
- Uniformisation du nom **EasyFlow Statistics**, écrit intégralement et mis en évidence de façon cohérente.

## v0.9.7 - 2026-05-26

### Modifications

- Ajout du choix automatique de Pearson pour paires continues normales et Spearman pour paires non normales ou ordinales.
- Ajout de protections pour ignorer variables/modèles invalides en corrélation, tests appariés, test t / ANOVA, régression et logistique sans arrêter toute l'analyse.
- Ajout d'avertissements et de sorties ignorées pour faibles effectifs, variance nulle, ex æquo généralisés, cellules rares, séparation, rang déficient et seuils VIF.
- Ajout de matrices Pearson / polychoriques pour analyse factorielle/PCA avec conseils ordinaux et avertissements d'effectif.
- Regroupement des auxiliaires d'avertissements et de sorties ignorées entre écrans et Excel.

## v0.9.6 - 2026-05-25

### Modifications

- Ajout du menu autonome Test apparié non paramétrique avec rangs signés de Wilcoxon et Friedman.
- Ajout des corrections post hoc appariées Bonferroni et Holm-Bonferroni, avec Bonferroni par défaut.
- Ajout de médiane, Q1~Q3 et notes de taille d'effet Wilcoxon aux résultats appariés non paramétriques.
- Harmonisation des en-têtes, repères de notes, exportations et boutons des résultats appariés et appariés non paramétriques.

## v0.9.5 - 2026-05-25

### Modifications

- Ajout du menu autonome Tests non paramétriques avec U de Mann-Whitney et Kruskal-Wallis.
- Ajout de résumés de médiane et quartiles aux tests non paramétriques autonomes.
- Ajout du delta de Cliff aux résultats U de Mann-Whitney.
- Correction des lettres post hoc compactes pour attribuer des lettres combinées aux groupes non significatifs communs.
- Affichage des repères de notes p/effet dans d'étroites colonnes adjacentes pour stabiliser l'alignement.
- Amélioration de l'espacement des options Tests non paramétriques et Test apparié.

## v0.9.4 - 2026-05-25

### Modifications

- Amélioration des filigranes HTML/PDF réservés au développement avec marques horizontales EasyFlow et StatEdu.
- Activation de l'exportation des matrices de nuages de points et cartes thermiques de corrélation.

## v0.9.3 - 2026-05-25

### Modifications

- Harmonisation des charges PCA avec l'analyse factorielle : h², complexité, valeur propre, variance, variance cumulée et lignes KMO / Bartlett, sans colonnes de fidélité.
- Amélioration des commandes PCA de matrice, sélection par variance cumulée et alignement des champs numériques de composantes.
- Amélioration du style des diagnostics factoriels et finalisation du placement du résumé KMO / Bartlett.
- Maintien des cinq commandes d'enregistrement visibles en développement et ajout de PDF / Ajouter un résultat aux modules restants.
- Retrait de la décoration de couverture PDF et des libellés internes fichier/date, avec numérotation en bas à droite.
- Ajout d'une identité de couverture PDF selon l'édition, avec logo StatEdu et nom StatEdu Statistical Research Institute pour le développement.
- Ajout de la date de sortie PDF sous la date d'enregistrement sur la couverture.
- Ajout de filigranes réservés au développement aux rapports HTML et PDF exportés.

## v0.9.1 - 2026-05-24

### Modifications

- Amélioration de l'analyse factorielle exploratoire : charges triées, filtrage facultatif des petites charges, valeurs problématiques surlignées, communalités, complexité, valeurs propres, variances et matrices de structure oblique.
- Ajout de résumés facultatifs de fidélité des sous-facteurs directement à côté des charges.
- Amélioration des diagnostics factoriels pour extraction selon normalité, nombre fixe élevé de facteurs, valeurs manquantes/infinies et problèmes de fidélité par item.
- Compactage du panneau factoriel pour placer toutes les options dans les trois colonnes standard.

## v0.9.0 - 2026-05-24

### Modifications

- Ajout de l'accumulation dans Résultats pour recueillir les sorties compatibles dans l'ordre avec Ajouter un résultat.
- Ajout de l'exportation des collections en HTML, PDF, Excel et Word.
- Maintien de l'analyse factorielle et de PCA hors d'Ajouter un résultat jusqu'au choix définitif de leur format de tableaux.

## v0.8.12

### Modifications

- Ajout de l'analyse factorielle exploratoire avec axes principaux et maximum de vraisemblance, rotations Varimax/Oblimin, sélection par valeur propre ou nombre fixe, méthode selon normalité, KMO / Bartlett, graphiques des valeurs propres et exportation.
- Ajout de PCA avec matrice de corrélation/covariance, sélection par valeur propre, nombre fixe ou variance cumulée, rotation facultative, graphiques des valeurs propres et composantes, diagnostics et exportation.
- Ajout de validations des calculs et exportations d'analyse factorielle et PCA.

## v0.8.11

### Modifications

- Restauration du logo horizontal EasyFlow Statistics dans la barre de navigation, plutôt qu'une composition d'icône et de texte HTML.

## v0.8.10

### Modifications

- Correction du contraste de la marque de navigation pour garder le texte EasyFlow Statistics et la version lisibles sur l'en-tête clair.
- Uniformisation de la notation post hoc ordonnée pour afficher les motifs communs de façon cohérente, notamment `3, 2>1` et `3>2, 1`.
- Amélioration du retour des variables depuis les listes dépendantes ou indépendantes de test t / ANOVA.
- Amélioration de l'inférence du type de mesure pour ne pas classer les nombres décimaux comme catégoriels uniquement parce qu'ils ont peu de valeurs distinctes.
- Limitation du nettoyage du lanceur au port de l'application avant une nouvelle session EasyFlow Statistics.
- Ajout de détection automatique des valeurs manquantes avec conversion contrôlée vers `NA`.
- Ajout de transformations par formule pour créer des variables à partir d'expressions numériques, textuelles, statistiques, de dates et conditionnelles.
- Réorganisation de l'Éditeur de données et regroupement du recodage dans Recoder une variable, avec cible identique ou nouvelle variable.

## v0.8.7

### Modifications

- Ajout de détection automatique des textes Likert et de conversion par lots des enquêtes importées.
- Ajout de commandes Likert regroupées : texte d'item, libellés d'origine, valeurs numériques, codage inversé et type après conversion.
- Amélioration des niveaux Likert partiels pour aligner les items dont certaines modalités ne sont pas observées sur l'échelle complète détectée.
- Réduction de la largeur des colonnes statistiques compactes hiérarchiques pour faciliter la lecture.

## v0.8.6

### Modifications

- Ajout de l'examen des variables à l'Étape 3 avec vues Libellés / Variables et commande Appliquer commune aux libellés de valeurs/variables et aux types de mesure.
- Propagation garantie des changements de type de l'Étape 3 aux menus d'analyse après application.
- Maintien des commandes de l'Étape 3 cohérentes avec la présentation actuelle de Données.

## v0.8.4

### Modifications

- Amélioration des tableaux de fréquences, notes test t / ANOVA, p/IC de corrélation, analyse des items de fidélité, espacement Durbin-Watson, annotations hiérarchiques et résultats logistiques instables.
- Ajout de modification groupée des types de mesure à l'Étape 2 pour les variables cochées sur la page Données actuelle.
- Conservation des niveaux de mesure source lorsque le codage inversé automatique crée ou remplace des variables.

## v0.8.3

### Modifications

- Ajout à l'Éditeur de données des contrôles d'erreurs de codage, du codage inversé automatique, du recodage vers une autre variable et du calcul par ligne.
- Ajout de commandes d'application des corrections, d'aperçus des variables créées, d'enregistrement après création et de validations du recodage et des lectures CSV / DAT copiées.
- Uniformisation des configurations/résultats entre Éditeur de données, Calculateur et Analyse, avec placement commun des boutons et comportement de repli du visualiseur des données sélectionnées.
- Mise à jour des options par défaut et commandes post hoc non paramétriques, dont alfa ordinal de fidélité et espacement test t / ANOVA.
- Amélioration des fichiers synchronisés dans le cloud en copiant SAV, CSV et DAT dans un emplacement temporaire avant importation.
- Restauration de la sélection multiple Ctrl / Shift / Ctrl+A des listes de transfert, avec synchronisation Shiny stable.

## v0.8.2

### Modifications

- Activation par défaut des options courantes dans tests appariés, fréquences, corrélation, fidélité, logistique et test t / ANOVA.
- Ajout de corrections post hoc non paramétriques indépendantes après Kruskal-Wallis, Bonferroni par défaut et Holm Bonferroni disponible.
- Ajustement de l'espacement test t / ANOVA pour loger les commandes post hoc et taille d'effet dans le panneau.
- Ajout du recodage de la même variable dans l'Éditeur de données.

## v0.8.1

### Modifications

- Réunion de Test apparié (2) et Test apparié (3+) en une configuration choisissant l'analyse selon le nombre de mesures répétées.
- Renommage du parcours hiérarchique en Régression et retrait du menu de régression séparé, avec maintien des comportements à un bloc et hiérarchique multibloc.
- Ajout du suivi et de l'arrêt bootstrap au parcours de régression unifié.
- Mise à jour des dimensions du test apparié et de la marque de l'onglet Données.

## v0.8.0

### Modifications

- Ajout de configuration/résultats logistiques pour dépendantes binaires, ordinales et multinomiales, avec blocs hiérarchiques, OR / IC, pseudo R2, VIF, ajustement et avertissements.
- Ajout de Réinitialiser les paramètres commun aux analyses, actif seulement si le bloc d'affectation contient des variables.
- Uniformisation de l'accès au visualiseur sélectionné, du retrait par double-clic et de l'espacement des trois panneaux d'analyse.
- Compactage des blocs initiaux vides de régression et régression hiérarchique avant exécution.

## v0.7.11

### Modifications

- Modification des tableaux croisés pour affecter les colonnes au-dessus des lignes et dimensionner les panneaux selon les nombres attendus de variables.
- Ajout du PDF pour tableaux croisés et activation par défaut de toutes les actions d'enregistrement en développement.
- Uniformisation des tableaux croisés : statistiques alignées en haut, en-têtes centrés, valeurs de ligne à gauche et notes de taille d'effet numérotées.
- Uniformisation des tailles d'effet à trois décimales sans zéro initial dans les résultats.
- Ajout de notes numérotées de p, taille d'effet et tendance aux tests t / ANOVA, avec repères en exposant.
- Ajout de validations des notes test t / ANOVA et extension de celles des tableaux croisés.

## v0.7.10

### Modifications

- Ajout d'analyse des tableaux croisés pour variables binaires, ordinales et catégorielles avec chi carré de Pearson, repli Fisher exact / Monte Carlo et tendance.
- Ajout d'affectation multiple de lignes/colonnes ordonnables, tableaux regroupés par colonne, pourcentages ligne/colonne/total et cellules n/pourcentage séparées facultatives.
- Ajout de notes sur la méthode de p, de p de tendance avec notes propres à la méthode, de notes de taille d'effet et d'exportation HTML / Excel des tableaux croisés.
- Ajout de validations des statistiques, du rendu, de l'ordre des variables et des auxiliaires d'exportation des tableaux croisés.

## v0.7.9

### Modifications

- Resserrement et harmonisation des panneaux des calculateurs EQ-5D, syndrome métabolique et sévérité métabolique.
- Masquage de la table des critères par défaut du syndrome métabolique lorsque les critères personnalisés sont sélectionnés.
- Refonte du panneau Formule de sévérité métabolique selon les autres panneaux de référence et séparation de sa section Sortie.
- Ajout de validation des calculateurs HINT8, EQ-5D, syndrome métabolique, FRS, ASCVD10 et sévérité métabolique.

## v0.7.7

### Modifications

- Remplacement de l'affichage initial HINT8 par une matrice compacte item par niveau.
- Maintien des panneaux HINT8 visibles après chargement même sans variable ordinale disponible.
- Resserrement du panneau des valeurs initiales HINT8.

## v0.7.6

### Modifications

- Remplacement de la longue liste initiale EQ-5D par une matrice compacte dimension par niveau.

## v0.7.5

### Modifications

- Correction de l'application à Données Étape 3 : libellés de variables/valeurs et types appliqués en un clic et conservés dans les paramètres enregistrés.
- Réservation de PDF, Excel et Ajouter un résultat aux exportations payantes, en conservant HTML et figures dans le mode gratuit.
- Ajout de rapports PDF de régression et hiérarchiques avec couverture, orientations mixtes, tableaux larges redimensionnés et graphiques en deux colonnes.
- Amélioration du HTML enregistré comme visualiseur avec défilement horizontal des tableaux et conservation de la présentation à l'écran.
- Uniformisation des boutons d'enregistrement de régression et hiérarchiques, avec sr2, f2 et VIF actifs par défaut.
- Correction des dialogues d'enregistrement répétés après annulation et réduction des erreurs d'activation d'onglets d'analyse.

## v0.7.4

### Modifications

- Réorganisation de la navigation en Données, Éditeur de données, Calculateur, Analyse, Résultats et À propos.
- Ajout de groupes Éditeur de données et Analyse, dont les menus imbriqués Test apparié et Régression.
- Amélioration des menus imbriqués pour utiliser l'activation normale des onglets Shiny dans Analyse et Calculateur.
- Réduction des délais de configuration en évitant les vidages inutiles de tables de l'Étape 3 et les résumés répétés de variables pendant la navigation.

## v0.7.3

### Modifications

- Ajout de modules de calcul HINT8, EQ5D, Syndrome métabolique, Sévérité métabolique, FRS et ASCVD10.
- Réintégration des valeurs calculées dans les données chargées pour les rendre disponibles dans les menus d'analyse.
- Suppression des vestiges de Données Étape 4/5 et finalisation de l'édition des libellés à l'Étape 3.
- Uniformisation des notes de résultats pour respecter la largeur des tableaux dans toutes les analyses.

## v0.7.2

### Modifications

- Ajout de blocs de sous-facteurs de Fidélité avec lignes de total, analyse combinée des items et diagnostics de suppression pour l'ensemble des items.
- Amélioration de Fidélité pour afficher et valider les statistiques omega uniquement lorsque l'option correspondante est active.
- Ajustement des dimensions des listes, largeurs de tableaux et alignement des en-têtes de Fidélité.

## v0.7.1

### Modifications

- Amélioration des libellés de mesures répétées, en-têtes groupés, notes post hoc et annotations de taille d'effet de Test apparié (3+).
- Uniformisation des hauteurs de listes et boutons de transfert dans Fidélité, Fréquences, Test apparié, test t/ANOVA, Corrélation, Régression et Hiérarchique.

## v0.7.0

### Ajouts

- Ajout de Test apparié (3+) pour au moins trois mesures, avec sélection de RM ANOVA, Friedman ou Q de Cochran, vérification des hypothèses et comparaisons post hoc.
- Ajout de tailles d'effet répétées : eta carré partiel, W de Kendall, g de Hedges et r de Wilcoxon.

### Modifications

- Amélioration des tableaux appariés, notation post hoc, placement des tailles d'effet et exportations HTML/Excel.
- Ajustement de la sélection appariée avec lignes de mesures répétées regroupées et listes cibles plus compactes.

## v0.6.8

### Ajouts

- Ajout de Test apparié pour deux mesures : test t apparié/Wilcoxon, McNemar/McNemar exact pour paires binaires et Stuart-Maxwell/Bowker pour paires catégorielles.
- Ajout de contrôles facultatifs des différences appariées avec Shapiro-Wilk ou asymétrie/aplatissement, plus détection des valeurs aberrantes à 3*IQR.
- Ajout d'exportations HTML et Excel des tableaux appariés et notes de contrôle des hypothèses.

## v0.6.7

### Modifications

- Recentrage des boutons de transfert par suppression du décalage inférieur commun et alignement des deux boutons de régression/test t avec leurs lignes cibles.

## v0.6.6

### Modifications

- Affichage des coefficients catégoriels de régression et hiérarchiques sous `variable:level`, avec ligne de référence par défaut même sans référence explicite dans Données.

## v0.6.5

### Modifications

- Restauration de l'alignement des boutons de transfert selon la géométrie 0.5.7, appliquée aussi au nouvel onglet Fidélité.
- Restauration du placement des boutons d'enregistrement hiérarchiques sur la rangée d'actions 0.5.7.

## v0.6.4

### Modifications

- Application d'astérisques de significativité à la matrice de corrélation lorsque l'option des niveaux de significativité est choisie.

## v0.6.3

### Modifications

- Correction de l'Étape 3 pour transmettre les types de mesure actuels avec les libellés lors de la navigation vers les analyses.
- Inclusion des sélecteurs de mesure des libellés de catégories de l'Étape 3 dans la collecte directe des entrées côté serveur.
- Retour du bloc d'enregistrement hiérarchique sous le troisième bloc de configuration.

## v0.6.2

### Modifications

- Correction du filtrage des dépendantes de régression et hiérarchiques pour respecter les substitutions de mesure de l'Étape 3 lors du choix de dépendantes continues.
- Réalignement des boutons de transfert après les changements communs de géométrie de configuration.

## v0.6.1

### Modifications

- Correction de la propagation immédiate des types modifiés à l'Étape 3 dans les listes Fidélité, Fréquences, test t/ANOVA, Corrélation, Régression et Hiérarchique.

## v0.6.0

### Modifications

- Ajout de Fidélité avec items de même niveau, sélection automatique KR-20/alpha de Cronbach/omega, alpha/omega ordinaux, diagnostics d'items et notes selon normalité.
- Uniformisation des commandes HTML/figures/Excel/ajout de résultat selon l'édition entre onglets de résultats.
- Amélioration de HTML et Excel pour conserver les notes à la largeur des tableaux et des colonnes Excel lisibles.
- Ajout de `psych` comme moteur d'alpha, omega et coefficients ordinaux fondés sur les corrélations polychoriques.

## v0.5.7

### Modifications

- Refonte de la configuration hiérarchique avec Variables dépendantes et un Bloc actif à la fois, navigation précédent/suivant et état des Blocs 1/2/3 conservé.
- Ajustement des hauteurs, tailles de listes et navigation de blocs pour une configuration hiérarchique compacte et alignée.
- Amélioration des séparateurs entre coefficients et ajustement dans les tableaux hiérarchiques.
- Ajustement des largeurs et marges internes pour les sorties hiérarchiques larges à trois modèles avec colonnes de taille d'effet.

## v0.5.6

### Modifications

- Ajout d'exportation HTML commune aux résultats et harmonisation avec le style des tableaux de régression de l'application.
- Extension de Corrélation : méthode automatique selon mesure, corrélations latentes facultatives, matrices méthode/motif, p et IC 95 %, et figures de dispersion/chaleur plus grandes.
- Amélioration du style Excel/HTML de régression et hiérarchique avec en-têtes doubles, alignement numérique, notes et dialogues d'enregistrement.
- Stabilisation des options de régression et hiérarchiques pendant bootstrap.
- Uniformisation de la géométrie des blocs de configuration des analyses non hiérarchiques.

## v0.5.5

### Modifications

- Implémentation de Corrélation avec corrélations par paire, normalité facultative, p, intervalles de confiance, repères de significativité, matrices et graphiques.
- Ajout d'Excel pour les tableaux test t / ANOVA.
- Ajout d'Excel et d'exportation des figures de diagnostic résiduel pour la régression hiérarchique.
- Mise à jour des paquets requis pour l'exécution locale avec les nouvelles dépendances d'analyse.

## v0.5.4

### Modifications

- Ajout de taille d'effet, tendance, notation ordonnée de significativité et post hoc étendu aux tests t / ANOVA.
- Amélioration des options de normalité, libellés d'aperçu/statistiques, notes p et tableaux test t / ANOVA.
- Ajout du test des étendues multiples de Duncan via agricolae et mise à jour du chargement des paquets requis.
- Correction des colonnes facultatives de Fréquences / Descriptifs et adoption de largeurs compactes de type régression.
- Amélioration des espacements, séparateurs et libellés chi carré des tableaux hiérarchiques.

## v0.5.2

### Modifications

- Ajout du nombre de réplications bootstrap et de la graine à l'aperçu du modèle lors d'une régression bootstrap.
- Stabilisation de la sélection Shift du premier item de régression et réduction des réinitialisations du défilement dues à la sélection.
- Agrandissement de la liste disponible de régression pour afficher 20 variables.
- Ajout et amélioration des concepts de logo SVG EasyFlow Statistics.

## v0.5.1

### Modifications

- Stabilisation de la sélection multiple, Ctrl+A, direction et conservation de l'ordre des transferts de régression.
- Amélioration de la configuration de régression : hauteurs fixes, boutons et persistance des cases d'options.
- Ajout d'un comportement commun d'exportation des tableaux et figures d'analyse.
- Ajout de l'ébauche de configuration/résultats Fréquences / Descriptifs avec transfert partagé des variables.
- Mise à jour des métadonnées de citation EasyFlow Statistics.

## v0.5.0

### Ajouts

- Ajout d'une ébauche Hiérarchique pour régression multiple hiérarchique avec une dépendante et prédicteurs organisés en Blocs 1/2/3.
- Ajout de commandes de transfert du Bloc 2 au Bloc 3 pour la future configuration hiérarchique.
- Ajout d'une ébauche Généralisé pour les futurs modèles de régression généralisée.

### Modifications

- Mise à jour de Généralisé pour les modèles GLM, avec retrait de bootstrap et sr2/f2 propres à OLS.
- Regroupement des modèles de comptage en Poisson / Binomiale négative / Inflation de zéros, et maintien de Gamma pour résultats continus positifs.
- Mise à jour de la présentation Généralisé pour utiliser exp(B) comme IRR / rapport.

## v0.4.1

### Modifications

- Renommage de l'onglet et des titres de page de régression d'EasyFlow Statistics en Régression.

### Corrections

- Correction du traitement des avertissements VIF vides pouvant afficher `missing value where TRUE/FALSE needed`.

## v0.4.0

### Ajouts

- Ajout de dialogues Windows natifs pour enregistrer les tableaux Excel et sélectionner le dossier de figures.
- Ajout de classeurs Excel au style de revue avec coefficients, ajustement, diagnostics et notes.
- Ajout d'avertissements de multicolinéarité VIF avec recommandations pour les valeurs sévères.
- Ajout de Ridge, LASSO et Elastic Net par validation croisée pour la multicolinéarité sévère.
- Ajout de tableaux SCI de régression pénalisée : performance, comparaison des coefficients OLS/pénalisés et prédicteurs retenus.

### Modifications

- Amélioration d'Aperçu du modèle dans Excel avec fusion des cellules d'indépendantes communes, renvoi à la ligne et largeurs compactes.
- Masquage des diagnostics résiduels et de Durbin-Watson lors de l'affichage des régressions pénalisées.
- Nommage des feuilles de régression selon les libellés ou noms des dépendantes.

### Corrections

- Correction des erreurs d'enregistrement des paramètres sans variable catégorielle sélectionnée.
- Interdiction d'enregistrer des noms de mesure vides dans les substitutions de mesure.

## v0.3.1

### Modifications

- Réunion d'Aperçu du modèle en une table pour plusieurs dépendantes.
- Réorganisation de la régression avec tous les coefficients d'abord, puis les graphiques de diagnostic.
- Réunion des contrôles d'hypothèses et des résultats Durbin-Watson en une table chacun pour les dépendantes.
- Affichage des dépendantes par libellé s'il existe, sinon par nom.
- Affichage unique des recommandations de taille d'effet après les coefficients.

## v0.2.0

### Ajouts

- Ajout de sorties séquentielles de régression pour plusieurs dépendantes.
- Ajout du suivi et de l'arrêt bootstrap dans la configuration de régression.
- Ajout facultatif de sr2, f2 et diagnostics VIF/colinéarité.
- Ajout de références d'interprétation pour sr2 et f2 de Cohen.
- Ajout de graphiques de diagnostic résiduel côte à côte.

### Modifications

- Refonte de la configuration avec Variables, Variables dépendantes, Variables indépendantes et commandes bootstrap.
- Mise à jour d'Aperçu du modèle pour présenter dépendante, indépendantes, N, R2(adj. R2), F(p) et méthode choisie.
- Uniformisation des graphiques d'homoscédasticité résiduelle et des limites de valeurs aberrantes.
- Amélioration des exposants/indices des résultats de régression.

### Corrections

- Correction de l'édition des libellés pour ne plus réinitialiser la saisie à chaque caractère.
- Correction du chargement des paramètres et de la propagation entre étapes des libellés de variables, mesures, références et libellés de valeurs.
- Correction de l'arrêt bootstrap et du placement de la progression.

## v0.1.2

### Ajouts

- Ajout des commandes Monter/Descendre sous Variables dépendantes dans la configuration de régression.
- Conservation de l'ordre des dépendantes dans les paramètres enregistrés et les résumés.

## v0.1.1

### Corrections

- Activation du bouton d'en-tête `selected` de l'Étape 3 pour sélectionner ou désélectionner toutes les variables visibles du rôle actif.
- Modification d'Appliquer à l'Étape 3 pour transmettre l'état actuel des cases DataTables.
- Conservation et synchronisation des modifications `var_label`, `reference`, `value` et `label` lors des rafraîchissements DataTables.

## v0.1.0

### Ajouts

- Prototype initial de l'application Shiny.
- Chargement CSV et sélection des variables.
- Analyse de régression multiple.
- Test de normalité résiduelle Kolmogorov-Smirnov corrigé par Lilliefors.
- Test d'homoscédasticité Breusch-Pagan.
- Erreurs-types robustes HC3.
- Intervalles de confiance bootstrap.
- Consultation dL/dU de Durbin-Watson via `C:/StatEdu/easyflow_statistics/easyflow_statistics_3.0.xlsx`.
