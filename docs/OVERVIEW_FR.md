# Présentation — StatEdu Studio 1.3.0

StatEdu Studio est une application statistique Windows pour préparer les données, analyser, visualiser les modèles, planifier les effectifs et présenter les résultats. Affectez variables et options dans l’interface et examinez méthodes, hypothèses, diagnostics et interprétation. Cette présentation couvre toute l’application, pas seulement les nouveautés de 1.3.0.

## Édition des données, périmètre et calculateurs

Importez SPSS, SAS, Stata, Excel, CSV et DAT ; vérifiez noms, libellés, niveaux de mesure, catégories et références. Consultez l’effectif utilisé et le traitement des données manquantes de chaque analyse.

Gérez noms, labels, niveaux de mesure et catégories; recodez et calculez des variables, traitez les valeurs manquantes, fusionnez, agrégez par ID et transformez WIDE–LONG. Sélection et segmentation définissent le périmètre. Des calculateurs EQ-5D, HINT-8, Framingham, ASCVD et métaboliques sont également proposés.

## Analyses disponibles

### Fréquences, descriptifs et tableaux croisés

Affichez effectifs, pourcentages, statistiques de position et dispersion et tableaux de contingence. Les options choisies fournissent tests d’association, tailles d’effet et diagnostics.

### Comparaisons de groupes et ANCOVA

Utilisez les tests t indépendants, ANOVA, ANCOVA et comparaisons non paramétriques. Les options disponibles incluent des méthodes adaptées aux variances, comparaisons post hoc et tailles d’effet.

### Mesures appariées et répétées mixtes

Utilisez les analyses appariées, à mesures répétées, l’ANOVA mixte et les analyses appariées non paramétriques. Consultez les effets de temps et de groupe et les comparaisons du plan choisi.

### Corrélation, fidélité et accord

Analysez corrélations, fidélité des échelles et accord interévaluateurs. Choisissez méthodes et indices adaptés aux variables et au plan d’évaluation.

### Analyse factorielle exploratoire et PCA

Examinez nombre de facteurs/composantes, extraction, rotation, saturations et variance expliquée en EFA et PCA. La CFA fait l’objet d’une rubrique distincte.

### Analyse importance–performance (IPA)

Ouvrez Analyse → IPA et choisissez évaluations directes ou importance dérivée. Appariez importance et performance dans le même ordre, ou affectez performance et satisfaction globale. Choisissez ensemble, groupes indépendants ou avant/après apparié. Utilisez les colonnes WIDE correspondantes ou ID/temps LONG et deux valeurs temporelles. Réglez références et graphiques; vérifiez effectifs, coordonnées, intervalles et différences. Word/HWPX s’enregistrent depuis la collection après ajout.

### Régression et régression hiérarchique

Utilisez OLS, l’inférence robuste HC3 et la régression bootstrap. L’analyse hiérarchique compare les blocs successifs et les variations de variance expliquée. Les sorties choisies incluent sr², f², colinéarité et diagnostics résiduels. La régression hiérarchique prend en charge jusqu’à quatre blocs. Chaque étape conserve les variables des blocs précédents et ajoute le bloc suivant.

### Effets de médiation et de modération

Définissez prédicteur, résultat, médiateur, modérateur et covariables, puis dessinez les chemins pour estimer les effets directs, indirects, totaux et conditionnels. Il ne s’agit pas de sélectionner un numéro de modèle. Les structures incompatibles sont vérifiées avant exécution.

### Régression logistique, GLM et pénalisée

Utilisez modèles logistiques et linéaires généralisés, Ridge, LASSO et Elastic Net. Rapportez coefficients et performances avec échelle du résultat, famille, lien et validation croisée.

### Données longitudinales, de panel et d’enquête

Les plans compatibles comprennent GEE, LMM, GLMM, effets fixes/aléatoires et enquêtes complexes. Vérifiez identifiants individuels/de grappes, temps, poids, strates et grappes selon le cas.

### Analyse de survie

Utilisez Kaplan–Meier, log-rank, RMST, Cox et les analyses compatibles de risques concurrents. Vérifiez format et codage des événements ; les combinaisons non prises en charge sont restreintes.

### Analyse factorielle confirmatoire (CFA)

Utilisez ML, MLR et WLSMV/DWLS pour données ordinales ; examinez saturations, ajustement, fidélité, AVE, HTMT et comparaisons compatibles d’invariance de mesure. Normal/Wishart se limite aux réglages ML applicables.

### Modèles d’équations structurelles (SEM)

Rapportez chemins de mesure/structurels, ajustement et effets directs, indirects et conditionnels compatibles. Le bootstrap utilise par défaut 5,000 réplications ; les résultats reflètent les intervalles BC ou percentiles et les options choisies.

### PLS-SEM et PLSc

Utilisez Mode A réflectif, Mode B formatif, PLSc réflectif, diagnostics de chemins/mesure et prédiction/comparaisons compatibles. Les valeurs manquantes sont remplacées par les moyennes des indicateurs via seminr::mean_replacement, recalculées à chaque réplication bootstrap. Le défaut est 5,000 réplications.

### Effectif, puissance et taille d’effet

Calculez effectif, puissance et taille d’effet pour les tests compatibles. Consignez alpha, effet supposé, répartition des groupes et direction du test.

## Examiner, cumuler et enregistrer

Examinez puis ajoutez les résultats à la collection. HTML, PDF, Word et Excel sont disponibles. HWPX apparaît uniquement dans les résultats cumulés en interface coréenne et est écrit directement, sans Word ni Hancom. Word/HWPX proposent tableaux principaux, annexes, explications et figures; les tableaux principaux sont présélectionnés. La capture est enregistrée sans recalcul. Free utilise 300 dpi, le développement 600 dpi. HTML inclut couverture et liste de tableaux avec liens.

## Documentation et périmètre

Dans Informations, ouvrez Présentation, Guide, Analyses, Notes méthodologiques, Validation et Historique. Les tableaux statistiques principaux restent en anglais; menus et explications suivent la langue choisie. La validation concerne les données et options indiquées, pas toutes les combinaisons ni une relecture par des locuteurs natifs.

L’installeur de développement est StatEdu Studio Dev 1.3.0-dev, avec un nom distinct et les menus de développement. L’édition publique exclut la méta-analyse et l’ANOVA des traitements répétés chez un même sujet; l’ANOVA mixte à mesures répétées et les tests appariés restent disponibles.
