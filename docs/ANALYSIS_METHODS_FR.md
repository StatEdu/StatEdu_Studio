# Analyses — StatEdu Studio 1.3.0

Périmètre et résultats de StatEdu Studio 1.3.0, édition publique.

## Sommaire

1. [Périmètre public de 1.3.0](#scope)
2. [Préparation des données](#data)
3. [Fréquences, descriptifs et tableaux croisés](#descriptive)
4. [Comparaisons de groupes et ANCOVA](#group)
5. [Mesures appariées et répétées mixtes](#paired)
6. [Corrélation, fidélité et accord](#correlation)
7. [Analyse factorielle exploratoire et PCA](#factor)
8. [Analyse importance–performance (IPA)](#ipa)
9. [Régression et régression hiérarchique](#regression)
10. [Effets de médiation et de modération](#mediation)
11. [Régression logistique, GLM et pénalisée](#generalized)
12. [Données longitudinales, de panel et d’enquête](#longitudinal)
13. [Analyse de survie](#survival)
14. [Analyse factorielle confirmatoire (CFA)](#cfa)
15. [Modèles d’équations structurelles (SEM)](#sem)
16. [PLS-SEM et PLSc](#pls)
17. [Effectif, puissance et taille d’effet](#planning)
18. [Tableaux, figures et exportation](#reporting)
19. [Validation et limites du rapport](#validation)

<a id="scope"></a>

## 1. Périmètre public de 1.3.0

CFA, SEM, PLS-SEM et les effets de médiation/modération figurent dans les menus publics. L’installateur public exclut la méta-analyse et l’ANOVA des traitements à mesures répétées sur les mêmes sujets. Pro est prévu pour une version ultérieure.

<a id="data"></a>

## 2. Préparation des données

Importez SPSS, SAS, Stata, Excel, CSV et DAT ; vérifiez noms, libellés, niveaux de mesure, catégories et références. Consultez l’effectif utilisé et le traitement des données manquantes de chaque analyse.

<a id="descriptive"></a>

## 3. Fréquences, descriptifs et tableaux croisés

Affichez effectifs, pourcentages, statistiques de position et dispersion et tableaux de contingence. Les options choisies fournissent tests d’association, tailles d’effet et diagnostics.

<a id="group"></a>

## 4. Comparaisons de groupes et ANCOVA

Utilisez les tests t indépendants, ANOVA, ANCOVA et comparaisons non paramétriques. Les options disponibles incluent des méthodes adaptées aux variances, comparaisons post hoc et tailles d’effet.

<a id="paired"></a>

## 5. Mesures appariées et répétées mixtes

Utilisez les analyses appariées, à mesures répétées, l’ANOVA mixte et les analyses appariées non paramétriques. Consultez les effets de temps et de groupe et les comparaisons du plan choisi.

<a id="correlation"></a>

## 6. Corrélation, fidélité et accord

Analysez corrélations, fidélité des échelles et accord interévaluateurs. Choisissez méthodes et indices adaptés aux variables et au plan d’évaluation.

<a id="factor"></a>

## 7. Analyse factorielle exploratoire et PCA

Examinez nombre de facteurs/composantes, extraction, rotation, saturations et variance expliquée en EFA et PCA. La CFA fait l’objet d’une rubrique distincte.

<a id="ipa"></a>

## 8. Analyse importance–performance (IPA)

Ouvrez Analyse → IPA et choisissez évaluations directes ou importance dérivée. Appariez importance et performance dans le même ordre, ou affectez performance et satisfaction globale. Choisissez ensemble, groupes indépendants ou avant/après apparié. Utilisez les colonnes WIDE correspondantes ou ID/temps LONG et deux valeurs temporelles. Réglez références et graphiques; vérifiez effectifs, coordonnées, intervalles et différences. Word/HWPX s’enregistrent depuis la collection après ajout.

<a id="regression"></a>

## 9. Régression et régression hiérarchique

Utilisez OLS, l’inférence robuste HC3 et la régression bootstrap. L’analyse hiérarchique compare les blocs successifs et les variations de variance expliquée. Les sorties choisies incluent sr², f², colinéarité et diagnostics résiduels. La régression hiérarchique prend en charge jusqu’à quatre blocs. Chaque étape conserve les variables des blocs précédents et ajoute le bloc suivant.

<a id="mediation"></a>

## 10. Effets de médiation et de modération

Définissez prédicteur, résultat, médiateur, modérateur et covariables, puis dessinez les chemins pour estimer les effets directs, indirects, totaux et conditionnels. Il ne s’agit pas de sélectionner un numéro de modèle. Les structures incompatibles sont vérifiées avant exécution.

<a id="generalized"></a>

## 11. Régression logistique, GLM et pénalisée

Utilisez modèles logistiques et linéaires généralisés, Ridge, LASSO et Elastic Net. Rapportez coefficients et performances avec échelle du résultat, famille, lien et validation croisée.

<a id="longitudinal"></a>

## 12. Données longitudinales, de panel et d’enquête

Les plans compatibles comprennent GEE, LMM, GLMM, effets fixes/aléatoires et enquêtes complexes. Vérifiez identifiants individuels/de grappes, temps, poids, strates et grappes selon le cas.

<a id="survival"></a>

## 13. Analyse de survie

Utilisez Kaplan–Meier, log-rank, RMST, Cox et les analyses compatibles de risques concurrents. Vérifiez format et codage des événements ; les combinaisons non prises en charge sont restreintes.

<a id="cfa"></a>

## 14. Analyse factorielle confirmatoire (CFA)

Utilisez ML, MLR et WLSMV/DWLS pour données ordinales ; examinez saturations, ajustement, fidélité, AVE, HTMT et comparaisons compatibles d’invariance de mesure. Normal/Wishart se limite aux réglages ML applicables.

<a id="sem"></a>

## 15. Modèles d’équations structurelles (SEM)

Rapportez chemins de mesure/structurels, ajustement et effets directs, indirects et conditionnels compatibles. Le bootstrap utilise par défaut 5,000 réplications ; les résultats reflètent les intervalles BC ou percentiles et les options choisies.

<a id="pls"></a>

## 16. PLS-SEM et PLSc

Utilisez Mode A réflectif, Mode B formatif, PLSc réflectif, diagnostics de chemins/mesure et prédiction/comparaisons compatibles. Les valeurs manquantes sont remplacées par les moyennes des indicateurs via seminr::mean_replacement, recalculées à chaque réplication bootstrap. Le défaut est 5,000 réplications.

<a id="planning"></a>

## 17. Effectif, puissance et taille d’effet

Calculez effectif, puissance et taille d’effet pour les tests compatibles. Consignez alpha, effet supposé, répartition des groupes et direction du test.

<a id="reporting"></a>

## 18. Tableaux, figures et exportation

L’édition publique 1.3.0 exporte HTML/images et PDF/Word/Excel. Les rapports HTML/PDF de médiation/modération, CFA, SEM et PLS-SEM ajoutent la figure du modèle à la fin. Les images conservent la disposition affichée ; Free utilise 300 dpi et développement/Pro, 600 dpi. Examinez puis ajoutez les résultats à la collection. HTML, PDF, Word et Excel sont disponibles. HWPX apparaît uniquement dans les résultats cumulés en interface coréenne et est écrit directement, sans Word ni Hancom. Word/HWPX proposent tableaux principaux, annexes, explications et figures; les tableaux principaux sont présélectionnés. La capture est enregistrée sans recalcul. Free utilise 300 dpi, le développement 600 dpi. HTML inclut couverture et liste de tableaux avec liens.

<a id="validation"></a>

## 19. Validation et limites du rapport

Les analyses générales, régressions, analyses longitudinales et de survie ont été comparées à SPSS ; CFA/SEM à AMOS ; PLS-SEM/PLSc et CB-SEM à SmartPLS. Ces validations cumulées sont intégrées à 1.3.0 ; la page Validation résume conditions et différences restantes.
