# Notes méthodologiques — StatEdu Studio 1.3.0

Développement des notes originales : principes d’estimation, hypothèses, formules, diagnostics et interprétation.

## Sommaire

- [1. Niveau de mesure](#method-1)
- [2. Descriptifs et fréquences](#method-2)
- [3. Tableaux croisés](#method-3)
- [4. Test t et ANOVA](#method-4)
- [5. Tests non paramétriques](#method-5)
- [6. Données appariées et répétées](#method-6)
- [7. Corrélation](#method-7)
- [8. Fidélité et accord](#method-8)
- [9. Analyse factorielle exploratoire](#method-9)
- [10. Composantes principales](#method-10)
- [11. Régression linéaire](#method-11)
- [12. Régression hiérarchique](#method-12)
- [13. Médiation et modération](#method-13)
- [14. Régression logistique](#method-14)
- [15. Modèle linéaire généralisé](#method-15)
- [16. Régression régularisée](#method-16)
- [17. Modèles longitudinaux et de panel](#method-17)
- [18. Enquêtes complexes](#method-18)
- [19. Interprétation des seuils](#method-19)
- [20. Avertissements et résultats omis](#method-20)
- [21. Interprétation des résultats enregistrés](#method-21)
- [22. Présentation et citations](#method-22)
- [23. Références](#method-23)
- [24. Effectif, puissance et effet](#method-24)
- [25. Méthodes CFA, SEM et PLS-SEM](#method-25)
- [26. Estimands de survie](#method-26)
- [27. Interprétation de la validation externe](#method-27)
- [28. Analyse importance–performance (IPA)](#method-28)

<a id="method-1"></a>

## 1. Niveau de mesure

Ne confondez pas codes numériques de catégories et mesures continues. Fixez le traitement des valeurs manquantes et les catégories de référence avant l’analyse ; tenez compte des différences d’échantillon entre résultats.

<a id="method-2"></a>

## 2. Descriptifs et fréquences

Vérifiez les dénominateurs des pourcentages et les valeurs manquantes. Examinez les petits effectifs attendus et l’indépendance ; distinguez significativité et force de l’association.

<a id="method-3"></a>

## 3. Tableaux croisés

Pour les tableaux croisés, vérifier les effectifs attendus et l’indépendance ; pour les tableaux clairsemés, vérifier les conditions des tests exacts.

<a id="method-4"></a>

## 4. Test t et ANOVA

Examinez indépendance, distribution des résidus et variances. L’ANCOVA exige une relation covariable–résultat et des pentes appropriées. Les tests non paramétriques ne comparent pas toujours les seules médianes.

$$
F=\frac{MS_{\mathrm{between}}}{MS_{\mathrm{within}}}
$$

<a id="method-5"></a>

## 5. Tests non paramétriques

Choisissez Mann–Whitney, Kruskal–Wallis, Wilcoxon ou Friedman selon les rangs et le plan. Distinguez groupes indépendants et appariés ; examinez ex æquo, différences nulles, tests exacts/asymptotiques et correction de continuité. Si les formes des distributions diffèrent, l’interprétation ne se limite pas aux médianes. Appliquez la correction choisie aux comparaisons multiples.

<a id="method-6"></a>

## 6. Données appariées et répétées

Appariez correctement les observations d’une même personne. Vérifiez les hypothèses propres au plan, dont la sphéricité, les corrections, interactions et comparaisons multiples. Le menu spécifique des traitements répétés est exclu de l’installateur public.

<a id="method-7"></a>

## 7. Corrélation

La corrélation n’établit ni causalité ni accord. Une cohérence interne élevée ne prouve pas l’unidimensionnalité. Précisez le modèle ICC, les mesures individuelles/moyennes et l’accord/la cohérence.

$$
z=\frac{1}{2}\log\frac{1+r}{1-r}
$$

<a id="method-8"></a>

## 8. Fidélité et accord

α et ω mesurent la fiabilité, sans prouver unidimensionnalité ou validité. Vérifier les items inversés et les covariances.

$$
\alpha=\frac{k}{k-1}\left(1-\frac{\sum_i\sigma_i^2}{\sigma_{\mathrm{total}}^2}\right)
$$

<a id="method-9"></a>

## 9. Analyse factorielle exploratoire

L’AFE estime des facteurs communs. Indiquer nombre de facteurs, extraction et rotation ; examiner charges croisées et unicités.

$$
\Sigma=\Lambda\Phi\Lambda^\prime+\Psi
$$

<a id="method-10"></a>

## 10. Composantes principales

L’ACP résume la variance observée en composantes et ne constitue pas un modèle de facteurs communs. La standardisation importe si les échelles diffèrent.

<a id="method-11"></a>

## 11. Régression linéaire

Les comparaisons hiérarchiques utilisent les cas complets communs du modèle final. Distinguez tests de variation OLS, tests de Wald HC3 et intervalles bootstrap de ΔR². Examinez colinéarité, observations influentes et résidus ; les notes expliquent uniquement les statistiques affichées.

$$
\hat\beta=(X^\prime X)^{-1}X^\prime y
$$

<a id="method-12"></a>

## 12. Régression hiérarchique

La régression hiérarchique prend en charge jusqu’à quatre blocs. Chaque étape conserve les variables des blocs précédents et ajoute le bloc suivant.

- Modèle 1: Bloc 1.
- Modèle 2: Bloc 1 + Bloc 2.
- Modèle 3: Bloc 1 + Bloc 2 + Bloc 3.
- Modèle 4: Bloc 1 + Bloc 2 + Bloc 3 + Bloc 4.

Comparez modèles réduit et complet sur les mêmes cas complets. ΔR²=R²full−R²reduced mesure l’apport du bloc ajouté. Le F de variation OLS diffère du Wald HC3 ; remplacer les erreurs-types ne rend pas le F robuste. En bootstrap, ajustez les deux modèles sur chaque même rééchantillonnage.

<a id="method-13"></a>

## 13. Médiation et modération

La médiation simple s’écrit M=aX+eM et Y=c′X+bM+eY ; l’effet indirect ab peut être significatif sans effet total significatif. Dans Y=b0+b1X+b2W+b3XW+e, l’effet conditionnel de X vaut b1+b3W. Centrer change le sens de W=0 sans garantir l’interaction. Interprétez pentes simples ou Johnson–Neyman dans l’étendue observée. BC n’inclut pas l’accélération BCa. Rapportez rôles, chemins, équations avec covariables, réplications demandées/valides et type d’intervalle.

<a id="method-14"></a>

## 14. Régression logistique

En régression logistique, exp(B) est un rapport de cotes. Vérifier catégorie de référence, séparation et cellules rares ; ne pas le confondre avec un risque relatif.

$$
\log\frac{p}{1-p}=X\beta,\qquad OR_j=\exp(\beta_j)
$$

<a id="method-15"></a>

## 15. Modèle linéaire généralisé

Le GLM exige une distribution et une fonction de lien. Vérifier surdispersion, offset et résidus ; interpréter les coefficients sur l’échelle du lien.

$$
g(\mu_i)=x_i^\prime\beta+\mathrm{offset}_i
$$

<a id="method-16"></a>

## 16. Régression régularisée

La régression régularisée ajoute à la perte résiduelle un terme limitant les coefficients. Ridge utilise L2, LASSO L1 et Elastic Net les combine. λ contrôle l’intensité, α le mélange. Précisez standardisation, partitions identiques de validation croisée et choix λ.min/λ.1se. Après sélection, n’utilisez pas les p ordinaires d’un OLS préspécifié ; examinez stabilité de sélection et prédiction externe.

$$
\frac{1}{2n}\|y-X\beta\|_2^2+\lambda\left[\frac{1-\alpha}{2}\|\beta\|_2^2+\alpha\|\beta\|_1\right]
$$

<a id="method-17"></a>

## 17. Modèles longitudinaux et de panel

Ne traitez pas les répétitions comme des cas indépendants. Distinguez effets moyens populationnels et conditionnels au sujet ; précisez structures de corrélation/effets aléatoires et plan. Les menus d’équations structurelles ne couvrent pas tous ces plans.

<a id="method-18"></a>

## 18. Enquêtes complexes

Spécifiez strates, PSU, poids et correction de population finie si applicable. Linéarisation de Taylor et poids répliqués diffèrent ; suivez le fournisseur. Estimez les sous-groupes comme domaines conservant le plan, sans simplement supprimer des lignes avant les erreurs-types. Rapportez degrés de liberté et traitement des PSU isolées.

<a id="method-19"></a>

## 19. Interprétation des seuils

.05, .30, .70 et les VIF de 5 ou 10 ne sont pas des seuils universels d’acceptation. Distinguez règles du logiciel et heuristiques bibliographiques ; considérez effectif, effet, intervalles, résidus et objectif scientifique. Un petit p ne signifie ni grand effet ni importance clinique.

<a id="method-20"></a>

## 20. Avertissements et résultats omis

Non-estimabilité, non-convergence, plan singulier et réplications invalides ne signifient pas non-significativité. Si le problème demeure, rapportez analyses/chemins omis et motifs. En régression, ≥80% de validité donne Adequate, 50–<80% Caution ; <50% ou moins de20 réplications valides suspend intervalles et p. PLS exige ≥80% de réplications complètes valides.

<a id="method-21"></a>

## 21. Interprétation des résultats enregistrés

Le fond est transparent, ou blanc si le format ne le permet pas. Les chemins avec p valide ≥ .05 sont en pointillés lorsque l’affichage de non-significativité est activé, contrairement aux chemins sans p. La couverture suit la langue de l’interface ; Free affiche le logo et StatEdu, Institute of Statistics. Vérifiez orientation, retours à la ligne, notes et graphiques résiduels appariés après exportation.

<a id="method-22"></a>

## 22. Présentation et citations

Citez les sources correspondant à l’estimateur et aux hypothèses ; précisez logiciel/version, échantillon, valeurs manquantes, codage, échelle d’effet et intervalles. L’accord numérique ne remplace pas une référence méthodologique. Référencez chaque méthode employée.

<a id="method-23"></a>

## 23. Références

The implementation and method notes are aligned with standard references and package documentation commonly used for applied statistical reporting:

| Reference | Connected method note |
|---|---|
| Agresti (2013) | Cross-tabulation, logistic regression, categorical GLMM interpretation |
| Bartlett (1954) | Bartlett test in factor analysis and PCA diagnostics |
| Breusch & Pagan (1979) | Regression residual homoscedasticity diagnostics |
| Cronbach (1951) | Cronbach's alpha |
| Curran, West, & Finch (1996) | Normality screening for t-test / ANOVA, correlation, reliability, and factor analysis |
| Durbin & Watson (1950, 1951) | Regression serial-correlation diagnostics |
| Efron & Tibshirani (1993) | Bootstrap inference |
| Fisher (1935) | t-test / ANOVA and general linear model background |
| Friedman, Hastie, & Tibshirani (2010) | `glmnet` regularization path |
| Kaiser (1974) | KMO in factor analysis and PCA |
| Levene (1960) | Equal-variance screening |
| MacKinnon & White (1985) | HC3 robust standard errors |
| Mardia (1970) | Multivariate normality |
| McDonald (1999) | Omega reliability |
| O'Brien (2007) | VIF interpretation |
| Shapiro & Wilk (1965) | Shapiro-Wilk normality test |
| Tibshirani (1996) | LASSO |
| White (1980) | Heteroskedasticity-consistent covariance |
| Zou & Hastie (2005) | Elastic Net |

- Agresti, A. (2013). *Categorical Data Analysis*.
- Altman, D. G., & Bland, J. M. (1983). Measurement in medicine: the analysis of method comparison studies.
- Bland, J. M., & Altman, D. G. (1986). Statistical methods for assessing agreement between two methods of clinical measurement.
- Bonett, D. G. (2002). Sample size requirements for estimating intraclass correlations with desired precision.
- Cohen, J. (1988). *Statistical Power Analysis for the Behavioral Sciences*.
- Faul, F., Erdfelder, E., Lang, A. G., & Buchner, A. (2007). G*Power 3.
- Fox, J., & Weisberg, S. (2019). *An R Companion to Applied Regression*.
- Harrell, F. E. (2015). *Regression Modeling Strategies*.
- Hsieh, F. Y., Bloch, D. A., & Larsen, M. D. (1998). A simple method of sample size calculation for linear and logistic regression.
- Kline, R. B. (2016). *Principles and Practice of Structural Equation Modeling*.
- Kraemer, H. C., & Thiemann, S. (1987). *How Many Subjects?*
- McHugh, M. L. (2012). Interrater reliability: the kappa statistic.
- McNeish, D. (2018). Thanks coefficient alpha, we'll take it from here.
- Mundry, R., & Nunn, C. L. (2009). Stepwise model fitting and statistical inference.
- Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for obtaining R2 from generalized linear mixed-effects models.
- Rosner, B. (2015). *Fundamentals of Biostatistics*.
- Tabachnick, B. G., & Fidell, L. S. (2019). *Using Multivariate Statistics*.
- West, S. G., Finch, J. F., & Curran, P. J. (1995). Structural equation models with nonnormal variables.
- Wilcox, R. R. (2017). *Introduction to Robust Estimation and Hypothesis Testing*.

<a id="method-24"></a>

## 24. Effectif, puissance et effet

α désigne l’erreur de type I, 1−β la puissance, n l’effectif, ρ la corrélation, σ l’écart-type et Δ la différence cible. Les formules antérieures sont conservées ci-dessous. Préspécifiez effet, allocation, attrition et corrélation. Les approximations de planification GEE/LMM/GLMM ou SEM/CFA ne sont pas une simulation complète réajustant chaque modèle ; examinez la sensibilité des plans complexes.

### 24.1 Symboles communs

$$
n_{\mathrm{recruit}}=\left\lceil \frac{n}{1-\mathrm{dropout}}\right\rceil
$$

### 24.2 Test t

$$
d=\frac{\bar{x}_1-\bar{x}_2}{s_p}
$$

$$
s_p=\sqrt{\frac{(n_1-1)s_1^2+(n_2-1)s_2^2}{n_1+n_2-2}}
$$

$$
g=Jd,\qquad J=1-\frac{3}{4df-1}
$$

$$
d_z=\frac{\bar{x}_D}{s_D}
$$

### 24.3 Proportions

$$
RD=p_1-p_2
$$

$$
RR=\frac{p_1}{p_2}
$$

$$
OR=\frac{p_1/(1-p_1)}{p_2/(1-p_2)}
$$

$$
h=2\arcsin(\sqrt{p_1})-2\arcsin(\sqrt{p_2})
$$

### 24.4 Chi carré

$$
w=\sqrt{\sum_i \frac{(p_i-p_{0i})^2}{p_{0i}}}
$$

$$
\lambda=Nw^2
$$

$$
\phi=\sqrt{\frac{\chi^2}{N}}
$$

$$
V=\sqrt{\frac{\chi^2}{N\min(r-1,c-1)}}
$$

### 24.5 Corrélation

$$
r=\operatorname{sign}(t)\sqrt{\frac{t^2}{t^2+df}}
$$

$$
r=\sqrt{\frac{F}{F+df_{\mathrm{error}}}}
$$

$$
z_r=\operatorname{atanh}(r)=\frac{1}{2}\log\left(\frac{1+r}{1-r}\right)
$$

$$
SE_z=\frac{1}{\sqrt{n-3}}
$$

$$
q=z_{r1}-z_{r2}
$$

### 24.6 ANOVA

$$
f=\sqrt{\frac{\eta^2}{1-\eta^2}}
$$

$$
f=\sqrt{\frac{\eta_p^2}{1-\eta_p^2}}
$$

$$
\eta_p^2=\frac{F\,df_{\mathrm{effect}}}{F\,df_{\mathrm{effect}}+df_{\mathrm{error}}}
$$

$$
\omega_p^2\approx
\frac{F\,df_{\mathrm{effect}}-df_{\mathrm{effect}}}
     {F\,df_{\mathrm{effect}}+df_{\mathrm{error}}+1}
$$

### 24.7 ANCOVA / MANOVA

$$
f_{\mathrm{adjusted}}=\frac{f}{\sqrt{1-R^2_{\mathrm{covariates}}}}
$$

$$
f=\sqrt{\frac{\eta_p^2}{1-\eta_p^2}}
$$

$$
f^2=\frac{V}{1-V},\qquad f=\sqrt{f^2}
$$

$$
\eta^2=1-\Lambda^{1/s},\qquad
f^2=\frac{\eta^2}{1-\eta^2}
$$

### 24.8 Non paramétriques

$$
r_{\mathrm{rb}}=1-\frac{2U}{n_1n_2}
$$

$$
\delta=P(X>Y)-P(X<Y)
$$

$$
\varepsilon^2=\frac{H(N+1)}{N^2-1}
$$

$$
W=\frac{\chi^2_F}{N(k-1)}
$$

### 24.9 McNemar

$$
OR=\frac{p_{01}}{p_{10}}
$$

$$
OR=\frac{b}{c},\qquad \log(OR)=\log(b)-\log(c)
$$

$$
g=\left|p_{01}-p_{10}\right|
$$

### 24.10 Régression

$$
f^2=\frac{R^2}{1-R^2}
$$

$$
f^2=\frac{R^2_{\mathrm{full}}-R^2_{\mathrm{reduced}}}{1-R^2_{\mathrm{full}}}
$$

$$
f^2=\frac{\Delta R^2}{1-\Delta R^2}
$$

$$
d\approx\frac{\log(OR)\sqrt{3}}{\pi}
$$

$$
ab=\beta_a\beta_b
$$

### 24.11 GEE

$$
OR_{\mathrm{GEE}}=\exp(B)
$$

$$
IRR_{\mathrm{GEE}}=\exp(B)
$$

$$
h=2\arcsin(\sqrt{p_1})-2\arcsin(\sqrt{p_2})
$$

### 24.12 LMM

$$
d_{\mathrm{LMM}}=\frac{B}{SD_{\mathrm{residual}}}
$$

$$
\eta_p^2=
\frac{F\cdot df_{\mathrm{effect}}}
     {F\cdot df_{\mathrm{effect}}+df_{\mathrm{error}}}
$$

$$
f=\sqrt{\frac{\eta_p^2}{1-\eta_p^2}}
$$

$$
d_z=\frac{\bar{x}_1-\bar{x}_2}
{\sqrt{s_1^2+s_2^2-2\,\mathrm{cov}_{12}}}
$$

$$
y_{gij}=d_{\mathrm{LMM}}g_it_j+b_i+\epsilon_{gij}
$$

$$
\widehat{\mathrm{power}}=\frac{1}{S}\sum_{s=1}^{S}I(p_s<\alpha)
$$

### 24.13 GLMM

$$
OR_{\mathrm{GLMM}}=\exp(B),\qquad B=\log(OR_{\mathrm{GLMM}})
$$

$$
d_{\mathrm{latent}}\approx\frac{B\sqrt{3}}{\pi}
=\frac{\log(OR)\sqrt{3}}{\pi}
$$

$$
IRR_{\mathrm{GLMM}}=\exp(B),\qquad B=\log(IRR_{\mathrm{GLMM}})
$$

$$
d_{\mathrm{GLMM}}=\frac{B}{SD_{\mathrm{residual}}}
$$

### 24.14 Survie / Cox

$$
\log(HR)
$$

$$
E\approx
\frac{(z_{1-\alpha/2}+z_{1-\beta})^2}
     {p_1p_2[\log(HR)]^2}
$$

### 24.15 Équivalence et non-infériorité

$$
D_{\mathrm{equiv}}=\Delta-|\hat{\theta}|
$$

$$
D_{\mathrm{NI}}=\Delta+\hat{\theta}
$$

### 24.16 ROC AUC et précision diagnostique

$$
\Delta_{\mathrm{AUC}}=AUC-AUC_0
$$

$$
d\approx\sqrt{2}\,\Phi^{-1}(AUC)
$$

### 24.17 Régression des comptes et taux

$$
IRR=\exp(B),\qquad B=\log(IRR)
$$

$$
\operatorname{Var}(Y)=\mu
$$

$$
\operatorname{Var}(Y)=\mu+\alpha\mu^2
$$

$$
RR_{\mathrm{rate}}=\frac{\lambda_1}{\lambda_2}
$$

### 24.18 Essais en grappes

$$
DE=1+(m-1)ICC
$$

$$
n_{\mathrm{clustered}}\approx n_{\mathrm{individual}}DE
$$

### 24.19 Précision et intervalles

$$
n=\left(\frac{z\,SD}{h}\right)^2
$$

$$
n=\frac{z^2p(1-p)}{h^2}
$$

$$
z_r=\operatorname{atanh}(r),\qquad SE_z=\frac{1}{\sqrt{n-3}}
$$

### 24.20 Fidélité et accord

$$
\log(1-\alpha)
$$

$$
n\ge items+1
$$

### 24.21 SEM / CFA

$$
\lambda=(N-1)\,df\,RMSEA^2
$$

$$
\Delta_{\mathrm{RMSEA}}=RMSEA_A-RMSEA_0
$$

$$
z=\operatorname{atanh}(\theta)
$$

$$
M_{\mathrm{obs}}=\frac{p(p+1)}{2}
$$

$$
q\approx 2p+k+s
$$

$$
df\approx M_{\mathrm{obs}}-q
$$

<a id="method-25"></a>

## 25. Méthodes CFA, SEM et PLS-SEM

Adaptez l’estimateur au niveau de mesure. Vérifiez convergence, variances négatives, fortes corrélations factorielles et défauts d’ajustement locaux. Ne supposez ni libération automatique des contraintes d’invariance partielle ni regroupement d’items entièrement automatique.

Les modèles actuels couvrent des données transversales indépendantes. Ne les interprétez pas comme des SEM multiniveaux, longitudinaux ou d’enquêtes complexes. Examinez l’invariance avant les comparaisons de chemins entre groupes ; les modèles modérés multigroupes ont des restrictions.

N’appliquez pas PLSc aux modèles formatifs. L’inférence bootstrap est restreinte si moins de 80% des réplications complètes sont valides. Distinguez inférence des effets et bootstrap d’ajustement exact ; vérifiez conditions et avertissements de MICOM/MGA et validation croisée.

$$
\Sigma=\Lambda\Phi\Lambda^\prime+\Theta
$$

<a id="method-26"></a>

## 26. Estimands de survie

S(t) est la probabilité de rester sans événement jusqu’à t ; RMST intègre S(t) jusqu’à τ. exp(β) dans Cox est un rapport de risques instantanés, pas de probabilités de survie. Vérifiez censure indépendante, risques proportionnels et origine temporelle. CIF, HR par cause et SHR de sous-distribution ciblent des quantités différentes.

<a id="method-27"></a>

## 27. Interprétation de la validation externe

L’accord sur certains indices ne prouve pas une équivalence universelle. Harmonisez données, codage, estimateur, valeurs manquantes et tolérances. Par exemple, 49 des 50 modèles AMOS comparables concordent ; 41 des 2,895 valeurs LMM de SPSS diffèrent encore. Les données sources détaillées et journaux internes ne sont pas inclus dans la documentation publique.

Les analyses générales, régressions, analyses longitudinales et de survie ont été comparées à SPSS ; CFA/SEM à AMOS ; PLS-SEM/PLSc et CB-SEM à SmartPLS. Ces validations cumulées sont intégrées à 1.3.0 ; la page Validation résume conditions et différences restantes.

Examinez puis ajoutez les résultats à la collection. HTML, PDF, Word et Excel sont disponibles. HWPX apparaît uniquement dans les résultats cumulés en interface coréenne et est écrit directement, sans Word ni Hancom. Word/HWPX proposent tableaux principaux, annexes, explications et figures; les tableaux principaux sont présélectionnés. La capture est enregistrée sans recalcul. Free utilise 300 dpi, le développement 600 dpi. HTML inclut couverture et liste de tableaux avec liens.

<a id="method-28"></a>

## 28. Analyse importance–performance (IPA)

L’importance dérivée est une corrélation partielle de Pearson ajustée sur les autres attributs. Le logarithme IPA révisé exige des valeurs positives et conserve les signes négatifs. Utilisez les cas complets communs par groupe ou les paires communes. Les différences, deuxième moins premier, utilisent Welch ou t apparié. Holm ajuste les p finis affichés des scores directs; les intervalles 95% restent individuels. Importance dérivée et différences utilisent un bootstrap percentile exigeant au moins 80% et 50 réplications valides; aucun p de t n’est attribué aux différences de corrélations partielles. La référence commune est une moyenne de coordonnées pondérée par les répondants, pas une corrélation partielle regroupée. Définissez les groupes dans IPA; segmenter le fichier produit des analyses séparées.
