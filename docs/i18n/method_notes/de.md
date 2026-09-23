## 1. Messniveau

Verwechseln Sie numerische Kategoriencodes nicht mit kontinuierlichen Messwerten. Legen Sie Fehlwertbehandlung und Referenzkategorien vorab fest; berücksichtigen Sie unterschiedliche Stichproben beim Ergebnisvergleich.

## 2. Deskriptivstatistik und Häufigkeiten

Prüfen Sie Prozentnenner und Fehlwertbehandlung. Beachten Sie kleine erwartete Häufigkeiten und Unabhängigkeit; unterscheiden Sie Signifikanz und Zusammenhangsstärke.

## 3. Kreuztabellen

Bei Kreuztabellen erwartete Häufigkeiten und Unabhängigkeit prüfen; bei dünn besetzten Tabellen die Voraussetzungen exakter Tests beachten.

## 4. t-Test und ANOVA

Prüfen Sie Unabhängigkeit, Residuenverteilung und Varianzen. ANCOVA verlangt einen angemessenen Zusammenhang zwischen Kovariate und Ergebnis sowie geeignete Steigungen. Nichtparametrische Tests vergleichen nicht immer nur Mediane.

$$
F=\frac{MS_{\mathrm{between}}}{MS_{\mathrm{within}}}
$$

## 5. Nichtparametrische Tests

Wählen Sie Mann–Whitney, Kruskal–Wallis, Wilcoxon oder Friedman nach Rangdaten und Design. Unterscheiden Sie unabhängige und gepaarte Gruppen; prüfen Sie Bindungen, Nulldifferenzen, exakte/asymptotische Tests und Kontinuitätskorrektur. Unterschiedliche Verteilungsformen erlauben keine reine Medianinterpretation. Verwenden Sie die gewählte Mehrfachkorrektur.

## 6. Gepaarte Daten und Messwiederholungen

Ordnen Sie Messungen derselben Person korrekt zu. Prüfen Sie designspezifische Annahmen einschließlich Sphärizität, Korrekturen, Interaktionen und Mehrfachvergleiche. Das spezielle Menü für wiederholte Behandlungen fehlt im öffentlichen Installer.

## 7. Korrelation

Korrelation belegt weder Kausalität noch Übereinstimmung. Hohe interne Konsistenz allein belegt keine Eindimensionalität. Nennen Sie ICC-Modell, Einzel-/Durchschnittsmessung und Übereinstimmung/Konsistenz.

$$
z=\frac{1}{2}\log\frac{1+r}{1-r}
$$

## 8. Reliabilität und Übereinstimmung

α und ω messen Reliabilität, beweisen aber weder Eindimensionalität noch Validität. Umpolung und Kovarianzen der Items prüfen.

$$
\alpha=\frac{k}{k-1}\left(1-\frac{\sum_i\sigma_i^2}{\sigma_{\mathrm{total}}^2}\right)
$$

## 9. Explorative Faktorenanalyse

Die EFA schätzt gemeinsame Faktoren. Faktorzahl, Extraktion und Rotation berichten; Kreuzladungen und Einzigartigkeiten prüfen.

$$
\Sigma=\Lambda\Phi\Lambda^\prime+\Psi
$$

## 10. Hauptkomponenten

Die PCA fasst beobachtete Varianz in Komponenten zusammen und ist kein gemeinsames Faktorenmodell. Bei unterschiedlichen Skalen beeinflusst Standardisierung das Ergebnis.

## 11. Lineare Regression

Hierarchische Vergleiche verwenden gemeinsame vollständige Fälle des Endmodells. Unterscheiden Sie OLS-Änderungstests, HC3-Wald-Tests und Bootstrap-Intervalle für ΔR². Prüfen Sie Kollinearität, einflussreiche Fälle und Residuen; Anmerkungen erläutern nur angezeigte Kennwerte.

$$
\hat\beta=(X^\prime X)^{-1}X^\prime y
$$

## 12. Hierarchische Regression

Die hierarchische Regression unterstützt bis zu vier Blöcke. Jeder Schritt behält die Variablen der vorherigen Blöcke bei und fügt den nächsten Block hinzu.

- Modell 1: Block 1.
- Modell 2: Block 1 + Block 2.
- Modell 3: Block 1 + Block 2 + Block 3.
- Modell 4: Block 1 + Block 2 + Block 3 + Block 4.

Vergleichen Sie reduziertes und vollständiges Modell anhand derselben vollständigen Fälle. ΔR²=R²full−R²reduced beschreibt den zusätzlichen Blockbeitrag. OLS-F-Änderungstest und HC3-Wald-Test unterscheiden sich; robuste Standardfehler allein machen den F-Test nicht robust. Passen Sie im Bootstrap beide Modelle an dieselbe jeweilige Stichprobe an.

## 13. Mediation und Moderation

Einfache Mediation lautet M=aX+eM und Y=c′X+bM+eY; der indirekte Effekt ab kann ohne signifikanten Gesamteffekt signifikant sein. Bei Y=b0+b1X+b2W+b3XW+e ist der bedingte X-Effekt b1+b3W. Zentrierung ändert die Bedeutung von W=0, garantiert aber keine Interaktion. Interpretieren Sie einfache Steigungen oder Johnson–Neyman nur im beobachteten Bereich. BC umfasst keine BCa-Beschleunigungskorrektur. Berichten Sie Rollen, Pfade, Kovariatengleichungen, angeforderte/gültige Wiederholungen und Intervallmethode.

## 14. Logistische Regression

Bei logistischer Regression ist exp(B) das Odds Ratio. Referenzkategorie, Separation und dünn besetzte Zellen prüfen; nicht mit relativem Risiko verwechseln.

$$
\log\frac{p}{1-p}=X\beta,\qquad OR_j=\exp(\beta_j)
$$

## 15. Verallgemeinertes lineares Modell

Ein GLM benötigt Antwortverteilung und Linkfunktion. Überdispersion, Offset und Residuen prüfen; Koeffizienten auf der Linkskala interpretieren.

$$
g(\mu_i)=x_i^\prime\beta+\mathrm{offset}_i
$$

## 16. Regularisierte Regression

Regularisierte Regression ergänzt den Residuenverlust um eine Begrenzung der Koeffizienten. Ridge nutzt L2, LASSO L1, Elastic Net beide. λ steuert die Stärke, α die Mischung. Dokumentieren Sie Standardisierung, identische Kreuzvalidierungsaufteilungen und λ.min/λ.1se. Nach Variablenauswahl gelten gewöhnliche p-Werte eines vorab festgelegten OLS nicht automatisch; prüfen Sie Auswahlstabilität und externe Vorhersage.

$$
\frac{1}{2n}\|y-X\beta\|_2^2+\lambda\left[\frac{1-\alpha}{2}\|\beta\|_2^2+\alpha\|\beta\|_1\right]
$$

## 17. Längsschnitt- und Panelmodelle

Behandeln Sie wiederholte Beobachtungen nicht als unabhängige Fälle. Unterscheiden Sie populationsgemittelte und personenspezifisch bedingte Effekte; nennen Sie Korrelations-/Zufallseffektstruktur und Design. Die SEM-Menüs unterstützen nicht alle diese Designs.

## 18. Komplexe Stichproben

Geben Sie Schichten, PSU, Gewichte und gegebenenfalls endliche Populationskorrektur an. Taylor-Linearisierung und Replikationsgewichte unterscheiden sich; folgen Sie dem Datenanbieter. Schätzen Sie Teilgruppen als Domänen unter Beibehaltung des Designs, statt vor der Standardfehlerberechnung nur Zeilen zu löschen. Berichten Sie Designfreiheitsgrade und einzelne PSU.

## 19. Interpretation von Schwellenwerten

.05, .30, .70 sowie VIF 5 oder 10 sind keine universellen Annahmegrenzen. Unterscheiden Sie Softwarewarnungen und Literaturheuristiken; berücksichtigen Sie Stichprobe, Effekt, Intervalle, Residuen und Forschungsziel. Kleine p-Werte bedeuten weder große Effekte noch klinische Bedeutung.

## 20. Warnungen und ausgelassene Ergebnisse

Nichtschätzbarkeit, fehlende Konvergenz, singuläre Designs und ungültige Wiederholungen bedeuten nicht Nichtsignifikanz. Berichten Sie ungelöste Auslassungen und Gründe. Bei Regression ist ≥80% Gültigkeit Adequate, 50–<80% Caution; <50% oder weniger als20 gültige Wiederholungen unterdrücken Intervalle und p. PLS erfordert ≥80% gültige vollständige Wiederholungen.

## 21. Interpretation gespeicherter Ergebnisse

Der Hintergrund ist transparent, andernfalls weiß, wenn das Format Transparenz nicht unterstützt. Bei aktivierter Nichtsignifikanzanzeige sind Pfade mit gültigem p ≥ .05 gestrichelt, anders als Pfade ohne p. Das Deckblatt folgt der UI-Sprache; Free zeigt Logo und StatEdu, Institute of Statistics. Prüfen Sie Ausrichtung, Umbrüche, Anmerkungen und gepaarte Residuenplots nach dem Export.

## 22. Methodenbericht und Zitate

Zitieren Sie Quellen passend zu Schätzer und Annahmen; nennen Sie Software/Version, Stichprobe, Fehlwerte, Kodierung, Effektskala und Intervalle. Zahlenübereinstimmung ersetzt keine methodische Quelle. Belegen Sie unterschiedliche Methoden getrennt.

## 23. Literatur

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

## 24. Stichprobe, Teststärke und Effekt

α ist Fehler erster Art, 1−β Teststärke, n Stichprobengröße, ρ Korrelation, σ Standardabweichung und Δ Zielunterschied. Die bisherigen Formeln bleiben unten erhalten. Legen Sie Effekt, Zuteilung, Ausfallrate und Korrelation vorab fest. GEE/LMM/GLMM- oder SEM/CFA-Planungsnäherungen sind keine vollständige Datensimulation mit erneuter Anpassung jedes Modells; prüfen Sie komplexe Designs durch Sensitivitätsanalysen.

### 24.1 Gemeinsame Symbole

$$
n_{\mathrm{recruit}}=\left\lceil \frac{n}{1-\mathrm{dropout}}\right\rceil
$$

### 24.2 t-Test

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

### 24.3 Anteile

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

### 24.4 Chi-Quadrat

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

### 24.5 Korrelation

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

### 24.8 Nichtparametrisch

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

### 24.10 Regression

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

### 24.14 Überleben / Cox

$$
\log(HR)
$$

$$
E\approx
\frac{(z_{1-\alpha/2}+z_{1-\beta})^2}
     {p_1p_2[\log(HR)]^2}
$$

### 24.15 Äquivalenz und Nichtunterlegenheit

$$
D_{\mathrm{equiv}}=\Delta-|\hat{\theta}|
$$

$$
D_{\mathrm{NI}}=\Delta+\hat{\theta}
$$

### 24.16 ROC AUC und diagnostische Genauigkeit

$$
\Delta_{\mathrm{AUC}}=AUC-AUC_0
$$

$$
d\approx\sqrt{2}\,\Phi^{-1}(AUC)
$$

### 24.17 Zähl- und Ratenregression

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

### 24.18 Clusterstudien

$$
DE=1+(m-1)ICC
$$

$$
n_{\mathrm{clustered}}\approx n_{\mathrm{individual}}DE
$$

### 24.19 Präzision und Intervalle

$$
n=\left(\frac{z\,SD}{h}\right)^2
$$

$$
n=\frac{z^2p(1-p)}{h^2}
$$

$$
z_r=\operatorname{atanh}(r),\qquad SE_z=\frac{1}{\sqrt{n-3}}
$$

### 24.20 Reliabilität und Übereinstimmung

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

## 25. CFA-, SEM- und PLS-SEM-Methoden

Passen Sie den Schätzer dem Messniveau an. Prüfen Sie Konvergenz, negative Varianzen, hohe Faktorkorrelationen und lokale Fehlanpassung. Setzen Sie keine automatische Freigabe partieller Invarianzrestriktionen oder vollautomatische Item-Paketbildung voraus.

Aktuelle Strukturmodelle gelten für unabhängige Querschnittsdaten. Interpretieren Sie sie nicht als Mehrebenen-, komplexe Survey- oder Längsschnitt-SEM. Prüfen Sie Messinvarianz vor Gruppenpfadvergleichen; moderierte Mehrgruppenmodelle unterliegen Einschränkungen.

Wenden Sie PLSc nicht auf formative Modelle an. Bei weniger als 80% gültigen vollständigen Wiederholungen wird Bootstrap-Inferenz eingeschränkt. Unterscheiden Sie Effektinferenz vom Bootstrap für exakte Anpassung; prüfen Sie MICOM/MGA- und Kreuzvalidierungsbedingungen sowie Warnungen.

$$
\Sigma=\Lambda\Phi\Lambda^\prime+\Theta
$$

## 26. Zielgrößen der Überlebenszeitanalyse

S(t) ist die Wahrscheinlichkeit, bis t ereignisfrei zu bleiben; RMST integriert S(t) bis τ. exp(β) bei Cox ist ein Hazardverhältnis, kein Verhältnis von Überlebenswahrscheinlichkeiten. Prüfen Sie unabhängige Zensierung, proportionale Hazards und Zeitursprung. CIF, ursachenspezifische HR und Subdistributions-SHR haben unterschiedliche Zielgrößen.

## 27. Interpretation externer Validierung

Übereinstimmung einzelner Indizes belegt keine allgemeine Gleichwertigkeit. Stimmen Sie Daten, Kodierung, Schätzer, Fehlwerte und Toleranzen ab. Beispielsweise stimmten 49 von 50 vergleichbaren AMOS-Modellen überein; 41 von 2,895 SPSS-LMM-Werten wichen ab. Detaillierte Quelldaten und interne Ausführungsprotokolle sind nicht Teil der öffentlichen Dokumentation.

Allgemeine Analysen, Regressionen, Längsschnitt- und Überlebenszeitanalysen wurden mit SPSS verglichen; CFA/SEM mit AMOS; PLS-SEM/PLSc und CB-SEM mit SmartPLS. Diese kumulierten Prüfungen sind in 1.3.0 eingeflossen; die Validierungsseite fasst Bedingungen und verbleibende Unterschiede zusammen.

Prüfen und sammeln Sie Ergebnisse. HTML, PDF, Word und Excel werden unterstützt. HWPX ist nur bei koreanischer Oberfläche in gesammelten Ergebnissen verfügbar und wird ohne Word/Hancom direkt geschrieben. Word/HWPX bieten Haupttabellen, Anhänge, Erläuterungen und Abbildungen; Haupttabellen sind vorausgewählt. Gespeichert wird die erfasste Darstellung ohne Neuberechnung. Free nutzt 300 dpi, Entwicklung 600 dpi. HTML enthält Deckblatt und verlinkte Tabellenliste.

## 28. Wichtigkeits–Leistungsanalyse (IPA)

Abgeleitete Wichtigkeit ist eine Pearson-Partialkorrelation unter Kontrolle anderer Leistungsattribute. Die logarithmische revidierte IPA erfordert positive Werte und erhält negative Vorzeichen. Verwendet werden gemeinsame vollständige Fälle je Gruppe oder gemeinsame Paare. Mittelwertdifferenzen, zweiter minus erster, nutzen Welch oder gepaarten t-Test. Holm korrigiert angezeigte endliche p-Werte direkter Bewertungen;95%-Intervalle bleiben punktweise. Abgeleitete Wichtigkeit und Differenzen verwenden Perzentil-Bootstrap mit mindestens 80% und 50 gültigen Wiederholungen; Partialkorrelationsdifferenzen erhalten keinen t-Test-p-Wert. Die gemeinsame Referenz ist ein nach Befragten gewichtetes Koordinatenmittel, keine gepoolte Partialkorrelation. Gruppenvergleiche werden in IPA definiert; Dateiaufteilung erzeugt separate Analysen.
