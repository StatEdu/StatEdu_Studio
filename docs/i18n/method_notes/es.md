## 1. Nivel de medida

No confunda códigos numéricos de categorías con medidas continuas. Defina el tratamiento de valores faltantes y las categorías de referencia antes del análisis; considere las diferencias de muestra al comparar resultados.

## 2. Descriptivos y frecuencias

Revise los denominadores de los porcentajes y el tratamiento de valores faltantes. Examine frecuencias esperadas pequeñas e independencia; distinga significación estadística de intensidad de asociación.

## 3. Tablas cruzadas

En tablas de contingencia, compruebe frecuencias esperadas e independencia; en tablas dispersas, revise las condiciones de las pruebas exactas.

## 4. Prueba t y ANOVA

Examine independencia, distribución residual y varianzas. ANCOVA requiere una relación covariable–resultado y pendientes apropiadas. Las pruebas no paramétricas no siempre comparan únicamente medianas.

$$
F=\frac{MS_{\mathrm{between}}}{MS_{\mathrm{within}}}
$$

## 5. Pruebas no paramétricas

Use Mann–Whitney, Kruskal–Wallis, Wilcoxon o Friedman según los rangos y el diseño. Distinga grupos independientes y pareados; compruebe empates, diferencias cero, pruebas exactas/asintóticas y corrección de continuidad. Con formas distribucionales distintas, el resultado no se reduce a diferencias de mediana. Aplique la corrección especificada en comparaciones múltiples.

## 6. Datos pareados y medidas repetidas

Empareje correctamente las observaciones de cada persona. Revise supuestos específicos, incluida la esfericidad, correcciones, interacciones y comparaciones múltiples. El menú específico de tratamientos repetidos se excluye del instalador público.

## 7. Correlación

La correlación no establece causalidad ni concordancia. Una consistencia interna alta no demuestra unidimensionalidad. Especifique el modelo ICC, medidas individuales/promedio y concordancia/consistencia.

$$
z=\frac{1}{2}\log\frac{1+r}{1-r}
$$

## 8. Fiabilidad y concordancia

α y ω miden fiabilidad, pero no demuestran unidimensionalidad ni validez. Compruebe ítems invertidos y covarianzas.

$$
\alpha=\frac{k}{k-1}\left(1-\frac{\sum_i\sigma_i^2}{\sigma_{\mathrm{total}}^2}\right)
$$

## 9. Análisis factorial exploratorio

El AFE estima factores comunes. Informe número de factores, extracción y rotación; revise cargas cruzadas y unicidades.

$$
\Sigma=\Lambda\Phi\Lambda^\prime+\Psi
$$

## 10. Componentes principales

El ACP resume varianza observada en componentes y no equivale al modelo factorial común. La estandarización importa cuando las escalas difieren.

## 11. Regresión lineal

Las comparaciones jerárquicas usan los casos completos comunes del modelo final. Distinga pruebas de cambio OLS, pruebas Wald HC3 e intervalos bootstrap de ΔR². Revise colinealidad, observaciones influyentes y residuos; las notas explican solo estadísticas mostradas.

$$
\hat\beta=(X^\prime X)^{-1}X^\prime y
$$

## 12. Regresión jerárquica

La regresión jerárquica admite hasta cuatro bloques. Cada paso conserva las variables de los bloques anteriores y añade el siguiente bloque.

- Modelo 1: Bloque 1.
- Modelo 2: Bloque 1 + Bloque 2.
- Modelo 3: Bloque 1 + Bloque 2 + Bloque 3.
- Modelo 4: Bloque 1 + Bloque 2 + Bloque 3 + Bloque 4.

Compare modelos reducido y completo en los mismos casos completos. ΔR²=R²full−R²reduced cuantifica la contribución del bloque añadido. El F de cambio OLS y Wald HC3 son distintos; sustituir errores estándar no vuelve robusto el F de cambio. En bootstrap ajuste ambos modelos en la misma remuestra.

## 13. Mediación y moderación

La mediación simple se expresa como M=aX+eM e Y=c′X+bM+eY; el efecto indirecto es ab, que puede ser significativo aunque el total no lo sea. En Y=b0+b1X+b2W+b3XW+e, el efecto condicional de X es b1+b3W. Centrar cambia el significado de W=0, sin garantizar interacción. Interprete pendientes simples o Johnson–Neyman dentro del rango observado. BC no incluye la corrección de aceleración BCa. Informe roles, rutas, ecuaciones con covariables, remuestras solicitadas/válidas y método de intervalo.

## 14. Regresión logística

En regresión logística, exp(B) es la razón de momios. Revise categoría de referencia, separación y celdas dispersas; no la confunda con riesgo relativo.

$$
\log\frac{p}{1-p}=X\beta,\qquad OR_j=\exp(\beta_j)
$$

## 15. Modelo lineal generalizado

El GLM requiere distribución de respuesta y función de enlace. Revise sobredispersión, offset y residuos, e interprete coeficientes en la escala del enlace.

$$
g(\mu_i)=x_i^\prime\beta+\mathrm{offset}_i
$$

## 16. Regresión regularizada

La regresión regularizada añade a la pérdida residual un término que limita los coeficientes. Ridge usa L2, LASSO L1 y Elastic Net combina ambos. λ controla la intensidad y α la mezcla. Informe estandarización, las mismas particiones de validación cruzada y la elección λ.min o λ.1se. Tras seleccionar variables, no interprete los coeficientes con los p ordinarios de un OLS preespecificado; examine estabilidad de selección y predicción externa.

$$
\frac{1}{2n}\|y-X\beta\|_2^2+\lambda\left[\frac{1-\alpha}{2}\|\beta\|_2^2+\alpha\|\beta\|_1\right]
$$

## 17. Modelos longitudinales y de panel

No trate medidas repetidas como casos independientes. Distinga efectos promedio poblacionales y condicionales al sujeto; especifique estructuras de correlación/efectos aleatorios y diseño. Los menús de ecuaciones estructurales no admiten todos estos diseños.

## 18. Encuestas complejas

Especifique estratos, PSU, pesos y, cuando corresponda, corrección por población finita. La linealización de Taylor y los pesos replicados son distintos; siga las instrucciones del proveedor. Estime subgrupos como dominios conservando el diseño, sin eliminar simplemente filas antes de calcular errores estándar. Informe grados de libertad y tratamiento de PSU solitarias.

## 19. Interpretación de umbrales

Valores como .05, .30, .70 o VIF de 5 y 10 no son criterios universales de aprobación. Distinga reglas del programa y heurísticas bibliográficas; considere muestra, efecto, intervalos, residuos y objetivo científico. Un p pequeño no implica un efecto grande ni importancia clínica.

## 20. Advertencias y resultados omitidos

No estimabilidad, falta de convergencia, diseño singular y remuestras inválidas no significan ausencia de significación. Si persisten, informe análisis/rutas omitidos y motivos. En regresión, validez ≥80% es Adequate; 50–<80% es Caution; <50% o menos de20 remuestras válidas suprime intervalos y p. PLS exige ≥80% de remuestras completas válidas para inferencia.

## 21. Interpretación de resultados guardados

Use fondo transparente, o blanco si el formato no admite transparencia. Las rutas con p válido ≥ .05 son discontinuas al activar la visualización de no significación; se distinguen de rutas sin p. La portada sigue el idioma de la interfaz; Free muestra el logotipo y StatEdu, Institute of Statistics. Revise orientación, saltos, notas y gráficos residuales emparejados tras exportar.

## 22. Informe y citas metodológicas

Cite fuentes correspondientes al estimador y sus supuestos; informe software y versión, muestra, faltantes, codificación, escala del efecto e intervalos. La coincidencia numérica no sustituye una referencia metodológica. Cite por separado los distintos métodos utilizados.

## 23. Referencias

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

## 24. Tamaño muestral, potencia y efecto

α es error de tipo I, 1−β potencia, n tamaño muestral, ρ correlación, σ desviación estándar y Δ diferencia objetivo. Se conservan a continuación las fórmulas de las notas anteriores. Preespecifique efecto, asignación, abandono y correlación. Las aproximaciones de planificación GEE/LMM/GLMM o SEM/CFA no equivalen a simular datos completos y reajustar cada modelo; en diseños complejos examine sensibilidad.

### 24.1 Símbolos comunes

$$
n_{\mathrm{recruit}}=\left\lceil \frac{n}{1-\mathrm{dropout}}\right\rceil
$$

### 24.2 Prueba t

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

### 24.3 Proporciones

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

### 24.4 Chi-cuadrado

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

### 24.5 Correlación

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

### 24.8 No paramétricos

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

### 24.10 Regresión

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

### 24.14 Supervivencia / Cox

$$
\log(HR)
$$

$$
E\approx
\frac{(z_{1-\alpha/2}+z_{1-\beta})^2}
     {p_1p_2[\log(HR)]^2}
$$

### 24.15 Equivalencia y no inferioridad

$$
D_{\mathrm{equiv}}=\Delta-|\hat{\theta}|
$$

$$
D_{\mathrm{NI}}=\Delta+\hat{\theta}
$$

### 24.16 ROC AUC y exactitud diagnóstica

$$
\Delta_{\mathrm{AUC}}=AUC-AUC_0
$$

$$
d\approx\sqrt{2}\,\Phi^{-1}(AUC)
$$

### 24.17 Regresión de recuentos y tasas

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

### 24.18 Ensayos por conglomerados

$$
DE=1+(m-1)ICC
$$

$$
n_{\mathrm{clustered}}\approx n_{\mathrm{individual}}DE
$$

### 24.19 Precisión e intervalos

$$
n=\left(\frac{z\,SD}{h}\right)^2
$$

$$
n=\frac{z^2p(1-p)}{h^2}
$$

$$
z_r=\operatorname{atanh}(r),\qquad SE_z=\frac{1}{\sqrt{n-3}}
$$

### 24.20 Fiabilidad y concordancia

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

## 25. Métodos CFA, SEM y PLS-SEM

Adecue el estimador al nivel de medida. Revise convergencia, varianzas negativas, correlaciones factoriales altas y desajuste local. No suponga liberación automática de restricciones para invariancia parcial ni parcelación completamente automática.

Los modelos actuales cubren datos transversales independientes. No los interprete como SEM multinivel, longitudinal o de encuestas complejas. Examine invariancia antes de comparar rutas entre grupos; los modelos moderados multigrupo tienen restricciones.

No aplique PLSc a modelos formativos. La inferencia bootstrap se restringe si menos del 80% de las remuestras completas son válidas. Distinga inferencia de efectos del bootstrap de ajuste exacto; revise condiciones y advertencias de MICOM/MGA y validación cruzada.

$$
\Sigma=\Lambda\Phi\Lambda^\prime+\Theta
$$

## 26. Estimandos de supervivencia

S(t) es la probabilidad de seguir libre del evento hasta t; RMST integra S(t) hasta τ. exp(β) en Cox es una razón de riesgos instantáneos, no de probabilidades de supervivencia. Revise censura independiente, riesgos proporcionales y origen temporal. CIF, HR de causa específica y SHR de subdistribución tienen objetivos diferentes.

## 27. Interpretación de la validación externa

La coincidencia de ciertos índices no establece equivalencia universal. Iguale datos, codificación, estimador, faltantes y tolerancias. Por ejemplo, coincidieron 49 de 50 modelos AMOS comparables; persistieron diferencias en 41 de 2,895 valores LMM de SPSS. Los datos originales detallados y registros internos no forman parte de la documentación pública.

Se compararon análisis generales, regresión, longitudinales y supervivencia con SPSS; CFA/SEM con AMOS; y PLS-SEM/PLSc y CB-SEM con SmartPLS. Son validaciones acumuladas incorporadas en 1.3.0; la página de Validación resume condiciones y diferencias pendientes.

Revise y añada los resultados a la colección. Se admiten HTML, PDF, Word y Excel. HWPX solo aparece en resultados acumulados con interfaz coreana y se escribe directamente, sin Word ni Hancom. Word/HWPX permiten tablas principales, anexos, explicaciones y figuras; por defecto se seleccionan las principales. Se guarda la captura sin recalcular. Free usa 300 dpi y desarrollo 600 dpi. HTML incluye portada y lista enlazada de tablas.

## 28. Análisis importancia–desempeño (IPA)

La importancia derivada es correlación parcial de Pearson ajustada por los demás atributos. El logaritmo de IPA revisado requiere valores positivos y conserva signos negativos. Se usan casos completos comunes por grupo o pares comunes entre tiempos. Las diferencias, segundo menos primero, usan Welch o t pareada. Holm ajusta los p finitos mostrados de puntuaciones directas; los intervalos 95% son individuales. La importancia derivada y sus diferencias usan bootstrap percentil con al menos 80% y 50 réplicas válidas; no se asigna p de t a diferencias de correlaciones parciales. La referencia común es media de coordenadas ponderada por participantes, no correlación parcial conjunta. Defina los grupos dentro de IPA; dividir el archivo produce análisis separados.
