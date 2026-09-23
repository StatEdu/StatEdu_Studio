# Análisis — StatEdu Studio 1.3.0

Alcance y resultados de StatEdu Studio 1.3.0, edición pública.

## Índice

1. [Alcance público de 1.3.0](#scope)
2. [Preparación de datos](#data)
3. [Frecuencias, descriptivos y tablas cruzadas](#descriptive)
4. [Comparaciones de grupos y ANCOVA](#group)
5. [Medidas pareadas y repetidas mixtas](#paired)
6. [Correlación, fiabilidad y concordancia](#correlation)
7. [Análisis factorial exploratorio y PCA](#factor)
8. [Análisis importancia–desempeño (IPA)](#ipa)
9. [Regresión y regresión jerárquica](#regression)
10. [Efectos de mediación y moderación](#mediation)
11. [Regresión logística, GLM y penalizada](#generalized)
12. [Datos longitudinales, de panel y encuestas](#longitudinal)
13. [Análisis de supervivencia](#survival)
14. [Análisis factorial confirmatorio (CFA)](#cfa)
15. [Modelos de ecuaciones estructurales (SEM)](#sem)
16. [PLS-SEM y PLSc](#pls)
17. [Tamaño muestral, potencia y efecto](#planning)
18. [Tablas, figuras y exportación](#reporting)
19. [Validación y límites del informe](#validation)

<a id="scope"></a>

## 1. Alcance público de 1.3.0

CFA, SEM, PLS-SEM y efectos de mediación/moderación están disponibles en los menús públicos. El instalador público excluye el metaanálisis y el ANOVA de tratamientos con medidas repetidas en los mismos sujetos. Pro se prevé para una versión posterior.

<a id="data"></a>

## 2. Preparación de datos

Importe SPSS, SAS, Stata, Excel, CSV y DAT; revise nombres, etiquetas, niveles de medida, categorías y referencias. Consulte el tamaño muestral utilizado y el tratamiento de valores faltantes en cada análisis.

<a id="descriptive"></a>

## 3. Frecuencias, descriptivos y tablas cruzadas

Obtenga frecuencias, porcentajes, estadísticas de posición y dispersión y tablas de contingencia. Las opciones seleccionadas ofrecen pruebas de asociación, tamaños del efecto y diagnósticos.

<a id="group"></a>

## 4. Comparaciones de grupos y ANCOVA

Use pruebas t independientes, ANOVA, ANCOVA y comparaciones no paramétricas. Las opciones compatibles incluyen métodos según la varianza, comparaciones posteriores y tamaños del efecto.

<a id="paired"></a>

## 5. Medidas pareadas y repetidas mixtas

Use análisis pareados, de medidas repetidas, ANOVA mixto y análisis pareados no paramétricos. Consulte efectos de tiempo y grupo y comparaciones del diseño elegido.

<a id="correlation"></a>

## 6. Correlación, fiabilidad y concordancia

Analice correlaciones, fiabilidad de escalas y concordancia entre evaluadores. Elija métodos e índices adecuados al tipo de variable y al diseño de evaluación.

<a id="factor"></a>

## 7. Análisis factorial exploratorio y PCA

Examine el número de factores/componentes, extracción, rotación, cargas y varianza explicada en EFA y PCA. CFA se describe por separado.

<a id="ipa"></a>

## 8. Análisis importancia–desempeño (IPA)

Abra Análisis → IPA y elija valoraciones directas o importancia derivada. Empareje importancia y desempeño en el mismo orden de atributos, o asigne desempeño y satisfacción global. Elija total, grupos independientes o pre/post pareado. Use columnas WIDE correspondientes o ID/tiempo LONG y dos valores temporales. Configure referencias y gráficos; revise tamaños, coordenadas, intervalos y diferencias. Word/HWPX se guardan desde resultados acumulados tras añadirlos.

<a id="regression"></a>

## 9. Regresión y regresión jerárquica

Use OLS, inferencia robusta HC3 y regresión bootstrap. El análisis jerárquico compara bloques sucesivos y cambios de varianza explicada. Los resultados seleccionados incluyen sr², f², colinealidad y diagnósticos residuales. La regresión jerárquica admite hasta cuatro bloques. Cada paso conserva las variables de los bloques anteriores y añade el siguiente bloque.

<a id="mediation"></a>

## 10. Efectos de mediación y moderación

Asigne predictor, resultado, mediador, moderador y covariables y dibuje rutas para estimar efectos directos, indirectos, totales y condicionales. No se selecciona un número de modelo. Las estructuras no compatibles se comprueban antes de ejecutar.

<a id="generalized"></a>

## 11. Regresión logística, GLM y penalizada

Use modelos logísticos y lineales generalizados, Ridge, LASSO y Elastic Net. Informe coeficientes y rendimiento con la escala del resultado, familia, enlace y configuración de validación cruzada.

<a id="longitudinal"></a>

## 12. Datos longitudinales, de panel y encuestas

Los diseños compatibles incluyen GEE, LMM, GLMM, efectos fijos/aleatorios y encuestas complejas. Revise identificadores de sujeto/conglomerado, tiempo, pesos, estratos y conglomerados según corresponda.

<a id="survival"></a>

## 13. Análisis de supervivencia

Use Kaplan–Meier, log-rank, RMST, Cox y análisis compatibles de riesgos competitivos. Revise primero el formato y los códigos de evento; las combinaciones no admitidas se restringen.

<a id="cfa"></a>

## 14. Análisis factorial confirmatorio (CFA)

Use ML, MLR y WLSMV/DWLS para datos ordinales; examine cargas, ajuste, fiabilidad, AVE, HTMT y comparaciones compatibles de invariancia de medida. Normal/Wishart se limita a configuraciones ML aplicables.

<a id="sem"></a>

## 15. Modelos de ecuaciones estructurales (SEM)

Informe rutas de medida/estructurales, ajuste y efectos directos, indirectos y condicionales compatibles. El bootstrap usa 5,000 remuestras por defecto; los resultados reflejan intervalos BC o percentiles y demás opciones elegidas.

<a id="pls"></a>

## 16. PLS-SEM y PLSc

Use Mode A reflectivo, Mode B formativo y PLSc reflectivo, diagnósticos de rutas/medida y predicción/comparaciones grupales compatibles. Los faltantes se sustituyen por medias de indicadores mediante seminr::mean_replacement, recalculadas en cada remuestra bootstrap. El valor predeterminado es 5,000 remuestras.

<a id="planning"></a>

## 17. Tamaño muestral, potencia y efecto

Calcule tamaño muestral, potencia y tamaño del efecto para pruebas compatibles. Registre alfa, efecto supuesto, asignación de grupos y dirección del contraste.

<a id="reporting"></a>

## 18. Tablas, figuras y exportación

La edición pública 1.3.0 exporta HTML/imágenes y PDF/Word/Excel. Los informes HTML/PDF de mediación/moderación, CFA, SEM y PLS-SEM añaden la figura del modelo al final. Las imágenes conservan la disposición visible; Free usa 300 dpi y desarrollo/Pro, 600 dpi. Revise y añada los resultados a la colección. Se admiten HTML, PDF, Word y Excel. HWPX solo aparece en resultados acumulados con interfaz coreana y se escribe directamente, sin Word ni Hancom. Word/HWPX permiten tablas principales, anexos, explicaciones y figuras; por defecto se seleccionan las principales. Se guarda la captura sin recalcular. Free usa 300 dpi y desarrollo 600 dpi. HTML incluye portada y lista enlazada de tablas.

<a id="validation"></a>

## 19. Validación y límites del informe

Se compararon análisis generales, regresión, longitudinales y supervivencia con SPSS; CFA/SEM con AMOS; y PLS-SEM/PLSc y CB-SEM con SmartPLS. Son validaciones acumuladas incorporadas en 1.3.0; la página de Validación resume condiciones y diferencias pendientes.
