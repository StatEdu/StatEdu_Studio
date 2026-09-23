# Descripción general — StatEdu Studio 1.3.0

StatEdu Studio es una aplicación estadística para Windows que permite preparar datos, analizar, visualizar modelos, planificar tamaños muestrales y presentar resultados. Asigne variables y opciones en la interfaz y revise métodos, supuestos, diagnósticos e interpretación. Esta descripción cubre toda la aplicación, no solo las novedades de 1.3.0.

## Edición de datos, alcance y calculadoras

Importe SPSS, SAS, Stata, Excel, CSV y DAT; revise nombres, etiquetas, niveles de medida, categorías y referencias. Consulte el tamaño muestral utilizado y el tratamiento de valores faltantes en cada análisis.

Gestione nombres, etiquetas, niveles de medida y categorías; recodifique y calcule variables, trate datos ausentes, combine datos, agregue por ID y transforme WIDE–LONG. La selección de casos y la división definen el alcance. Incluye calculadoras EQ-5D, HINT-8, Framingham, ASCVD y medidas metabólicas.

## Análisis disponibles

### Frecuencias, descriptivos y tablas cruzadas

Obtenga frecuencias, porcentajes, estadísticas de posición y dispersión y tablas de contingencia. Las opciones seleccionadas ofrecen pruebas de asociación, tamaños del efecto y diagnósticos.

### Comparaciones de grupos y ANCOVA

Use pruebas t independientes, ANOVA, ANCOVA y comparaciones no paramétricas. Las opciones compatibles incluyen métodos según la varianza, comparaciones posteriores y tamaños del efecto.

### Medidas pareadas y repetidas mixtas

Use análisis pareados, de medidas repetidas, ANOVA mixto y análisis pareados no paramétricos. Consulte efectos de tiempo y grupo y comparaciones del diseño elegido.

### Correlación, fiabilidad y concordancia

Analice correlaciones, fiabilidad de escalas y concordancia entre evaluadores. Elija métodos e índices adecuados al tipo de variable y al diseño de evaluación.

### Análisis factorial exploratorio y PCA

Examine el número de factores/componentes, extracción, rotación, cargas y varianza explicada en EFA y PCA. CFA se describe por separado.

### Análisis importancia–desempeño (IPA)

Abra Análisis → IPA y elija valoraciones directas o importancia derivada. Empareje importancia y desempeño en el mismo orden de atributos, o asigne desempeño y satisfacción global. Elija total, grupos independientes o pre/post pareado. Use columnas WIDE correspondientes o ID/tiempo LONG y dos valores temporales. Configure referencias y gráficos; revise tamaños, coordenadas, intervalos y diferencias. Word/HWPX se guardan desde resultados acumulados tras añadirlos.

### Regresión y regresión jerárquica

Use OLS, inferencia robusta HC3 y regresión bootstrap. El análisis jerárquico compara bloques sucesivos y cambios de varianza explicada. Los resultados seleccionados incluyen sr², f², colinealidad y diagnósticos residuales. La regresión jerárquica admite hasta cuatro bloques. Cada paso conserva las variables de los bloques anteriores y añade el siguiente bloque.

### Efectos de mediación y moderación

Asigne predictor, resultado, mediador, moderador y covariables y dibuje rutas para estimar efectos directos, indirectos, totales y condicionales. No se selecciona un número de modelo. Las estructuras no compatibles se comprueban antes de ejecutar.

### Regresión logística, GLM y penalizada

Use modelos logísticos y lineales generalizados, Ridge, LASSO y Elastic Net. Informe coeficientes y rendimiento con la escala del resultado, familia, enlace y configuración de validación cruzada.

### Datos longitudinales, de panel y encuestas

Los diseños compatibles incluyen GEE, LMM, GLMM, efectos fijos/aleatorios y encuestas complejas. Revise identificadores de sujeto/conglomerado, tiempo, pesos, estratos y conglomerados según corresponda.

### Análisis de supervivencia

Use Kaplan–Meier, log-rank, RMST, Cox y análisis compatibles de riesgos competitivos. Revise primero el formato y los códigos de evento; las combinaciones no admitidas se restringen.

### Análisis factorial confirmatorio (CFA)

Use ML, MLR y WLSMV/DWLS para datos ordinales; examine cargas, ajuste, fiabilidad, AVE, HTMT y comparaciones compatibles de invariancia de medida. Normal/Wishart se limita a configuraciones ML aplicables.

### Modelos de ecuaciones estructurales (SEM)

Informe rutas de medida/estructurales, ajuste y efectos directos, indirectos y condicionales compatibles. El bootstrap usa 5,000 remuestras por defecto; los resultados reflejan intervalos BC o percentiles y demás opciones elegidas.

### PLS-SEM y PLSc

Use Mode A reflectivo, Mode B formativo y PLSc reflectivo, diagnósticos de rutas/medida y predicción/comparaciones grupales compatibles. Los faltantes se sustituyen por medias de indicadores mediante seminr::mean_replacement, recalculadas en cada remuestra bootstrap. El valor predeterminado es 5,000 remuestras.

### Tamaño muestral, potencia y efecto

Calcule tamaño muestral, potencia y tamaño del efecto para pruebas compatibles. Registre alfa, efecto supuesto, asignación de grupos y dirección del contraste.

## Revisar, acumular y guardar

Revise y añada los resultados a la colección. Se admiten HTML, PDF, Word y Excel. HWPX solo aparece en resultados acumulados con interfaz coreana y se escribe directamente, sin Word ni Hancom. Word/HWPX permiten tablas principales, anexos, explicaciones y figuras; por defecto se seleccionan las principales. Se guarda la captura sin recalcular. Free usa 300 dpi y desarrollo 600 dpi. HTML incluye portada y lista enlazada de tablas.

## Documentación y alcance

Abra Descripción general, Guía, Análisis, Notas metodológicas, Validación e Historial en Información. Las tablas estadísticas principales siguen en inglés; menús y explicaciones siguen el idioma de interfaz. La validación se limita a los datos y opciones indicados; no abarca todas las combinaciones ni revisión por hablantes nativos.

El instalador de desarrollo es StatEdu Studio Dev 1.3.0-dev, con nombre de aplicación separado y menús de desarrollo. La edición pública excluye metaanálisis y ANOVA de tratamientos con medidas repetidas intraindividuales; conserva ANOVA mixto de medidas repetidas y pruebas pareadas.
