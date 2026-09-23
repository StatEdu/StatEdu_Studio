# Historial de versiones

## v1.3.0 - 2026-09-20

- Se incorporan a la edición pública la edición, estimación y diagnóstico CFA, SEM y PLS-SEM/PLSc, con bootstrap y comparaciones grupales compatibles. Supervivencia ofrece Kaplan–Meier, RMST, Cox y riesgos competitivos compatibles.

- Se añaden exportaciones públicas PDF, Word y Excel. La portada sigue el idioma de la interfaz; Free muestra logotipo e institución. HTML/PDF añaden al final las figuras de los cuatro análisis de modelos conservando su disposición. Las imágenes Free usan 300 dpi y fondo transparente o blanco.

- El flujo personalizado pasa a llamarse Efectos de mediación/moderación y se eliminan menús de regresión duplicados. Se separan comandos y ejecución y se ajustan guardado, posición de diálogos e iconos. Los cuatro análisis comparten rutas discontinuas para p válido ≥ .05.

- Se mejoran tablas verticales, gráficos residuales en una página, notas según los indicadores mostrados y notas en la última tabla de regresión jerárquica. Se unifican fuentes y saltos de encabezados HTML. Análisis, Notas metodológicas e Historial de versiones se ofrecen en los ocho idiomas de la interfaz.

- El instalador público excluye metaanálisis y ANOVA de tratamientos repetidos en los mismos sujetos. Pro se prevé más adelante; desarrollo/Pro usa una política de 600 dpi. La validación se explica por versión, casos y evidencia.

- Preparada la documentación del instalador de desarrollo 1.3.0: seis secciones conectadas en ocho idiomas e instrucciones de exportación actualizadas.

- Documentadas selección de casos, división de datos, gestión de resultados, HWPX directo en coreano, selección de contenido Word/HWPX y portada y lista de tablas HTML.

- Añadido IPA con valoraciones directas/importancia derivada, diseños total/independiente/pre-post pareado, referencias, gráficos e inferencia compatible, con documentación y exportación en ocho idiomas.

## v1.2.0 - 2026-08-06

### Añadido

- Se añadió el flujo de concordancia entre evaluadores al conjunto de análisis públicos, con resultados que priorizan el índice de concordancia recomendado y conservan los índices complementarios.
- Se añadió ANOVA mixto de medidas repetidas para comparaciones entre grupos antes/después y en múltiples momentos, con vías PP/ITT, resúmenes ajustados por covariables, revisión de supuestos, comparaciones post hoc y exportaciones HTML/PDF/Excel.
- Se añadió el lienzo de modelo personalizado de mediación/moderación al grupo público Regresión / Modelos.

### Modificado

- Se promovió el trabajo estabilizado posterior a 1.1.3 a los metadatos oficiales de la versión `1.2.0`.
- Se integró ANOVA de medidas repetidas en Comparación de grupos y se armonizaron su configuración, revisión de supuestos, comprobaciones de esfericidad/Levene, orientación sobre normalidad y recomendaciones con el análisis guiado de StatEdu Studio.
- Se amplió la validación estadística de correlación, fiabilidad, concordancia entre evaluadores, análisis factorial / PCA, prueba t / ANOVA, medidas repetidas pareadas, regresión, regresión logística, análisis longitudinal / panel, regresión penalizada, tamaño muestral, tamaño del efecto, editor de datos, modelos personalizados y medidas repetidas mixtas.
- Se renombró la etiqueta coreana del menú de modelos personalizados para facilitar la navegación pública.

### Corregido

- Se corrigió el lanzador público empaquetado de Electron para activar por defecto el lienzo de modelo personalizado de mediación/moderación en el instalador 1.2.0.
- Se eliminaron filas duplicadas al reutilizar resultados de modelos personalizados ajustados.

## v1.1.3 - 2026-07-12

### Modificado

- Se promovió la compilación de desarrollo estabilizada al instalador oficial `1.1.3`.
- Se mantuvo el alcance público conforme a la regla de empaquetado 1.1.1, excluyendo documentación, pruebas, ejemplos, fuentes y otros contenidos ajenos a la ejecución del entorno R incluido.
- Se añadió cobertura multilingüe para las nuevas etiquetas de carpetas de proyectos de análisis latente, textos de ejemplo de archivos de datos, delimitadores DAT y controles de análisis compartidos.

### Corregido

- Se corrigió la revisión de importación Excel para mantener interactivos los archivos seleccionados y cargarlos mediante la revisión de hoja/celda inicial.
- Se limitaron las vistas compartidas «Ver datos seleccionados» a las variables seleccionadas y 15 filas.
- Se corrigió la resolución de carpetas de proyecto/resultados de Latent Mplus para crear resultados junto al archivo de datos original cuando su ruta esté disponible.
- Se cambió el guardado de configuración de Latent Mplus para abrir un diálogo que permite elegir el nombre del archivo.
- Se alineó a la izquierda la visualización de la carpeta de proyecto/resultados de Latent Mplus.
- Se enviaron las etiquetas directas restantes a la tabla i18n compartida para traducir las nuevas opciones de datos, regresión, mediación, moderación y análisis latente.
- Se mantuvo la exclusión del lienzo de modelo personalizado de mediación/moderación de la versión oficial.

## v1.1.1 - 2026-07-07

### Corregido

- Se publicó un instalador de corrección para que las actualizaciones de Windows sustituyan los archivos de escritorio empaquetados en lugar de reutilizar una instalación obsoleta de la misma versión.
- Se reforzó el diagnóstico de inicio de Electron, ampliando el tiempo de arranque de Shiny y registrando la salida del proceso R cuando falla.
- Se corrigieron la carga de datos de escritorio, la selección del coreano al iniciar, el manejo de archivos de diseño de muestras complejas y la conservación de etiquetas vacías.

## v1.1.0 - 2026-07-06

### Añadido

- Se publicó la versión pública 1.1.0 con todos los análisis excepto el lienzo de modelo personalizado de mediación/moderación.
- Se añadieron restricciones de guardado públicas: HTML permanece activado por defecto; los controles de figuras, PDF, Excel y Añadir resultado permanecen visibles pero desactivados.
- Se mantuvo prueba t / ANOVA como excepción pública con HTML, figuras, PDF, Excel y Añadir resultado activados; sus colecciones de resultados pueden exportarse a Excel y Word.

### Modificado

- Se actualizaron los perfiles de publicación de Electron para generar instaladores públicos de StatEdu Studio con las versiones semánticas finales.

## v1.0.1 - 2026-06-28

### Modificado

- Se estabilizó el cambio entre coreano e inglés en menús principales, configuración, calculadoras, documentación y notificaciones, manteniendo las tablas de resultados en inglés.
- Se actualizaron el Editor de datos y los menús de análisis con categorías agrupadas, etiquetas coreanas corregidas y diseños de botones y pestañas alineados.
- Se refinaron los umbrales de normalidad de prueba t / ANOVA, los resúmenes ordenados de marcadores post hoc y el diseño/exportación de tablas cruzadas.
- Se añadieron controles del nombre de variable de salida de las calculadoras y se mejoraron los paneles EQ-5D y ASCVD10.
- Se añadieron comprobaciones de actualizaciones, metadatos de asociación de archivos `.studio` y el icono de archivo `.studio`.
- Se añadió Ayuda para informar errores, solicitar funciones o análisis, preguntas y respuestas y comprobaciones de actualizaciones.
- Se vincularon las solicitudes de Ayuda a los formularios web de StatEdu Studio y se dirigieron las preguntas y respuestas según el idioma de la interfaz.
- Se actualizaron las capturas de la guía 1.0 y los recursos de documentación bilingüe.

## v1.0.0 - 2026-06-25

### Modificado

- Se promovió la línea de versiones estabilizada a los metadatos públicos 1.0.0.
- Se sustituyeron los nombres beta del paquete Electron por los nombres finales de StatEdu Studio.
- Se mantuvieron explícitos en la documentación los anuncios públicos 1.0 aplazados, la verificación del DOI y del sitio web y los controles de calidad del paquete.
- Se actualizaron los marcadores post hoc ordenados para mostrar solo comparaciones directamente significativas por orden de medias, evitando cadenas transitivas como `b>a>c` cuando algún par no es significativo.
- Se añadió orientación PDF según el ancho de las tablas cruzadas: tablas principales anchas en horizontal y tablas más estrechas o complementarias en vertical.
- Se añadieron umbrales de normalidad por asimetría/curtosis seleccionables en prueba t / ANOVA: 2/5, 2/7 y 3/7, conservando 2/7 por defecto.
- Se actualizó el diálogo de guardado de configuración para abrir la carpeta del archivo de datos cargado cuando se conoce su ruta.
- Se impidió mostrar la revisión de importación Excel al iniciar salvo que exista una ruta válida de archivo Excel pendiente.

## v0.9.42 - 2026-06-23

### Añadido

- Se añadió Editor de datos > De ancho a largo para transformar columnas de medidas repetidas al formato largo antes del análisis longitudinal / panel.
- Se añadió validación de la transformación de ancho a largo y se ampliaron las comprobaciones de recodificación.
- Se añadieron pruebas básicas de inicio de Shiny y de publicación de Electron para validar versiones candidatas.
- Se añadió validación UTF-8 de la documentación versionada para comprobar versiones candidatas.

### Modificado

- Se normalizaron paneles, botones, espaciado y visores del Editor de datos según el patrón de prueba t / ANOVA.
- Se actualizó el tratamiento de valores perdidos para admitir su marcado por el usuario y su conversión a NA del sistema.
- Se mejoraron recodificación y renombrado con eliminación de variables en cola, paneles de destino alineados y reglas más claras.
- Se simplificó la configuración al formato `.studio` y se actualizaron los diálogos de guardado/carga.
- Se refinó el diseño longitudinal / panel y la presentación de categorías de referencia de predictores categóricos.
- Se reforzaron las comprobaciones de limpieza de publicación para archivos exclusivamente locales, artefactos generados, metadatos de versión y documentación.
- Se restauró el plan de producto coreano con las prioridades actuales de estabilización 1.0.
- Se actualizaron las referencias de la documentación coreana vigente de usuario y métodos a 0.9.42 y se añadió validación contra referencias de versión obsoletas.
- Se marcó como revisado el plan de distribución, licencias y actualizaciones 1.0 para la estabilización 0.9.42.
- Se añadió validación de limpieza de publicación a la suite central de estabilización para impedir versionar accidentalmente archivos de preparación Electron y artefactos locales.
- Se añadieron seguimiento y validación de preparación de publicación para empaquetado, DOI, sitio web y decisiones de aplazamiento 1.0.
- Se añadió un script previo a la publicación que ejecuta la validación completa de estabilización y pruebas básicas de inicio de Shiny y publicación de Electron.
- Se añadió un registro de decisiones 1.0 sobre empaquetado, DOI, sitio web, restricciones por edición, licencias, actualizaciones y notas públicas.
- Se reforzaron el contrato y la validación del diseño de interfaz para la ubicación estándar de botones en los tres bloques del Editor de datos.
- Se añadió un protocolo manual de calidad para revisar apariencia, datos, análisis, exportaciones y Electron empaquetado de las versiones candidatas.
- Se vinculó el protocolo manual de calidad con las pruebas básicas de publicación de Electron.
- Se actualizó el estado de preparación para registrar que la comprobación previa a la publicación pasó.
- Se reforzó la validación de metadatos frente a impedimentos pendientes de la versión pública 1.0.
- Se añadieron una plantilla de registro y validación para evidencias de calidad manual de versiones candidatas.
- Se retiraron del control de versiones los artefactos comparativos generados de `outputs/` y se bloquearon los artefactos de salida de la raíz en la validación de limpieza.
- Se documentó en README y en la validación de metadatos el comando completo de comprobación previa de Electron con salida empaquetada.
- Se aclaró en la documentación de requisitos de publicación que la URL de destino del DOI es `https://studio.statedu.com`.
- Se aclaró que el plan de distribución, licencias y actualizaciones 1.0 es planificación, no una afirmación de que estén implementadas ediciones restringidas, activación de licencias, actualizador o infraestructura de instaladores públicos.
- Se añadió una advertencia en README y en preparación de publicación: el DOI previsto debe resolverse antes de anunciar públicamente su cita para 1.0.
- Se reforzó la validación de avisos de fuentes/licencias para disponibilidad pública del código, avisos de terceros, informes de licencias y referencias a textos de licencia incluidos.
- Se armonizaron la lista de publicación, README y la guía de calidad manual para conservar las evidencias completadas junto a notas de publicación y artefactos de validación.
- Se documentó el requisito de sustituir los nombres beta 0.9.x de paquetes Electron antes del instalador público 1.0.
- Se armonizaron los diálogos de configuración de Latent Mplus con el contrato de usar exclusivamente `.studio`.
- Se actualizó el plan de distribución/licencias/actualizaciones 1.0 para que las notas beta históricas no parezcan la base de la versión actual.
- Se añadió un control manual de calidad que prohíbe afirmar en notas públicas o textos visibles que existen ediciones restringidas, activación de licencias, actualizaciones internas o infraestructura pública de instaladores aún no implementadas.
- Se sustituyeron las notificaciones provisionales del generador Latent Mplus por mensajes explícitos de función no habilitada en la versión y su validación.
- Se reformuló el subtítulo alternativo de tamaño del efecto para que las calculadoras no disponibles no parezcan promesas futuras.
- Se eliminó del ensamblado de menús un auxiliar sin uso de pestaña provisional de Análisis.
- Se aclaró la lista de publicación para excluir los formatos de configuración heredados de los diálogos públicos, manteniendo documentados los identificadores internos de compatibilidad.

## v0.9.41 - 2026-06-20

### Modificado

- Se agruparon Análisis, Tamaño muestral y Tamaño del efecto en categorías estadísticas coherentes de primer nivel.
- Se corrigieron los controles de transferencia de tablas cruzadas para devolver fiablemente las variables de fila o columna seleccionadas a la lista disponible.
- Se ajustó la ubicación de los botones de transferencia en los paneles de columnas y filas de tablas cruzadas.

## v0.9.40 - 2026-06-20

### Modificado

- Se incrementó la versión de desarrollo tras la publicación beta 0.9.39.
- Se cambió la marca visible a **StatEdu Studio**, incluidos encabezado, Acerca de, lanzador, instalador, favicon, logotipos y nombres de exportación predeterminados.
- Se añadieron comprobaciones de compatibilidad de marca que documentan los identificadores heredados conservados por DOI, entorno, rutas o búsqueda retrocompatible de datos.
- Se protegieron las llamadas de entrada Shiny al arrancar el cliente para evitar errores tempranos de `Shiny.setInputValue` antes de estar listo el enlace del cliente.
- Se actualizaron los metadatos de bloqueo de auditoría del paquete Electron para resolver la dependencia transitiva de desarrollo `undici` sin hallazgos de npm audit.
- Se añadieron metadatos del autor del paquete Electron y se ignoraron los artefactos `StatEdu_Studio_*.zip` durante desarrollo local y preparación de Electron.

## v0.9.39 - 2026-06-18

### Añadido

- Se añadió un flujo independiente `Analysis > Longitudinal / Panel Models` para GEE, LMM, GLMM y modelos de panel con efectos fijos o aleatorios.
- Se añadieron comprobaciones de supuestos específicas, alternativas recomendadas, comparaciones automáticas de sensibilidad, estimaciones listas para publicación, texto para manuscrito, lista de presentación SCI y versiones de software para resultados longitudinales / panel.
- Se añadió la pestaña de valores perdidos longitudinal / panel con tratamiento principal, motores reales MI/IPW/WGEE de sensibilidad y registro del método en el informe.
- Se añadieron ponderaciones longitudinales con un destino de variable de peso, tipos muestral/longitudinal/IPW/combinado, recorte, pesos finales normalizados y tamaño muestral efectivo.
- Se añadió exposición / offset opcional para modelos longitudinales de conteo/tasa, incluidos offsets `log(exposure)` en ajustes principales y de sensibilidad.
- Se añadieron detalles de detección de inflación de ceros comparando proporciones observadas y esperadas bajo Poisson.
- Se añadió el flujo activo `Analysis > GLM` para GLM gaussianos, logísticos binarios, Gamma y de conteo, reutilizando la detección GEE para elegir Poisson o binomial negativa.
- Se añadieron detalles de presentación SCI de GLM: casos completos, selección Poisson/binomial negativa, detección de EPV/separación/celdas escasas en logística, revisión de independencia, influencia, notas de publicación, listas de presentación y texto sugerido para manuscrito.
- Se añadieron opciones GLM con pestañas, incluida la de valores perdidos para casos completos, imputación múltiple y ponderación por probabilidad inversa.
- Se añadió documentación GLM en Guía de usuario, Métodos de análisis y Notas metodológicas en coreano sobre familia/enlace, sensibilidad a datos perdidos, errores estándar robustos, sobredispersión y presentación SCI.
- Se añadió compatibilidad HTML, PDF, Excel y colecciones de resultados guardadas para GLM, incluidas notas de publicación, listas SCI, texto para manuscrito y hojas de versiones de software.
- Se añadió compatibilidad HTML, PDF, Excel y colecciones de resultados guardadas para modelos longitudinales / panel.
- Se amplió la validación de ajustes longitudinales / panel y GLM, estructura de configuración, catálogos de supuestos, exportación HTML/Excel, sensibilidad y secciones SCI.

### Modificado

- Se armonizó la configuración longitudinal / panel con la transferencia de variables de prueba t / ANOVA, mostrando solo opciones pertinentes al modelo.
- Se fusionaron Modelo y Términos longitudinales / panel para configurar tipo de modelo, términos de tiempo fijo y efectos aleatorios en una pestaña.
- Se cambió la correlación de trabajo GEE predeterminada a intercambiable; los ajustes AR(1) pasan ahora el orden de sujeto/onda temporal a `geepack::geeglm`.
- Se reetiquetaron los ajustes GEE de binomial negativa como GLM marginal de binomial negativa con errores estándar robustos por conglomerado de sujeto, ya que geepack no proporciona GEE nativo de binomial negativa.
- Se aclaró que ID de conglomerado opcional agrupa un intercepto aleatorio adicional en LMM/GLMM y no se aplica al ajuste principal GEE/panel seleccionado.
- Se aclaró que LMM/GLMM trata valores perdidos mediante verosimilitud bajo MAR con medidas disponibles, reservando MI/IPW para sensibilidad y no para el ajuste principal predeterminado.
- Se excluyeron modelos de conteo con inflación de ceros y hurdle del módulo longitudinal / panel predeterminado para evitar dependencias opcionales pesadas; el exceso de ceros se informa como orientación diagnóstica.
- Se sustituyó la estructura provisional Generalizado por configuración GLM funcional, ejecución, tabla de coeficientes, estadísticos de ajuste, errores estándar robustos, sobredispersión y VIF opcional.
- Se armonizó `run_app.R` con `R/app_bootstrap.R` para usar la misma lista de paquetes requeridos al instalar desde el lanzador y durante la ejecución.
- Se fijó Electron en 39.8.6 y se actualizó el archivo de bloqueo tras auditar los paquetes.
- Se reforzó el script beta de Electron para localizar Rscript fuera de PATH y no fallar por el módulo opcional Latent Mplus excluido del paquete.
- Se actualizaron README y la Guía de usuario, Métodos de análisis y Notas metodológicas en coreano para los nuevos flujos longitudinal / panel y GLM.

## v0.9.38 - 2026-06-15

### Añadido

- Se añadieron estimaciones empíricas de tamaño muestral para mediación de Fritz & MacKinnon (2007) con potencia .80.
- Se añadieron referencias específicas de Fritz & MacKinnon, Monte Carlo, bootstrap y Sobel para calcular tamaños muestrales de mediación.

### Modificado

- Se amplió el bloque de resultados de Tamaño muestral en ventanas grandes, conservando los tres bloques en pantallas de 1280 px de ancho.

## v0.9.37 - 2026-06-12

### Añadido

- Se añadieron diccionarios de detección Likert de 4 puntos en coreano e inglés correspondientes a los existentes de 5 puntos.
- Se añadió un tutorial animado con superposición de acciones a la guía coreana interna, usando imágenes incluidas.

### Modificado

- Se refinó el diccionario personalizado Likert para abrir los registrados desde un botón, listarlos y mostrar sus detalles seleccionados en un panel lateral.
- Se priorizaron coincidencias exactas de niveles Likert frente a superconjuntos compatibles para detectar respuestas de 4 puntos como escalas de 4 puntos.
- Se ajustaron tiempos y posiciones de superposiciones de la guía para carga de datos, prueba t / ANOVA y revisión de resultados.

### Corregido

- Se conservó el desplazamiento al abrir o seleccionar diccionarios de detección Likert registrados.
- Se mejoró el ancho de las columnas de selección y nombre de detección de la tabla Likert.

## v0.9.36 - 2026-06-11

### Añadido

- Se añadió un gestor editable de diccionarios personalizados Likert con revisión, detalles, edición y eliminación de diccionarios registrados.
- Se amplió la validación de análisis logístico, recodificación, análisis factorial/PCA, correlación, pruebas pareadas, E/S de datos e historial de resultados.

### Modificado

- Se refinó la presentación de tablas orientada a B5 en regresión logística, análisis factorial, PCA, fiabilidad, pruebas pareadas/repetidas, correlación y resultados guardados.
- Se mejoró la revisión de importación Excel, trasladando la vista de hoja al panel principal y simplificando controles.
- Se actualizó la conversión Likert para asignar a las variables el tipo de medición solicitado tras convertirlas.

### Corregido

- Se corrigió el desplazamiento de botones de selección en las listas del Editor de datos tras importar Excel.
- Se corrigieron las actualizaciones de detección automática de valores perdidos y Likert tras convertir/importar.
- Se corrigieron ubicación de tablas, celdas de referencia, VIF y opciones de intervalos de confianza en regresión logística jerárquica.
- Se corrigieron el orden de columnas de cargas factoriales/PCA y los encabezados compactos para B5 vertical.

## v0.9.35 - 2026-06-10

### Añadido

- Se añadió Latent Mplus para desarrolladores como módulo opcional de EasyFlow con pasos Datos, Configuración y Resultados.
- Se añadieron guardado/carga de roles latentes, condiciones de subconjunto, conservación del orden seleccionado y revisión de progreso desde Resultados.
- Se dirigieron las salidas latentes a la carpeta del archivo de datos cargado, incluidos resultados, temporales Mplus, registros, tablas Excel y figuras de 600 dpi.
- Se añadieron visualización de gráficos nativos Mplus seleccionados y variantes de perfiles de indicadores en color.

### Modificado

- Se armonizaron tablas y figuras latentes con un marco B5, salida alineada a la izquierda, tablas compactas y figuras escaladas a B5 vertical.
- Se mejoró la secuencia de carga/configuración/restablecimiento en Datos y se aplazó el registro del servidor latente hasta abrir una pestaña latente.
- Se ocultaron tablas exclusivas de LCA en resultados LPA y se eliminaron de Resultados las tablas internas de claves de clase BCH.
- Se actualizó el empaquetado beta Electron para excluir Latent Mplus, exclusivo de desarrollo, de la preparación pública.

### Corregido

- Se corrigieron las tablas de revisión de supuestos y resumen del modelo de prueba t / ANOVA para rellenar los resultados de comprobación.
- Se restablecieron roles/resultados latentes al cargar datos nuevos, conservando la configuración YAML restaurada explícitamente.
- Se conservó el desplazamiento de la tabla de variables latentes al asignar roles.
- Se trasladaron los mensajes de progreso latentes de Configuración a Resultados durante la ejecución.

## v0.9.34 - 2026-06-09

### Modificado

- Se refinaron las tablas pareadas y de medidas repetidas: opciones de resumen, alineación estadística, etiquetas de tamaño del efecto, advertencias y revisión de supuestos.
- Se corrigió el orden de variables de medidas repetidas pareadas para seguir la selección del usuario.
- Se mantuvieron visibles las pestañas de opciones pareadas, habilitando etiquetas de variables repetidas solo con tres o más medidas.
- Se aclararon las notas de medidas repetidas para no presentar lambda de Wilks y Greenhouse-Geisser como un método combinado.
- Se actualizaron los metadatos de cita con el DOI registrado de EasyFlow Statistics.

## v0.9.33 - 2026-06-06

### Modificado

- Se ampliaron los diagnósticos ANCOVA: Levene predeterminado, Brown-Forsythe / Breusch-Pagan / White opcionales, tablas de homogeneidad de pendientes, casos completos, gráficos de linealidad residual y sensibilidad a la influencia.
- Se añadieron controles ANCOVA para conservar la selección automática o informar advertencias manteniendo el modelo estándar.
- Se refinó la presentación compartida: fuente de 9 pt, anchos fijos verticales/horizontales, vista en pantalla a 1.5x, marcadores post hoc comunes, etiquetas ES y encabezados post hoc de dos líneas.
- Se reorganizaron Supuestos / Modelo / Salida de ANCOVA y se armonizaron espaciado, sangría y notas.

## v0.9.32 - 2026-06-03

### Modificado

- Se refinó la presentación ANCOVA con mejor control del ancho y estadísticos alineados a la derecha.
- Se añadió conversión de tamaños del efecto LMM al estilo SPSS: eta cuadrado parcial a partir de F/gl global y dz pareado basado en covarianza.
- Se añadió conversión de tamaños del efecto GLMM para resultados de efectos fijos con logit binario, enlace logarítmico de conteo y distribución gaussiana.
- Se actualizaron Guía de usuario, Métodos de análisis y Notas metodológicas en coreano para tamaños del efecto ANCOVA, LMM, GEE y GLMM.
- Se refinó la entrada de Tamaño del efecto para calcular LMM global y por pares con cualquiera de los conjuntos de datos de entrada disponibles.

## v0.9.31 - 2026-06-02

### Modificado

- Se añadió guardado/apertura del historial de resultados con el marcador de tipo `.efs-result`.
- Se separaron las configuraciones guardadas en `.efs-settings` con validación de tipo.
- Se cambió Añadir resultado para conservar la captura del resultado actualmente mostrado en vez de reconstruirlo.
- Se normalizaron anchos y reglas de exportación horizontal en análisis, historial, HTML, PDF y Word.
- Se refinaron resúmenes de modelo y tablas de advertencias en correlación, pruebas pareadas, análisis factorial, fiabilidad y regresión logística.
- Se eliminó la portada Word para iniciar los documentos guardados directamente con métodos y resultados.
- Se añadió ANCOVA con selección automática estándar, robusta HC3, por rangos o con interacción y exportación HTML, PDF, Excel e historial.

## v0.9.30 - 2026-06-02

### Modificado

- Se refinaron las calculadoras de tamaño muestral y tamaño del efecto, retirando del menú de efectos los flujos ajenos a ellos.
- Se añadieron cálculos de tamaño muestral en segundo plano con progreso y posibilidad de detenerlos.
- Se añadieron entradas de correlación no estructurada LMM y estimación de grados de libertad SEM/CFA por recuento del modelo.
- Se normalizó el énfasis del tamaño muestral requerido con etiquetas `n` explícitas y potencia predeterminada 0.95.
- Se ampliaron Guía de usuario, Métodos de análisis y Notas metodológicas en coreano para tamaño muestral, potencia y tamaño del efecto, con fórmulas y referencias.
- Se actualizó Métodos de análisis en coreano para 0.9.30, incluido el resumen del modelo en prueba t/ANOVA, pruebas pareadas, pareadas no paramétricas y correlación.
- Se incluyeron recursos MathJax locales para mostrar fórmulas sin conexión en Notas metodológicas.

## v0.9.29 - 2026-06-01

### Añadido

- Se añadieron menús superiores independientes Tamaño muestral y Tamaño del efecto después de Análisis.
- Se añadieron calculadoras respaldadas por referencias de tamaño muestral, potencia y efecto para prueba t, ANOVA / ANCOVA, GEE, LMM, pruebas no paramétricas, proporciones, chi-cuadrado, McNemar, regresión, supervivencia y otras planificaciones.
- Se añadió validación específica de las funciones de cálculo de tamaño muestral, potencia alcanzada y tamaño del efecto.

### Modificado

- Se reorganizaron las pantallas de tamaño muestral y efecto según el flujo compartido de tres bloques de configuración de análisis.
- Se ordenaron los menús Tamaño muestral y Tamaño del efecto por familia de diseño del estudio.
- Se destacó el tamaño del efecto de la prueba t para el método seleccionado y se mostraron efectos convertibles sin valores intermedios ajenos al tamaño del efecto.

## v0.9.28 - 2026-05-30

### Corregido

- Se llevaron al primer plano los diálogos Windows de datos/configuración mediante un propietario WinForms siempre visible en lugar del selector nativo de R.
- Se situaron las tablas de cargas factoriales y PCA inmediatamente después del resumen en pantalla, HTML/PDF y Excel.
- Se corrigió Acerca de > Licencias de código abierto para localizar los avisos generados desde la ruta incluida y agrupar licencias por paquetes EFS directos, dependencias incluidas, paquetes base/recomendados de R y entorno R.
- Se sustituyó la limpieza del puerto del lanzador Windows por `netstat` / `taskkill` para evitar bloqueos al cerrar una aplicación existente en el puerto 7894.

## v0.9.27 - 2026-05-29

### Modificado

- Se acortaron los nombres predeterminados de resultados, datos, configuración y exportaciones de `EasyFlow_Statistics_...` al prefijo `EFS_...`.

### Añadido

- Se añadió Acerca de > Historial de versiones para consultar el registro incluido dentro de la aplicación de escritorio.

## v0.9.26 - 2026-05-29

### Añadido

- Se añadió importación Excel en dos pasos con selección de hoja, celda inicial estilo A1, control de encabezados y vista previa antes de cargar.
- Se conservaron las opciones de importación Excel en la configuración guardada para reabrir los archivos.

## v0.9.25 - 2026-05-29

### Corregido

- Se armonizaron Añadir resultado y exportación Word de regresión con la tabla visible, conservando filas de referencia categórica, etiquetas de valores y resumen de ajuste en una línea.
- Se usó una sección Word horizontal para tablas de coeficientes anchas, acercando el archivo a la tabla visible.

## v0.9.24 - 2026-05-29

### Corregido

- Se corrigieron las tablas de prueba t / ANOVA y no paramétricas para conservar tres decimales en valores p como `.008` y efectos como `.022` cuando el marcador de nota coincide con el último dígito.

## v0.9.23 - 2026-05-29

### Corregido

- Se sustituyó el selector de datos de escritorio PowerShell por el diálogo Windows nativo R `choose.files()` para abrir datos fiablemente desde Electron instalado.

## v0.9.22 - 2026-05-29

### Corregido

- Se priorizó un diálogo Windows nativo antes de recurrir a Tcl/Tk, reduciendo casos de diálogos ocultos tras Electron o ausentes.
- Se mantuvieron visibles los filtros Excel, SAS, Stata, CSV, DAT y SPSS en el selector de datos.

## v0.9.21 - 2026-05-29

### Añadido

- Se añadió importación de Excel heredado `.xls`, SAS `.sas7bdat` / `.xpt` y Stata `.dta`.
- Se actualizaron el selector, textos de Datos y validación de E/S para los nuevos formatos.

## v0.9.20 - 2026-05-29

### Modificado

- Se redujo la carga inicial de Shiny renderizando los cuerpos de Editor de datos, Calculadora, Análisis y Acerca de solo al abrirlos.

## v0.9.19 - 2026-05-29

### Modificado

- Se redujo el inicio del escritorio instalado cargando únicamente Shiny y DT durante el arranque inicial.
- Se omitieron exploraciones redundantes de paquetes del entorno incluido al arrancar Electron; la disponibilidad sigue comprobándose en compilación y pruebas de publicación.
- Se acortó el intervalo de comprobación de disponibilidad Shiny y se añadieron diagnósticos separados del tiempo de carga de BrowserWindow.

## v0.9.18 - 2026-05-29

### Añadido

- Se añadió la fila estándar de cinco controles de guardado a resultados de regresión logística.
- Se añadió exportación logística HTML, PDF, Excel y a la colección de resultados guardada.

## v0.9.17 - 2026-05-29

### Corregido

- Se cambió Correlación > Correlaciones avanzadas para sustituir los métodos principales por correlaciones de variables latentes, evitando resultados duplicados aparte.
- Los pares continuos-ordinales/binarios aptos muestran ahora Polyserial directamente en Métodos cuando se activan correlaciones de variables latentes.

## v0.9.16 - 2026-05-29

### Corregido

- Se corrigió el ajuste de línea de notas de valores p y tamaños del efecto en prueba t / ANOVA y pruebas no paramétricas independientes.

## v0.9.15 - 2026-05-29

### Corregido

- Se corrigió el estilo de marcadores de nota en línea de prueba t / ANOVA para alinear tamaños del efecto conservando la omisión del cero inicial.

## v0.9.14 - 2026-05-29

### Modificado

- Se añadieron licencia GPL, oferta de código fuente y páginas Acerca de con avisos de fuentes/licencias del escritorio incluido.
- Se añadieron avisos OSS generados, informe de licencias, colección de textos de licencia y pruebas básicas de publicación de instaladores Electron/R.
- Se añadieron informes de depuración del entorno R incluido y versiones exactas de Electron/electron-builder.
- Se redujo la sobrecarga de inicio del escritorio y se añadieron diagnósticos temporales de arranque.
- Se eliminó la protección de cierre de sesión Shiny al iniciar que podía dejar una pantalla gris desactivada.
- Se recompiló el instalador beta Windows 0.9.14.

## v0.9.13 - 2026-05-28

### Modificado

- Se activó la exportación Word compatible y se normalizaron las reglas entre Word, PDF y Excel.
- Se añadió Word orientado a publicación con portada y métodos, selección de tablas principales, notas, superíndices y B5 vertical predeterminado, horizontal solo para tablas anchas.
- Se refinaron anchos, encabezados, columnas post hoc y estadísticos al pie para PDF/Word en resultados pareados, repetidos, prueba t/ANOVA, correlación, regresión, jerárquica y logística.
- Se mejoró Excel para conservar la estructura visible con títulos, encabezados de dos niveles, bordes, notas combinadas y anchos fijos.
- Se amplió la ventana inicial Electron y se estabilizó la alineación de acciones de configuración de regresión tras mostrar resultados.
- Se recompiló el instalador beta Windows 0.9.13.

## v0.9.12 - 2026-05-27

### Modificado

- Se refinaron PDF y diseños de publicación para resultados pareados, pareados de medidas repetidas, pareados no paramétricos, análisis factorial, PCA, regresión logística y prueba t / ANOVA.
- Se mejoraron espaciado de tablas, alineación del resumen del modelo y tamaño de marca de agua beta en informes exportados.
- Se ajustaron los textos para centrar los resúmenes compactos en métodos, N, supuestos y decisiones concisas.
- Se normalizó PDF en A4 y Word en B5, ajustando las tablas al ancho imprimible y conservando las reglas de alineación visibles.
- Se amplió el ajuste horizontal PDF para columnas de efectos de medidas repetidas pareadas y coeficientes de regresión jerárquica.
- Se normalizó Excel para reproducir encabezados de dos niveles, títulos, bordes, anchos fijos y notas combinadas de las tablas visibles.
- Se habilitó Guardar Word en Resultados para ediciones con exportación.
- Se armonizaron fuentes de encabezado/cuerpo Word, se activó el estilo del botón Guardar Word, se usaron dos decimales para medias/desviaciones pareadas y se combinaron tablas de resumen/supuestos/diagnóstico de pruebas pareadas mixtas.
- Se conservaron encabezados de dos niveles en Word, se excluyeron logotipos del cuerpo, se añadieron secciones horizontales para tablas anchas repetidas y jerárquicas y se ajustaron márgenes/columnas PDF pareadas, repetidas y jerárquicas.
- Se mostraron N, método y motivo como columnas directas del resumen prueba t / ANOVA; los resúmenes de regresión multimodelo usan encabezados dependiente/modelo de dos niveles.
- Se forzó el ancho horizontal PDF de tablas pareadas repetidas, se añadieron líneas bajo encabezados jerárquicos de primer nivel y se delimitaron secciones horizontales Word para mantener B5 vertical por defecto.
- Se ampliaron columnas post hoc y tolerancia, se agrandaron portadas PDF, se agruparon efectos pareados bajo encabezados dobles, se restauraron superíndices/notas Word y se combinaron pies repetidos de regresión.
- Se redujo la fuente de notas Word, se conservaron todas las notas mostradas, se aplicaron superíndices a marcadores de encabezado jerárquico y se combinaron estadísticos al pie una vez por modelo.
- Se etiquetaron como regresión ordinaria las ejecuciones de solo Bloque 1 desde regresión jerárquica al añadirlas a la colección guardada.
- Se cambió Word para incluir solo tablas principales listas para publicación, cada una en página nueva, figuras a tamaño renderizado sin ampliación y horizontal solo para tablas pareadas/jerárquicas anchas y matrices de correlación de al menos 10 variables.
- Se centraron los estadísticos al pie de regresión en Word y se añadió una línea superior fuerte sobre F(p), con homocedasticidad residual como único elemento x²(p).
- Se amplió la ventana inicial Electron, se añadieron portada y métodos Word, se colocaron dos figuras de regresión por página y se excluyeron detalles post hoc de prueba t/ANOVA de la exportación Word de tablas para publicación.
- Se refinaron espaciado Word, anchos de frecuencias/descriptivos, resúmenes de regresión, transiciones horizontales y tamaños de figuras para reducir saltos de línea, espacios y páginas vacías.
- Se estabilizaron las acciones de regresión tras renderizar resultados, se reforzó el filtrado post hoc Word, se ampliaron columnas combinadas n(%)/M±SD e IQR y se ajustaron tablas de correlación anchas.

## v0.9.11 - 2026-05-27

### Modificado

- Se añadieron resúmenes compactos de modelo y supuestos para pruebas pareadas, prueba t / ANOVA, regresión y regresión logística.
- Se trasladaron diagnósticos detallados a tablas de revisión, centrando los resúmenes de modelo en N, método y motivos concisos.
- Se añadieron pestañas de opciones para análisis factorial, PCA y prueba t / ANOVA con conservación de estado y espaciado refinado.
- Se cambió el tamaño del efecto de regresión para activar f2 por defecto y dejar sr2 sin marcar.

## v0.9.10 - 2026-05-27

### Modificado

- Se añadió empaquetado beta Electron con entorno R incluido, lanzador de ventana de escritorio, metadatos del instalador e iconos EasyFlow.
- Se mejoró la importación CSV y Excel coreana probando codificaciones comunes y normalizando nombres y valores de texto.
- Se conservaron los niveles binarios, categóricos y ordinales revisados al guardar/cargar configuración para mantener iguales los tipos de variables en Datos y Análisis.
- Se limitaron las columnas de Frecuencias / Descriptivos a estadísticos adecuados al tipo seleccionado.
- Se refinaron la marca de agua beta y los elementos provisionales de exportación Word.

## v0.9.9 - 2026-05-27

### Modificado

- Se rediseñó la recodificación sobre la misma variable con un paso `Add` en cola y `Apply` final para revisar reglas antes de cambiar datos.
- Se añadieron reglas en cola editables con selección de filas, eliminación, tipos predeterminados automáticos e inferencia del nivel de medición de salida.
- Se añadió recodificación de valores únicos para categorías observadas, marcadores de datos perdidos, avisos de valores sin coincidencia y conversión desde/hacia `NA`.
- Se refinaron controles de categorización, operadores de rango, alineación, botones y espaciado de Recodificar variable.

## v0.9.8 - 2026-05-26

### Modificado

- Se añadió Acerca de con Resumen, Guía de usuario, Métodos de análisis, Notas metodológicas e información de la aplicación.
- Se amplió la documentación coreana sobre uso, métodos implementados, notas, paquetes/entorno, criterios y referencias.
- Se aclararon los textos de homocedasticidad residual y las etiquetas de métodos de tendencia en tablas cruzadas.
- Se documentó la opción de 20,000 remuestreos bootstrap y se mantuvo 50,000 como recomendación.
- Se normalizó el nombre **EasyFlow Statistics**, escrito completo y destacado consistentemente.

## v0.9.7 - 2026-05-26

### Modificado

- Se añadió selección automática de Pearson para pares continuos normales y Spearman para pares no normales u ordinales.
- Se añadieron condiciones de protección para omitir variables/modelos inválidos en correlación, pruebas pareadas, prueba t / ANOVA, regresión y logística sin detener todo el análisis.
- Se añadieron advertencias y resultados omitidos por muestra pequeña, varianza cero, todos los valores empatados, celdas escasas, riesgo de separación, rango deficiente y umbrales VIF.
- Se añadieron matrices Pearson / policóricas para análisis factorial y PCA, con orientación ordinal y advertencias muestrales.
- Se unificaron auxiliares de advertencias y resultados omitidos entre pantallas y Excel.

## v0.9.6 - 2026-05-25

### Modificado

- Se añadió Prueba pareada no paramétrica independiente con rangos con signo de Wilcoxon y Friedman.
- Se añadieron correcciones post hoc pareadas Bonferroni y Holm-Bonferroni, con Bonferroni por defecto.
- Se añadieron mediana, Q1~Q3 y notas de tamaño del efecto Wilcoxon a resultados pareados no paramétricos.
- Se armonizaron encabezados, marcadores de nota, exportación y botones de resultados pareados y pareados no paramétricos.

## v0.9.5 - 2026-05-25

### Modificado

- Se añadió Pruebas no paramétricas independiente con U de Mann-Whitney y Kruskal-Wallis.
- Se añadieron resúmenes de mediana y cuartiles a las pruebas no paramétricas independientes.
- Se añadió delta de Cliff a resultados U de Mann-Whitney.
- Se corrigieron letras post hoc compactas para asignar letras combinadas a grupos compartidos no significativos.
- Se mostraron los marcadores de notas de valores p y efectos en columnas adyacentes estrechas para una alineación estable.
- Se refinó el espaciado de opciones de Pruebas no paramétricas y Prueba pareada.

## v0.9.4 - 2026-05-25

### Modificado

- Se refinaron las marcas de agua HTML/PDF exclusivas de desarrollo con marcas horizontales EasyFlow y StatEdu.
- Se activó la exportación de matrices de dispersión y mapas de calor de correlación.

## v0.9.3 - 2026-05-25

### Modificado

- Se armonizó la tabla de cargas PCA con análisis factorial, incluyendo h², complejidad, autovalor, varianza, varianza acumulada y filas KMO / Bartlett, sin columnas de fiabilidad.
- Se refinaron controles PCA de matriz, selección por varianza acumulada y alineación de campos numéricos de componentes.
- Se mejoró el estilo de diagnósticos factoriales y se finalizó la ubicación del resumen KMO / Bartlett.
- Se mantuvieron visibles los cinco controles de guardado en desarrollo y se añadió PDF / Añadir resultado a los módulos restantes.
- Se retiraron decoración de portada PDF y etiquetas internas de archivo/fecha; se numeraron páginas abajo a la derecha.
- Se añadió identidad de portada PDF según edición, incluidos logotipo StatEdu y nombre StatEdu Statistical Research Institute en desarrollo.
- Se añadió la fecha de salida PDF debajo de la fecha de guardado en la portada.
- Se añadieron marcas de agua exclusivas de desarrollo a HTML y PDF exportados.

## v0.9.1 - 2026-05-24

### Modificado

- Se refinó el análisis factorial exploratorio con cargas ordenadas, filtro opcional de cargas pequeñas, valores problemáticos resaltados, comunalidades, complejidad, autovalores, varianza y matrices de estructura oblicua.
- Se añadieron resúmenes opcionales de fiabilidad de subfactores junto a la matriz de cargas.
- Se mejoraron diagnósticos factoriales de extracción según normalidad, muchos factores fijos, valores perdidos/infinitos y problemas de fiabilidad por ítem.
- Se compactaron las opciones factoriales para caber en el bloque estándar de tres columnas.

## v0.9.0 - 2026-05-24

### Modificado

- Se añadió acumulación en Resultados para recopilar en orden las salidas compatibles mediante Añadir resultado.
- Se añadió exportación de colecciones de resultados a HTML, PDF, Excel y Word.
- Se excluyeron análisis factorial y PCA de Añadir resultado hasta decidir el formato definitivo de sus tablas.

## v0.8.12

### Modificado

- Se añadió análisis factorial exploratorio con ejes principales y máxima verosimilitud, rotaciones Varimax/Oblimin, selección por autovalor o número fijo, método según normalidad, KMO / Bartlett, sedimentación y exportación.
- Se añadió PCA con matriz de correlación/covarianza, selección por autovalor, número fijo o varianza acumulada, rotación opcional, gráficos de sedimentación/componentes, diagnósticos y exportación.
- Se añadió validación de cálculos y exportaciones factoriales y PCA.

## v0.8.11

### Modificado

- Se restauró el logotipo horizontal EasyFlow Statistics en la barra de navegación en lugar de componerlo con icono y texto HTML.

## v0.8.10

### Modificado

- Se corrigió el contraste de la marca de navegación para mantener visibles el texto EasyFlow Statistics y la versión sobre el encabezado claro.
- Se normalizó la significación post hoc ordenada para mostrar consistentemente patrones compartidos como `3, 2>1` y `3>2, 1`.
- Se mejoró la devolución de variables desde las listas dependientes o independientes de prueba t / ANOVA.
- Se refinó la inferencia de medición para no clasificar números decimales como categóricos solo por tener pocos valores únicos.
- Se limitó la limpieza del lanzador al puerto de la aplicación antes de iniciar otra sesión EasyFlow Statistics.
- Se añadió detección automática de valores perdidos con conversión revisada a `NA`.
- Se añadió transformación mediante fórmulas para crear variables a partir de expresiones numéricas, textuales, estadísticas, de fecha y condicionales.
- Se reorganizó el Editor de datos y se unificó Recodificar variable con destinos en la misma variable o en otra nueva.

## v0.8.7

### Modificado

- Se añadió detección automática de texto Likert y conversión por lotes de encuestas importadas.
- Se añadieron controles agrupados Likert para texto de ítems, etiquetas originales, valores numéricos, codificación inversa y tipo posterior de variable.
- Se mejoraron los niveles Likert parciales para alinear con la escala completa los ítems sin algunos niveles observados.
- Se estrecharon las columnas estadísticas compactas de regresión jerárquica para facilitar la lectura.

## v0.8.6

### Modificado

- Se añadió revisión de variables en Paso 3 con vistas Etiquetas / Variables y Aplicar unificado para etiquetas de valores, de variables y tipos de medición.
- Se garantizó la propagación a Análisis de los tipos modificados en Paso 3 al aplicar.
- Se mantuvieron los controles de revisión del Paso 3 coherentes con el diseño actual de Datos.

## v0.8.4

### Modificado

- Se mejoraron tablas de frecuencias, notas de prueba t / ANOVA, p/IC de correlación, ítems de fiabilidad, espaciado Durbin-Watson, anotaciones jerárquicas y resultados logísticos inestables.
- Se añadió edición masiva de tipos de medición en Paso 2 para variables marcadas en la página actual de Datos.
- Se conservaron los niveles de medición originales cuando la codificación inversa automática crea o sobrescribe variables.

## v0.8.3

### Modificado

- Se añadieron comprobación de errores de codificación, inversión automática, recodificación en otra variable y cálculo por filas al Editor de datos.
- Se añadieron controles para aplicar correcciones, vistas previas de variables creadas, guardado posterior de datos y validación de recodificación y lecturas CSV / DAT copiadas.
- Se normalizaron configuración/resultados de Editor de datos, Calculadora y Análisis, incluidos botones compartidos y comportamiento alternativo del visor de datos seleccionados.
- Se actualizaron opciones predeterminadas y controles post hoc no paramétricos, incluidos alfa ordinal de fiabilidad y espaciado prueba t / ANOVA.
- Se mejoró el manejo de archivos sincronizados en la nube copiando SAV, CSV y DAT a una ubicación temporal antes de importar.
- Se restauró la selección múltiple Ctrl / Shift / Ctrl+A en listas de transferencia manteniendo sincronización estable con Shiny.

## v0.8.2

### Modificado

- Se activaron opciones habituales por defecto en pruebas pareadas, frecuencias, correlación, fiabilidad, logística y prueba t / ANOVA.
- Se añadieron correcciones post hoc no paramétricas independientes para Kruskal-Wallis, con Bonferroni predeterminado y Holm Bonferroni disponible.
- Se ajustó el espaciado de prueba t / ANOVA para acomodar post hoc y tamaño del efecto en el panel.
- Se añadió recodificación sobre la misma variable al Editor de datos.

## v0.8.1

### Modificado

- Se unificaron Prueba pareada (2) y (3+) en una configuración que elige el análisis según el número de medidas repetidas.
- Se renombró Regresión jerárquica como Regresión y se retiró el menú separado, conservando regresión de un bloque y jerárquica multibloque.
- Se añadieron progreso y detención bootstrap al flujo unificado de regresión.
- Se actualizaron dimensiones de pruebas pareadas y marca de Datos.

## v0.8.0

### Modificado

- Se añadieron configuración/resultados logísticos para dependientes binarias, ordinales y multinomiales, con bloques jerárquicos, OR / IC, pseudo R2, VIF, ajuste y advertencias.
- Se añadieron controles compartidos Restablecer configuración, activos solo si el bloque de asignación contiene variables.
- Se normalizaron acceso al visor seleccionado, eliminación por doble clic y espaciado de tres paneles en los menús de análisis.
- Se compactaron los bloques iniciales vacíos de regresión y regresión jerárquica antes de ejecutar.

## v0.7.11

### Modificado

- Se cambió Tablas cruzadas para asignar columnas encima de filas, dimensionando ambos paneles según la cantidad prevista de variables.
- Se añadió PDF para tablas cruzadas y se activaron todas las acciones de guardado por defecto en desarrollo.
- Se normalizaron tablas cruzadas con estadísticos alineados arriba, encabezados de columna centrados, filas a la izquierda y notas numeradas de tamaño del efecto.
- Se normalizaron tamaños del efecto a tres decimales sin cero inicial en los resultados.
- Se añadieron notas numeradas de p, efecto y tendencia en prueba t / ANOVA con marcadores en superíndice.
- Se añadió validación de notas de prueba t / ANOVA y se amplió la de tablas cruzadas.

## v0.7.10

### Modificado

- Se añadió análisis de tablas cruzadas para variables binarias, ordinales y categóricas con chi-cuadrado de Pearson, alternativa exacta Fisher / Monte Carlo y tendencias.
- Se añadió asignación múltiple de filas/columnas ordenables, tablas agrupadas por columna, porcentajes de fila/columna/total y celdas n/porcentaje separadas opcionales.
- Se añadieron notas de método del valor p, p de tendencia con notas específicas, tamaños del efecto y exportación HTML / Excel de tablas cruzadas.
- Se añadió validación estadística, de presentación, orden de variables y auxiliares de exportación de tablas cruzadas.

## v0.7.9

### Modificado

- Se compactó y armonizó el espaciado de calculadoras EQ-5D, síndrome metabólico y gravedad metabólica.
- Se ocultó la tabla de criterios predeterminados de síndrome metabólico al elegir criterios personalizados.
- Se rediseñó Fórmula de gravedad metabólica según los paneles de referencia de las demás calculadoras y se separó Salida.
- Se añadió validación de HINT8, EQ-5D, síndrome metabólico, FRS, ASCVD10 y gravedad metabólica.

## v0.7.7

### Modificado

- Se cambió la visualización inicial HINT8 a una matriz compacta ítem por nivel.
- Se mantuvo visible la configuración HINT8 tras cargar datos aunque no haya variables ordinales.
- Se compactó el espaciado del panel de valores iniciales HINT8.

## v0.7.6

### Modificado

- Se cambió la lista larga de referencia inicial EQ-5D a una matriz compacta dimensión por nivel.

## v0.7.5

### Modificado

- Se corrigió Aplicar en Datos Paso 3 para aplicar etiquetas de variables/valores y tipos con un clic y conservarlos en la configuración guardada.
- Se restringieron PDF, Excel y Añadir resultado a exportación de pago, manteniendo HTML y figuras en modo gratuito.
- Se añadió PDF de regresión y regresión jerárquica con portada, orientación mixta, tablas anchas escaladas y páginas de gráficos en dos columnas.
- Se mejoró HTML guardado como visor con desplazamiento horizontal de tablas conservando el diseño original.
- Se normalizaron botones de guardado de regresión y jerárquica y se activaron sr2, f2 y VIF por defecto.
- Se corrigieron solicitudes repetidas de guardado tras cancelar y se redujeron errores de activación de pestañas de análisis.

## v0.7.4

### Modificado

- Se reorganizó la navegación superior en Datos, Editor de datos, Calculadora, Análisis, Resultados y Acerca de.
- Se agruparon Editor de datos y Análisis, incluidos menús anidados Prueba pareada y Regresión.
- Se mejoraron menús anidados para activar normalmente las pestañas Shiny de submenús de Análisis y Calculadora.
- Se redujeron demoras de configuración evitando vaciados innecesarios de tablas del Paso 3 y resúmenes repetidos de variables al navegar.

## v0.7.3

### Modificado

- Se añadieron módulos de calculadora HINT8, EQ5D, Síndrome metabólico, Gravedad metabólica, FRS y ASCVD10.
- Se incorporaron los resultados calculados a los datos cargados para utilizarlos en Análisis.
- Se eliminaron restos obsoletos de Datos Paso 4/5 y se finalizó la edición de etiquetas en Paso 3.
- Se normalizó la presentación de notas para ajustarlas al ancho de cada tabla de análisis.

## v0.7.2

### Modificado

- Se añadieron bloques de subfactores de Fiabilidad con resúmenes totales, análisis combinado de ítems y diagnóstico de eliminación para todos los ítems.
- Se refinó Fiabilidad para mostrar y validar omega solo cuando se activa su opción.
- Se ajustaron tamaños de listas, anchos de tablas y alineación de encabezados de Fiabilidad.

## v0.7.1

### Modificado

- Se refinaron etiquetas repetidas, encabezados agrupados, notas post hoc y anotaciones de efecto de Prueba pareada (3+).
- Se normalizaron alturas de listas y alineación de botones de transferencia en Fiabilidad, Frecuencias, Prueba pareada, prueba t/ANOVA, Correlación, Regresión y Jerárquica.

## v0.7.0

### Añadido

- Se añadió Prueba pareada (3+) para al menos tres mediciones, con selección de RM ANOVA, Friedman o Q de Cochran, comprobaciones de supuestos y post hoc.
- Se añadieron tamaños del efecto repetidos: eta cuadrado parcial, W de Kendall, g de Hedges y r de Wilcoxon.

### Modificado

- Se refinaron tablas pareadas, notación post hoc, ubicación del tamaño del efecto y diseños HTML/Excel.
- Se ajustó la selección pareada para usar filas de medidas repetidas agrupadas y destinos más compactos.

## v0.6.8

### Añadido

- Se añadió Prueba pareada para dos medidas, con elección t pareada/Wilcoxon, McNemar/McNemar exacta para pares binarios y Stuart-Maxwell/Bowker para pares categóricos.
- Se añadieron comprobaciones opcionales de diferencias pareadas con Shapiro-Wilk o asimetría/curtosis y detección de atípicos 3*IQR.
- Se añadió HTML y Excel para tablas pareadas y notas de supuestos.

## v0.6.7

### Modificado

- Se recentraron los botones de transferencia eliminando el desplazamiento inferior compartido y alineando los dos botones de regresión/prueba t con las filas de destino.

## v0.6.6

### Modificado

- Regresión y regresión jerárquica muestran coeficientes categóricos como `variable:level` e incluyen la referencia predeterminada aunque no se haya fijado explícitamente en Datos.

## v0.6.5

### Modificado

- Se restauró la alineación de transferencia de la geometría 0.5.7 y se aplicó a Fiabilidad.
- Se restauró la ubicación de botones de guardado jerárquico a la fila de acciones 0.5.7.

## v0.6.4

### Modificado

- Se aplicaron asteriscos de significación a la matriz de correlación cuando se selecciona la opción de niveles de significación.

## v0.6.3

### Modificado

- Se corrigieron cambios de medición del Paso 3 para transferir selecciones actuales junto con etiquetas al navegar a Análisis.
- Se incluyeron selectores de medición de etiquetas de categoría del Paso 3 en la recopilación directa de entradas del servidor.
- Se devolvió el bloque de guardado jerárquico debajo del tercer bloque de configuración.

## v0.6.2

### Modificado

- Se corrigió el filtrado de dependientes de regresión y jerárquica para respetar los niveles del Paso 3 al seleccionar dependientes continuas.
- Se realinearon botones de transferencia tras cambios compartidos de geometría de configuración.

## v0.6.1

### Modificado

- Se corrigió la propagación inmediata de tipos del Paso 3 a Fiabilidad, Frecuencias, prueba t/ANOVA, Correlación, Regresión y Jerárquica.

## v0.6.0

### Modificado

- Se añadió Fiabilidad con ítems del mismo nivel, elección automática KR-20/alfa de Cronbach/omega, alfa/omega ordinales, diagnósticos de ítems y notas según normalidad.
- Se normalizaron controles de guardado HTML/figuras/Excel/añadir resultado según edición entre pestañas de resultados.
- Se mejoraron HTML y Excel conservando notas al ancho de las tablas y columnas Excel legibles.
- Se añadió `psych` como motor de alfa, omega y coeficientes ordinales policóricos.

## v0.5.7

### Modificado

- Se rediseñó la configuración jerárquica para mostrar Dependientes y un Bloque activo, con navegación anterior/siguiente conservando variables de Bloque 1/2/3.
- Se ajustaron alturas, listas y navegación de bloques para un diseño jerárquico compacto y alineado.
- Se refinaron separadores entre coeficientes y ajuste del modelo en tablas jerárquicas.
- Se ajustaron anchos y relleno para resultados jerárquicos anchos de tres modelos con columnas de efecto.

## v0.5.6

### Modificado

- Se añadió HTML compartido a resultados y se armonizó su estilo con las tablas de regresión internas.
- Se amplió correlación con selección automática por nivel, correlaciones latentes opcionales, matrices de método/motivo, p e IC 95% y gráficos de dispersión/calor más grandes.
- Se refinó Excel/HTML de regresión y jerárquica con encabezados dobles, alineación numérica, notas y diálogos de guardado.
- Se estabilizaron opciones de regresión y jerárquica durante bootstrap.
- Se normalizó la geometría de bloques de configuración en análisis no jerárquicos.

## v0.5.5

### Modificado

- Se implementó Correlación con pares, normalidad opcional, valores p, intervalos, marcadores de significación, matrices y gráficos.
- Se añadió Excel de tablas prueba t / ANOVA.
- Se añadió Excel de tablas y exportación de figuras diagnósticas residuales de regresión jerárquica.
- Se actualizaron requisitos de paquetes locales para las nuevas dependencias analíticas.

## v0.5.4

### Modificado

- Se añadieron tamaño del efecto, tendencias, significación ordenada y post hoc ampliado a prueba t / ANOVA.
- Se refinaron opciones de normalidad, etiquetas de resumen y estadísticos, notas p y diseño de prueba t / ANOVA.
- Se añadió prueba de rangos múltiples de Duncan mediante agricolae y se actualizó la carga de paquetes.
- Se corrigieron columnas opcionales de Frecuencias / Descriptivos y se adoptaron anchos compactos de estilo regresión.
- Se refinaron espaciado, separadores y etiquetas chi-cuadrado de regresión jerárquica.

## v0.5.2

### Modificado

- Se añadieron número de remuestras bootstrap y semilla al resumen del modelo cuando se usa regresión bootstrap.
- Se estabilizó la transferencia de regresión para seleccionar el primer ítem con Shift y se redujeron reinicios de desplazamiento por selección.
- Se aumentó la altura de variables disponibles de regresión para mostrar 20.
- Se añadieron y refinaron conceptos de logotipo SVG de EasyFlow Statistics.

## v0.5.1

### Modificado

- Se estabilizaron selección múltiple, Ctrl+A, dirección y orden de transferencia de regresión.
- Se refinaron diseño, alturas fijas, botones y persistencia de casillas de opciones de regresión.
- Se añadió comportamiento compartido de exportación de tablas y figuras de análisis.
- Se añadió estructura de configuración/resultados de Frecuencias / Descriptivos con interfaz compartida de transferencia.
- Se actualizaron metadatos de cita de EasyFlow Statistics.

## v0.5.0

### Añadido

- Se añadió estructura de pestaña Jerárquica para regresión múltiple jerárquica con una dependiente y predictores en Bloque 1/2/3.
- Se añadieron controles de transferencia de Bloque 2 a 3 para la futura configuración jerárquica.
- Se añadió estructura de pestaña Generalizado para futuros modelos de regresión generalizada.

### Modificado

- Se actualizaron opciones Generalizado para GLM, retirando bootstrap y sr2/f2 exclusivos de OLS.
- Se agruparon conteos como Poisson / Binomial negativa / Inflación de ceros y se conservó Gamma para resultados continuos positivos.
- Se actualizaron opciones de presentación Generalizado para usar exp(B) como IRR / razón.

## v0.4.1

### Modificado

- Se renombraron pestaña y encabezados de regresión de EasyFlow Statistics a Regresión.

### Corregido

- Se corrigió el texto vacío de advertencias VIF que podía mostrar `missing value where TRUE/FALSE needed`.

## v0.4.0

### Añadido

- Se añadieron diálogos Windows nativos para guardar tablas Excel y seleccionar carpeta de figuras.
- Se añadió libro Excel con estilo de revista, coeficientes, filas de ajuste, diagnósticos y notas.
- Se añadieron advertencias de multicolinealidad VIF con orientación para valores graves.
- Se añadieron Ridge, LASSO y Elastic Net con validación cruzada para multicolinealidad grave.
- Se añadieron tablas SCI de regresión penalizada con rendimiento, comparación de coeficientes OLS/penalizados y predictores retenidos.

### Modificado

- Se mejoró Resumen del modelo en Excel con celdas combinadas de independientes compartidas, ajuste de texto y anchos compactos.
- Se ocultaron diagnósticos residuales y Durbin-Watson al mostrar regresión penalizada.
- Se nombraron las hojas de regresión con etiquetas o nombres de dependientes.

### Corregido

- Se corrigieron errores al guardar configuración sin variables categóricas seleccionadas.
- Se impidió almacenar nombres vacíos en sustituciones de niveles de medición.

## v0.3.1

### Modificado

- Se unificó el Resumen del modelo en una tabla para múltiples dependientes.
- Se ordenó la regresión con todas las tablas de coeficientes primero y después gráficos diagnósticos.
- Se unificaron comprobaciones de supuestos y Durbin-Watson en una tabla cada una para todas las dependientes.
- Se mostraron dependientes por etiqueta si existe y, en caso contrario, por nombre.
- Se mostraron criterios de tamaño del efecto una sola vez tras las tablas de coeficientes.

## v0.2.0

### Añadido

- Se añadió salida secuencial de regresión para múltiples dependientes.
- Se añadieron progreso y detención bootstrap en configuración de regresión.
- Se añadieron sr2, f2 y diagnósticos VIF/colinealidad opcionales.
- Se añadieron referencias de criterios de efecto para sr2 y f2 de Cohen.
- Se añadieron gráficos diagnósticos residuales contiguos.

### Modificado

- Se rediseñó la configuración de regresión con Variables, Dependientes, Independientes y controles bootstrap.
- Se actualizó Resumen del modelo para informar dependiente, independientes, N, R2(adj. R2), F(p) y método.
- Se normalizaron gráficos de homocedasticidad residual y límites de atípicos.
- Se mejoraron superíndices/subíndices en regresión.

### Corregido

- Se corrigió la edición de etiquetas para no reiniciar el texto con cada carácter.
- Se corrigieron carga de configuración y propagación entre pasos de etiquetas de variables, mediciones, referencias y etiquetas de valores.
- Se corrigieron detención bootstrap y ubicación del progreso.

## v0.1.2

### Añadido

- Se añadieron controles Arriba/Abajo bajo Dependientes en configuración de regresión.
- Se conservó el orden de dependientes en configuración guardada y resúmenes.

## v0.1.1

### Corregido

- Se habilitó el botón de encabezado `selected` del Paso 3 para seleccionar o desmarcar todas las variables visibles del rol activo.
- Se cambió Aplicar del Paso 3 para enviar el estado actual de las casillas DataTables.
- Se conservaron y sincronizaron ediciones `var_label`, `reference`, `value` y `label` al redibujar DataTables.

## v0.1.0

### Añadido

- Prototipo inicial de la aplicación Shiny.
- Carga CSV y selección de variables.
- Análisis de regresión múltiple.
- Prueba de normalidad residual Kolmogorov-Smirnov con corrección de Lilliefors.
- Prueba de homocedasticidad Breusch-Pagan.
- Errores estándar robustos HC3.
- Intervalos de confianza bootstrap.
- Consulta dL/dU de Durbin-Watson mediante `C:/StatEdu/easyflow_statistics/easyflow_statistics_3.0.xlsx`.
