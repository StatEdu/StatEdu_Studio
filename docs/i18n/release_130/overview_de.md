# Überblick — StatEdu Studio 1.3.0

StatEdu Studio ist eine statistische Windows-Anwendung für Datenvorbereitung, Analysen, Modellvisualisierung, Stichprobenplanung und Ergebnisberichte. Ordnen Sie Variablen und Optionen über die Oberfläche zu und prüfen Sie Methoden, Annahmen, Diagnostik und Interpretation. Dieser Überblick beschreibt die gesamte Anwendung, nicht nur Neuerungen in 1.3.0.

## Datenbearbeitung, Analyseumfang und Rechner

Importieren Sie SPSS, SAS, Stata, Excel, CSV und DAT; prüfen Sie Namen, Beschriftungen, Messniveaus, Kategorien und Referenzen. Prüfen Sie je Analyse die verwendete Stichprobengröße und den Umgang mit fehlenden Werten.

Verwalten Sie Namen, Labels, Skalenniveaus und Kategorien; kodieren und berechnen Sie Variablen, behandeln Sie fehlende Werte, führen Sie Daten zusammen, aggregieren Sie nach ID und formen Sie WIDE–LONG um. Fallauswahl und Aufteilung bestimmen den Umfang. Rechner für EQ-5D, HINT-8, Framingham, ASCVD und metabolische Maße sind enthalten.

## Verfügbare Analysen

### Häufigkeiten, Deskriptivstatistik und Kreuztabellen

Berichten Sie Häufigkeiten, Prozente, Lage- und Streuungsmaße sowie Kontingenztafeln. Gewählte Optionen liefern Zusammenhangstests, Effektgrößen und Diagnostik.

### Gruppenvergleiche und ANCOVA

Nutzen Sie unabhängige t-Tests, ANOVA, ANCOVA und nichtparametrische Gruppenvergleiche. Unterstützte Optionen umfassen varianzangepasste Verfahren, Post-hoc-Vergleiche und Effektgrößen.

### Gepaarte und gemischte Messwiederholungen

Nutzen Sie gepaarte Analysen, Messwiederholungen, gemischte ANOVA und gepaarte nichtparametrische Verfahren. Berichten Sie Zeit-, Gruppeneffekte und Vergleiche des gewählten Designs.

### Korrelation, Reliabilität und Übereinstimmung

Analysieren Sie Korrelationen, Skalenreliabilität und Beurteilerübereinstimmung. Wählen Sie Verfahren und Kennwerte passend zu Variablentyp und Beurteilungsdesign.

### Explorative Faktorenanalyse und PCA

Prüfen Sie Faktoren-/Komponentenzahl, Extraktion, Rotation, Ladungen und erklärte Varianz in EFA und PCA. CFA wird separat beschrieben.

### Wichtigkeits–Leistungsanalyse (IPA)

Öffnen Sie Analyse → IPA und wählen Sie direkte Bewertungen oder abgeleitete Wichtigkeit. Ordnen Sie Wichtigkeit und Leistung in gleicher Attributreihenfolge zu oder Leistung und Gesamtzufriedenheit. Wählen Sie Gesamtstichprobe, unabhängige Gruppen oder gepaarte Vorher/Nachher-Daten. Verwenden Sie entsprechende WIDE-Spalten oder LONG-ID/Zeit und zwei Zeitwerte. Prüfen Sie Referenzen, Diagramme, Fallzahlen, Koordinaten, Intervalle und Unterschiede. Word/HWPX speichern Sie nach Hinzufügen aus der Ergebnissammlung.

### Regression und hierarchische Regression

Nutzen Sie OLS, robuste HC3-Inferenz und Bootstrap-Regression. Hierarchische Analysen vergleichen aufeinanderfolgende Blöcke und Änderungen erklärter Varianz. Je Auswahl erscheinen sr², f², Kollinearität und Residuendiagnostik. Die hierarchische Regression unterstützt bis zu vier Blöcke. Jeder Schritt behält die Variablen der vorherigen Blöcke bei und fügt den nächsten Block hinzu.

### Mediations- und Moderationseffekte

Definieren Sie Prädiktor, Ergebnis, Mediator, Moderator und Kovariaten und zeichnen Sie Pfade für direkte, indirekte, Gesamt- und bedingte Effekte. Es wird keine Modellnummer ausgewählt. Nicht unterstützte Pfadstrukturen werden vor der Ausführung geprüft.

### Logistische, GLM- und penalierte Regression

Nutzen Sie logistische und verallgemeinerte lineare Modelle, Ridge, LASSO und Elastic Net. Berichten Sie Koeffizienten und Leistung mit Ergebnisskala, Familie, Link und Kreuzvalidierungseinstellungen.

### Längsschnitt-, Panel- und Survey-Daten

Unterstützte Designs umfassen GEE, LMM, GLMM, feste/zufällige Effekte und komplexe Stichproben. Prüfen Sie Personen-/Clusterkennungen, Zeit, Gewichte, Schichten und Cluster.

### Überlebenszeitanalyse

Nutzen Sie Kaplan–Meier, Log-Rank, RMST, Cox und unterstützte konkurrierende Risiken. Prüfen Sie zuerst Eingabeformat und Ereigniscodes; nicht unterstützte Kombinationen werden eingeschränkt.

### Konfirmatorische Faktorenanalyse (CFA)

Nutzen Sie ML, MLR und WLSMV/DWLS für ordinale Daten; prüfen Sie Ladungen, Anpassung, Reliabilität, AVE, HTMT und unterstützte Messinvarianzvergleiche. Normal/Wishart ist auf geeignete ML-Einstellungen beschränkt.

### Strukturgleichungsmodelle (SEM)

Berichten Sie Mess-/Strukturpfade, Anpassung und unterstützte direkte, indirekte und bedingte Effekte. Bootstrap verwendet standardmäßig 5,000 Wiederholungen; Ergebnisse berücksichtigen gewählte BC-/Perzentilintervalle und weitere Optionen.

### PLS-SEM und PLSc

Nutzen Sie reflektiven Mode A, formativen Mode B, reflektives PLSc, Pfad-/Messdiagnostik und unterstützte Vorhersage-/Gruppenvergleiche. Fehlwerte werden über seminr::mean_replacement durch Indikatormittelwerte ersetzt, die jede Bootstrap-Stichprobe neu berechnet. Standard sind 5,000 Wiederholungen.

### Stichprobengröße, Teststärke und Effektgröße

Berechnen Sie Stichprobengröße, Teststärke und Effektgröße für unterstützte Tests. Dokumentieren Sie Alpha, angenommene Effekte, Gruppenverteilung und Testrichtung.

## Ergebnisse prüfen, sammeln und speichern

Prüfen und sammeln Sie Ergebnisse. HTML, PDF, Word und Excel werden unterstützt. HWPX ist nur bei koreanischer Oberfläche in gesammelten Ergebnissen verfügbar und wird ohne Word/Hancom direkt geschrieben. Word/HWPX bieten Haupttabellen, Anhänge, Erläuterungen und Abbildungen; Haupttabellen sind vorausgewählt. Gespeichert wird die erfasste Darstellung ohne Neuberechnung. Free nutzt 300 dpi, Entwicklung 600 dpi. HTML enthält Deckblatt und verlinkte Tabellenliste.

## Dokumentation und Umfang

Öffnen Sie Überblick, Handbuch, Analysen, Methodische Hinweise, Validierung und Versionsverlauf im Informationsmenü. Statistische Haupttabellen bleiben englisch; Menüs und Ergänzungen folgen der Oberflächensprache. Die Validierung gilt für genannte Daten und Optionen, nicht alle Kombinationen oder muttersprachliche Prüfung.

Die Entwicklerversion heißt StatEdu Studio Dev 1.3.0-dev und verwendet einen separaten App-Namen mit Entwicklungsmenüs. Die öffentliche Ausgabe schließt Metaanalyse und die ANOVA wiederholter Behandlungen innerhalb derselben Personen aus; gemischte Messwiederholungs-ANOVA und gepaarte Tests bleiben verfügbar.
