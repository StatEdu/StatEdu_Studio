# Analysen — StatEdu Studio 1.3.0

Analyseumfang und Ausgabe der öffentlichen Version StatEdu Studio 1.3.0.

## Inhalt

1. [Öffentlicher Umfang von 1.3.0](#scope)
2. [Datenvorbereitung](#data)
3. [Häufigkeiten, Deskriptivstatistik und Kreuztabellen](#descriptive)
4. [Gruppenvergleiche und ANCOVA](#group)
5. [Gepaarte und gemischte Messwiederholungen](#paired)
6. [Korrelation, Reliabilität und Übereinstimmung](#correlation)
7. [Explorative Faktorenanalyse und PCA](#factor)
8. [Wichtigkeits–Leistungsanalyse (IPA)](#ipa)
9. [Regression und hierarchische Regression](#regression)
10. [Mediations- und Moderationseffekte](#mediation)
11. [Logistische, GLM- und penalierte Regression](#generalized)
12. [Längsschnitt-, Panel- und Survey-Daten](#longitudinal)
13. [Überlebenszeitanalyse](#survival)
14. [Konfirmatorische Faktorenanalyse (CFA)](#cfa)
15. [Strukturgleichungsmodelle (SEM)](#sem)
16. [PLS-SEM und PLSc](#pls)
17. [Stichprobengröße, Teststärke und Effektgröße](#planning)
18. [Tabellen, Abbildungen und Export](#reporting)
19. [Validierung und Aussagegrenzen](#validation)

<a id="scope"></a>

## 1. Öffentlicher Umfang von 1.3.0

CFA, SEM, PLS-SEM und Mediations-/Moderationseffekte sind öffentliche Analysemenüs. Der öffentliche Installer enthält weder Metaanalyse noch die ANOVA für wiederholte Behandlungen derselben Personen. Pro ist für eine spätere Version vorgesehen.

<a id="data"></a>

## 2. Datenvorbereitung

Importieren Sie SPSS, SAS, Stata, Excel, CSV und DAT; prüfen Sie Namen, Beschriftungen, Messniveaus, Kategorien und Referenzen. Prüfen Sie je Analyse die verwendete Stichprobengröße und den Umgang mit fehlenden Werten.

<a id="descriptive"></a>

## 3. Häufigkeiten, Deskriptivstatistik und Kreuztabellen

Berichten Sie Häufigkeiten, Prozente, Lage- und Streuungsmaße sowie Kontingenztafeln. Gewählte Optionen liefern Zusammenhangstests, Effektgrößen und Diagnostik.

<a id="group"></a>

## 4. Gruppenvergleiche und ANCOVA

Nutzen Sie unabhängige t-Tests, ANOVA, ANCOVA und nichtparametrische Gruppenvergleiche. Unterstützte Optionen umfassen varianzangepasste Verfahren, Post-hoc-Vergleiche und Effektgrößen.

<a id="paired"></a>

## 5. Gepaarte und gemischte Messwiederholungen

Nutzen Sie gepaarte Analysen, Messwiederholungen, gemischte ANOVA und gepaarte nichtparametrische Verfahren. Berichten Sie Zeit-, Gruppeneffekte und Vergleiche des gewählten Designs.

<a id="correlation"></a>

## 6. Korrelation, Reliabilität und Übereinstimmung

Analysieren Sie Korrelationen, Skalenreliabilität und Beurteilerübereinstimmung. Wählen Sie Verfahren und Kennwerte passend zu Variablentyp und Beurteilungsdesign.

<a id="factor"></a>

## 7. Explorative Faktorenanalyse und PCA

Prüfen Sie Faktoren-/Komponentenzahl, Extraktion, Rotation, Ladungen und erklärte Varianz in EFA und PCA. CFA wird separat beschrieben.

<a id="ipa"></a>

## 8. Wichtigkeits–Leistungsanalyse (IPA)

Öffnen Sie Analyse → IPA und wählen Sie direkte Bewertungen oder abgeleitete Wichtigkeit. Ordnen Sie Wichtigkeit und Leistung in gleicher Attributreihenfolge zu oder Leistung und Gesamtzufriedenheit. Wählen Sie Gesamtstichprobe, unabhängige Gruppen oder gepaarte Vorher/Nachher-Daten. Verwenden Sie entsprechende WIDE-Spalten oder LONG-ID/Zeit und zwei Zeitwerte. Prüfen Sie Referenzen, Diagramme, Fallzahlen, Koordinaten, Intervalle und Unterschiede. Word/HWPX speichern Sie nach Hinzufügen aus der Ergebnissammlung.

<a id="regression"></a>

## 9. Regression und hierarchische Regression

Nutzen Sie OLS, robuste HC3-Inferenz und Bootstrap-Regression. Hierarchische Analysen vergleichen aufeinanderfolgende Blöcke und Änderungen erklärter Varianz. Je Auswahl erscheinen sr², f², Kollinearität und Residuendiagnostik. Die hierarchische Regression unterstützt bis zu vier Blöcke. Jeder Schritt behält die Variablen der vorherigen Blöcke bei und fügt den nächsten Block hinzu.

<a id="mediation"></a>

## 10. Mediations- und Moderationseffekte

Definieren Sie Prädiktor, Ergebnis, Mediator, Moderator und Kovariaten und zeichnen Sie Pfade für direkte, indirekte, Gesamt- und bedingte Effekte. Es wird keine Modellnummer ausgewählt. Nicht unterstützte Pfadstrukturen werden vor der Ausführung geprüft.

<a id="generalized"></a>

## 11. Logistische, GLM- und penalierte Regression

Nutzen Sie logistische und verallgemeinerte lineare Modelle, Ridge, LASSO und Elastic Net. Berichten Sie Koeffizienten und Leistung mit Ergebnisskala, Familie, Link und Kreuzvalidierungseinstellungen.

<a id="longitudinal"></a>

## 12. Längsschnitt-, Panel- und Survey-Daten

Unterstützte Designs umfassen GEE, LMM, GLMM, feste/zufällige Effekte und komplexe Stichproben. Prüfen Sie Personen-/Clusterkennungen, Zeit, Gewichte, Schichten und Cluster.

<a id="survival"></a>

## 13. Überlebenszeitanalyse

Nutzen Sie Kaplan–Meier, Log-Rank, RMST, Cox und unterstützte konkurrierende Risiken. Prüfen Sie zuerst Eingabeformat und Ereigniscodes; nicht unterstützte Kombinationen werden eingeschränkt.

<a id="cfa"></a>

## 14. Konfirmatorische Faktorenanalyse (CFA)

Nutzen Sie ML, MLR und WLSMV/DWLS für ordinale Daten; prüfen Sie Ladungen, Anpassung, Reliabilität, AVE, HTMT und unterstützte Messinvarianzvergleiche. Normal/Wishart ist auf geeignete ML-Einstellungen beschränkt.

<a id="sem"></a>

## 15. Strukturgleichungsmodelle (SEM)

Berichten Sie Mess-/Strukturpfade, Anpassung und unterstützte direkte, indirekte und bedingte Effekte. Bootstrap verwendet standardmäßig 5,000 Wiederholungen; Ergebnisse berücksichtigen gewählte BC-/Perzentilintervalle und weitere Optionen.

<a id="pls"></a>

## 16. PLS-SEM und PLSc

Nutzen Sie reflektiven Mode A, formativen Mode B, reflektives PLSc, Pfad-/Messdiagnostik und unterstützte Vorhersage-/Gruppenvergleiche. Fehlwerte werden über seminr::mean_replacement durch Indikatormittelwerte ersetzt, die jede Bootstrap-Stichprobe neu berechnet. Standard sind 5,000 Wiederholungen.

<a id="planning"></a>

## 17. Stichprobengröße, Teststärke und Effektgröße

Berechnen Sie Stichprobengröße, Teststärke und Effektgröße für unterstützte Tests. Dokumentieren Sie Alpha, angenommene Effekte, Gruppenverteilung und Testrichtung.

<a id="reporting"></a>

## 18. Tabellen, Abbildungen und Export

Die öffentliche Version 1.3.0 exportiert HTML/Bilder und PDF/Word/Excel. HTML/PDF-Berichte für Mediation/Moderation, CFA, SEM und PLS-SEM hängen die Ergebnismodellgrafik an. Bilder bewahren die angezeigte Anordnung; Free nutzt 300 dpi, Entwicklung/Pro 600 dpi. Prüfen und sammeln Sie Ergebnisse. HTML, PDF, Word und Excel werden unterstützt. HWPX ist nur bei koreanischer Oberfläche in gesammelten Ergebnissen verfügbar und wird ohne Word/Hancom direkt geschrieben. Word/HWPX bieten Haupttabellen, Anhänge, Erläuterungen und Abbildungen; Haupttabellen sind vorausgewählt. Gespeichert wird die erfasste Darstellung ohne Neuberechnung. Free nutzt 300 dpi, Entwicklung 600 dpi. HTML enthält Deckblatt und verlinkte Tabellenliste.

<a id="validation"></a>

## 19. Validierung und Aussagegrenzen

Allgemeine Analysen, Regressionen, Längsschnitt- und Überlebenszeitanalysen wurden mit SPSS verglichen; CFA/SEM mit AMOS; PLS-SEM/PLSc und CB-SEM mit SmartPLS. Diese kumulierten Prüfungen sind in 1.3.0 eingeflossen; die Validierungsseite fasst Bedingungen und verbleibende Unterschiede zusammen.
