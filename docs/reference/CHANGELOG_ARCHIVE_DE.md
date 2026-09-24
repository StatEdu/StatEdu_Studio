# Versionsverlauf

## v1.2.0 - 2026-08-06

### Hinzugefügt

- Der öffentliche Analyseumfang wurde um die Beurteilerübereinstimmung erweitert; die Ausgabe priorisiert den empfohlenen Übereinstimmungsindex und enthält weiterhin ergänzende Indizes.
- Gemischte ANOVA mit Messwiederholung für Vorher-Nachher- und mehrzeitige Gruppenvergleiche mit PP/ITT-Analysewegen, kovariatenbereinigten Zusammenfassungen, Voraussetzungsprüfung, Post-hoc-Vergleichen und HTML/PDF/Excel-Exporten hinzugefügt.
- Die Modellfläche für benutzerdefinierte Mediation/Moderation wurde der öffentlichen Gruppe Regression / Modelle hinzugefügt.

### Geändert

- Die stabilisierten Arbeiten nach 1.1.3 wurden in die offiziellen Release-Metadaten `1.2.0` übernommen.
- ANOVA mit Messwiederholung wurde in das Menü Gruppenvergleich integriert; Einrichtung, Voraussetzungsprüfung, Sphärizitäts-/Levene-Prüfungen, Normalitätshinweise und Empfehlungen wurden an den geführten Analyseablauf von StatEdu Studio angepasst.
- Die statistische Validierung wurde auf Korrelation, Reliabilität, Beurteilerübereinstimmung, Faktorenanalyse / PCA, t-Test / ANOVA, gepaarte Messwiederholungen, Regression, logistische Regression, Längsschnitt-/Panelmodelle, regularisierte Regression, Stichprobengröße, Effektgröße, Dateneditor, benutzerdefinierte Modelle und gemischte Messwiederholungen erweitert.
- Die koreanische Menübezeichnung für benutzerdefinierte Modelle wurde für eine verständlichere öffentliche Navigation geändert.

### Behoben

- Der Startmechanismus der öffentlichen Electron-Ausgabe wurde korrigiert, sodass die Modellfläche für benutzerdefinierte Mediation/Moderation im Installationsprogramm 1.2.0 standardmäßig aktiviert ist.
- Doppelte Ergebniszeilen bei wiederverwendeten angepassten benutzerdefinierten Modellen wurden bereinigt.

## v1.1.3 - 2026-07-12

### Geändert

- Der aktuelle stabilisierte Entwicklerbuild wurde zum offiziellen Installationsprogramm `1.1.3` erhoben.
- Der öffentliche Release-Umfang bleibt an der Paketierungsregel von 1.1.1 ausgerichtet: Dokumentation, Tests, Beispiele, Quelltexte und andere nicht zur Laufzeit benötigte Inhalte der eingebundenen R-Laufzeit werden ausgeschlossen.
- Mehrsprachige Texte für neue Projektordnerbezeichnungen der latenten Analyse, Datendatei-Platzhalter, DAT-Trennzeichenoptionen und gemeinsame Analysesteuerelemente hinzugefügt.

### Behoben

- Die Excel-Importprüfung wurde korrigiert, sodass ausgewählte Dateien bedienbar bleiben und über die Prüfung von Arbeitsblatt und Startzelle geladen werden.
- Gemeinsame Vorschauen „Ausgewählte Daten anzeigen“ wurden auf ausgewählte Variablen und 15 Zeilen begrenzt.
- Die Auflösung der Projekt-/Ausgabeordner von Latent Mplus wurde korrigiert, sodass Ergebnisse neben der ursprünglich geladenen Datendatei entstehen, wenn deren ursprünglicher Pfad verfügbar ist.
- Beim Speichern der Latent-Mplus-Einstellungen wird nun ein Speicherdialog zur Dateinamenauswahl geöffnet.
- Die Anzeige des Projekt-/Ausgabeordners von Latent Mplus wurde linksbündig ausgerichtet.
- Verbleibende direkte Oberflächenbeschriftungen wurden über die gemeinsame i18n-Tabelle geleitet, damit Sprachüberlagerungen neue Daten-, Regressions-, Mediations-, Moderations- und latente Analyseoptionen übersetzen können.
- Die Modellfläche für benutzerdefinierte Mediation/Moderation bleibt aus der offiziellen Ausgabe ausgeschlossen.

## v1.1.1 - 2026-07-07

### Behoben

- Ein Patch-Installationsprogramm wurde veröffentlicht, damit Windows-Upgrades die paketierten Desktopdateien ersetzen, statt eine veraltete Installation derselben Version wiederzuverwenden.
- Die Electron-Startdiagnose wurde durch ein längeres Shiny-Startzeitfenster und die Protokollierung der R-Prozessausgabe bei Startfehlern verbessert.
- Das Laden von Desktop-Datendateien, die koreanische Startsprachauswahl, die Behandlung komplexer Stichprobendesigndateien und die Erhaltung leerer Beschriftungen wurden korrigiert.

## v1.1.0 - 2026-07-06

### Hinzugefügt

- Die öffentliche Version 1.1.0 wurde mit allen Analyseabläufen außer der Modellfläche für benutzerdefinierte Mediation/Moderation freigegeben.
- Speicherbeschränkungen für die öffentliche Ausgabe hinzugefügt: HTML-Speichern bleibt standardmäßig aktiv; Abbildungs-, PDF-, Excel- und Ergebnis-hinzufügen-Steuerelemente bleiben sichtbar, sind aber deaktiviert.
- t-Test / ANOVA bleibt die öffentliche Exportausnahme mit aktivem HTML-, Abbildungs-, PDF-, Excel- und Ergebnis-hinzufügen-Zugriff; t-Test-/ANOVA-Ergebnissammlungen können Excel und Word exportieren.

### Geändert

- Die Electron-Release-Profilierung wurde so geändert, dass endgültige semantische Versionen als öffentliche StatEdu-Studio-Installationsprogramme gebaut werden.

## v1.0.1 - 2026-06-28

### Geändert

- Die Umschaltung zwischen koreanischer und englischer Oberfläche wurde in Hauptmenüs, Einrichtungsseiten, Rechnern, Dokumentation und Mitteilungen stabilisiert; Ergebnistabellen bleiben englisch.
- Dateneditor und Analysemenüs wurden mit gruppierten Kategorien, korrigierten koreanischen Bezeichnungen sowie abgestimmten Schaltflächen- und Registerkartenlayouts aktualisiert.
- Normalitätsgrenzwerte für t-Test / ANOVA, geordnete Post-hoc-Markierungsübersichten sowie Layout und Export von Kreuztabellen wurden verbessert.
- Steuerelemente für Ausgabevariablennamen in Rechnern hinzugefügt und die Bereiche der EQ-5D- und ASCVD10-Rechner verbessert.
- Updateprüfung, Paketierungsmetadaten zur `.studio`-Dateizuordnung und das `.studio`-Dateisymbol hinzugefügt.
- Ein Hilfemenü für Fehlerberichte, Funktionswünsche, Analyseanfragen, Fragen und Antworten sowie Updateprüfungen hinzugefügt.
- Hilfeanfragen wurden mit Formularen der StatEdu-Studio-Website verknüpft; Fragen und Antworten richten sich nach der Oberflächensprache.
- Bildschirmabbildungen des Benutzerhandbuchs 1.0 und zweisprachige Dokumentationsdateien aktualisiert.

## v1.0.0 - 2026-06-25

### Geändert

- Die stabilisierte Versionslinie wurde auf die öffentlichen Versionsmetadaten 1.0.0 umgestellt.
- Die Electron-Paketmetadaten wurden von Beta-Bezeichnungen auf endgültige StatEdu-Studio-Release-Namen umgestellt.
- Zurückgestellte öffentliche Aussagen zu 1.0, DOI-Prüfung, Website-Prüfung und Prüfanforderungen für die paketierte Ausgabe bleiben ausdrücklich in den Release-Dokumenten aufgeführt.
- Geordnete Post-hoc-Markierungen zeigen nur unmittelbar signifikante Vergleiche in Mittelwertreihenfolge; transitive Ketten wie `b>a>c` werden vermieden, wenn ein Paar nicht signifikant ist.
- Breitenabhängige PDF-Ausrichtung für Kreuztabellen hinzugefügt: breite Haupttabellen im Querformat, schmalere und ergänzende Tabellen im Hochformat.
- Wählbare Schiefe-/Kurtosis-Normalitätsgrenzen für t-Test / ANOVA hinzugefügt: 2/5, 2/7 und 3/7; 2/7 bleibt Standard.
- Der Dialog zum Speichern von Einstellungen öffnet nun den Ordner der geladenen Datendatei, wenn deren Pfad verfügbar ist.
- Die Excel-Importprüfung wird beim Start nur angezeigt, wenn ein gültiger ausstehender Excel-Dateipfad vorliegt.

## v0.9.42 - 2026-06-23

### Hinzugefügt

- Dateneditor > Breit nach Lang zum Umformen von Messwiederholungsspalten ins Langformat vor Längsschnitt-/Panelanalysen hinzugefügt.
- Validierung für Breit-nach-Lang-Umformung hinzugefügt und Umkodierungsprüfungen des Dateneditors erweitert.
- Shiny-Start- und Electron-Release-Kurztests zur Prüfung von Release-Kandidaten hinzugefügt.
- UTF-8-Prüfung versionierter Dokumentation für Release-Kandidaten hinzugefügt.

### Geändert

- Bereiche, Schaltflächen, Abstände und Datenansichten des Dateneditors wurden nach dem Dialogmuster von t-Test / ANOVA vereinheitlicht.
- Die Fehlwertbehandlung unterstützt nun sowohl benutzerdefinierte Fehlwertmarkierungen als auch die Umwandlung in System-NA.
- Umkodieren und Umbenennen wurden durch Entfernen vorgemerkter Variablen, ausgerichtete Zielbereiche und klarere Regeleinstellungen verbessert.
- Einstellungsdateien wurden auf das Format `.studio` vereinfacht und Speicher-/Ladedialoge angepasst.
- Das Layout von Längsschnitt-/Panelmodellen und die Referenzkategorien-Berichterstattung kategorialer Prädiktoren wurden verbessert.
- Release-Bereinigungsprüfungen für ausschließlich lokale Dateien, erzeugte Artefakte, Versionsmetadaten und Release-Dokumentation wurden verschärft.
- Der koreanische Produktplan wurde mit aktuellen Stabilisierungsprioritäten für 1.0 wiederhergestellt.
- Aktuelle koreanische Benutzer- und Methodendokumentation verweist nun auf 0.9.42; veraltete Angaben zur aktuellen Version werden geprüft.
- Der Vertriebs-, Lizenz- und Updateplan für 1.0 wurde für die Stabilisierungsphase 0.9.42 als geprüft markiert.
- Release-Bereinigungsprüfungen wurden in die zentrale Stabilisierungssuite aufgenommen, damit erzeugte Electron-Stagingdateien und lokale Artefakte nicht versehentlich versioniert werden.
- Statusverfolgung und Validierung der Release-Bereitschaft für Paketierung, DOI, Website und Zurückstellungen für 1.0 hinzugefügt.
- Ein Release-Vorabprüfungsskript führt vollständige Stabilisierungsvalidierung, Shiny-Start-Kurztest und Electron-Release-Kurztests aus.
- Ein Entscheidungsprotokoll für 1.0 zu Paketierung, DOI, Website, Editionsfreigaben, Lizenz, Updates und öffentlichen Versionshinweisen hinzugefügt.
- Layoutvorgaben und Validierung für die standardmäßige Schaltflächenplatzierung im dreiteiligen Dateneditor verschärft.
- Ein manuelles QA-Protokoll für visuelle, Daten-, Analyse-, Export- und paketierte Electron-Prüfungen von Release-Kandidaten hinzugefügt.
- Das manuelle QA-Protokoll wurde in die Electron-Release-Kurztests eingebunden.
- Der Status der Release-Bereitschaft dokumentiert nun die bestandene Release-Vorabprüfung.
- Versionsmetadaten werden strenger auf ungelöste Hindernisse für die öffentliche Ausgabe 1.0 geprüft.
- Vorlage und Validierung für manuelle QA-Nachweise von Release-Kandidaten hinzugefügt.
- Versionierte erzeugte Vergleichsartefakte aus `outputs/` entfernt und Ausgabeartefakte im Stammverzeichnis durch Release-Bereinigungsprüfungen gesperrt.
- Der vollständige Vorabprüfungsbefehl für die paketierte Electron-Ausgabe wurde in README und Versionsmetadatenvalidierung dokumentiert.
- Die Release-Freigabedokumentation stellt klar, dass die DOI-Zielseite `https://studio.statedu.com` ist.
- Klargestellt, dass der Vertriebs-, Lizenz- und Updateplan für 1.0 ein Planungsdokument ist und keine Implementierung eingeschränkter Editionen, Lizenzaktivierung, Updater oder öffentlicher Installationsinfrastruktur behauptet.
- README und Release-Bereitschaft warnen nun, dass der vorgesehene DOI vor jeder öffentlichen Zitierankündigung für 1.0 auflösbar sein muss.
- Die Prüfung von Quelltext-/Lizenzhinweisen wurde für öffentliche Quelltextverfügbarkeit, Drittanbieterhinweise, Lizenzberichte und Verweise auf eingebundene Lizenztexte verschärft.
- Release-Checkliste, README und manuelle QA-Anleitung wurden abgestimmt, sodass abgeschlossene QA-Nachweise bei Versionshinweisen und Validierungsartefakten aufbewahrt werden.
- Die Freigabebedingung zum Ersetzen der Electron-Betapaketnamen 0.9.x vor einem öffentlichen Installationsprogramm 1.0 wurde dokumentiert.
- Latent-Mplus-Einstellungsdialoge wurden an die Vorgabe ausschließlich `.studio`-basierter Einstellungsdateien angepasst.
- Der Vertriebs-/Lizenz-/Updateplan für 1.0 wurde so überarbeitet, dass historische Betaversionshinweise nicht als aktueller Release-Ausgangspunkt erscheinen.
- Eine manuelle QA-Freigabebedingung verbietet öffentliche Versionshinweise und Oberflächentexte, die nicht implementierte eingeschränkte Editionen, Lizenzaktivierung, In-App-Updates oder öffentliche Installationsinfrastruktur behaupten.
- Platzhaltermitteilungen des Latent-Mplus-Builders wurden durch ausdrückliche Meldungen über nicht aktivierte Release-Funktionen und entsprechende Validierung ersetzt.
- Der Ersatzuntertitel für Effektgrößen wurde so umformuliert, dass nicht verfügbare Rechner keine künftigen Funktionsversprechen darstellen.
- Eine ungenutzte Hilfsfunktion für eine Analyse-Platzhalterregisterkarte wurde aus der Menüerstellung entfernt.
- Die Release-Checkliste stellt klar, dass ältere Einstellungsformate nicht in öffentlichen Einstellungsdialogen erscheinen, interne Kompatibilitätskennungen aber dokumentiert bleiben.

## v0.9.41 - 2026-06-20

### Geändert

- Die Menüs Analyse, Stichprobengröße und Effektgröße wurden in einheitliche statistische Kategorien der ersten Ebene gegliedert.
- Die Variablenübertragung in Kreuztabellen wurde korrigiert, sodass ausgewählte Spalten- oder Zeilenvariablen zuverlässig in die Liste verfügbarer Variablen zurückkehren.
- Die Übertragungsschaltflächen der Spalten- und Zeilenzielbereiche in Kreuztabellen wurden neu positioniert.


## v0.9.40 - 2026-06-20

### Geändert

- Entwicklungsmetadaten nach der Beta-Veröffentlichung 0.9.39 erhöht.
- Produktbezogene Oberflächen wurden in **StatEdu Studio** umbenannt, darunter App-Kopfzeile, Informationstexte, Startprogrammnamen, Installationsmetadaten, Favicon, Logos und Standardexportdateinamen.
- Release-Prüfungen zur Markenkompatibilität dokumentieren, welche alten Kennungen aus DOI-, Umgebungs-, Pfad- oder rückwärtskompatiblen Datenzugriffsgründen erhalten bleiben.
- Shiny-Eingabeaufrufe beim Clientstart werden abgesichert, um vorzeitige Fehler von `Shiny.setInputValue` vor Bereitschaft der Clientbindung zu vermeiden.
- Die Electron-Paketprüfungs-Sperrmetadaten wurden aktualisiert, sodass die transitive Entwicklungsabhängigkeit `undici` ohne npm-audit-Befunde aufgelöst wird.
- Electron-Paketautormetadaten hinzugefügt; Release-Artefakte `StatEdu_Studio_*.zip` werden bei lokaler Entwicklung und Electron-Staging ignoriert.

## v0.9.39 - 2026-06-18

### Hinzugefügt

- Ein eigener Ablauf `Analysis > Longitudinal / Panel Models` für GEE, LMM, GLMM sowie Panelmodelle mit festen und zufälligen Effekten hinzugefügt.
- Modellspezifische Voraussetzungsprüfungen, empfohlene Alternativen, automatische Sensitivitätsvergleiche, publikationsreife Schätzungen, Manuskripttexte, SCI-Berichtscheckliste und Softwareversionsangaben für Längsschnitt-/Panelergebnisse hinzugefügt.
- Eine Fehlwertregisterkarte für Längsschnitt-/Panelmodelle mit primärer Fehlwertbehandlung, tatsächlichen MI/IPW/WGEE-Sensitivitätsverfahren und Dokumentation der Fehlwertmethode im Bericht hinzugefügt.
- Gewichte für Längsschnittanalysen mit einem Ziel für die Gewichtsvariable, Stichproben-/Längsschnitt-/IPW-/kombinierten Gewichtstypen, Trimmung, normierten Endgewichten und effektiver Stichprobengröße hinzugefügt.
- Optionale Expositions-/Offset-Behandlung für Längsschnitt-Zähl-/Ratenmodelle einschließlich `log(exposure)`-Offsets in Haupt- und Sensitivitätsanpassungen hinzugefügt.
- Details zur Prüfung auf Nullinflation in Zählmodellen mit Vergleich beobachteter und unter Poisson erwarteter Nullanteile hinzugefügt.
- Ein aktiver Ablauf `Analysis > GLM` für gaußsche, binär-logistische, Gamma- und Zähl-GLMs wurde ergänzt; die GEE-Prüfung von Zählverteilungen dient zur Auswahl zwischen Poisson und negativer Binomialverteilung.
- SCI-orientierte GLM-Berichtsdetails für vollständige Fälle, Poisson-/Negativ-Binomial-Auswahl, logistische EPV-/Separations-/dünn besetzte Zellen-Prüfung, unabhängige Beobachtungen, Einflussdiagnostik, Publikationstabellennoten, Berichtschecklisten und Manuskriptvorschläge hinzugefügt.
- GLM-Optionen mit Registerkarten hinzugefügt, darunter Fehlwerte mit vollständigen Fällen, multipler Imputation und inverser Wahrscheinlichkeitsgewichtung.
- GLM-Dokumentation im koreanischen Benutzerhandbuch, in Analysemethoden und Methodenhinweisen ergänzt: Verteilungs-/Linkauswahl, Fehlwertsensitivität, robuste Standardfehler, Überdispersion und SCI-Berichtsanforderungen.
- HTML-, PDF-, Excel- und gespeicherte Ergebnissammlungen für GLM-Ausgaben einschließlich Publikationshinweisen, SCI-Checklisten, Manuskripttexten und Softwareversionsblättern hinzugefügt.
- HTML-, PDF-, Excel- und gespeicherte Ergebnissammlungen für Längsschnitt-/Panelmodellausgaben hinzugefügt.
- Validierung für Längsschnitt-/Panel- und GLM-Anpassungen, Einrichtungsoberflächen, Voraussetzungsprüfkataloge, HTML-/Excel-Export, Sensitivitätsvergleiche und SCI-Berichtsabschnitte hinzugefügt.

### Geändert

- Die Einrichtung der Längsschnitt-/Panelmodelle wurde an das Übertragungslayout von t-Test / ANOVA angepasst und zeigt nur modellrelevante Optionen.
- Modell- und Termoptionen für Längsschnitt-/Panelmodelle wurden in einer Registerkarte für Modelltyp, feste Zeitterme und Zufallseffektterme zusammengeführt.
- Die standardmäßige GEE-Arbeitskorrelation ist nun austauschbar; AR(1)-Anpassungen übergeben die Subjekt-/Zeitreihenfolge an `geepack::geeglm`.
- Negativ-binomialverteilte GEE-Zählanpassungen werden als marginale Negativ-Binomial-GLM mit subjektclusterrobusten Standardfehlern bezeichnet, da geepack keine native Negativ-Binomial-GEE bereitstellt.
- Klargestellt, dass die optionale Cluster-ID für LMM/GLMM eine zusätzliche Gruppierungsvariable des zufälligen Achsenabschnitts ist und nicht auf die primäre GEE-/Panelanpassung angewendet wird.
- Die Fehlwertbehandlung in LMM/GLMM wurde als likelihoodbasierte MAR-Analyse verfügbarer Messwiederholungen beschrieben; MI/IPW bleiben Sensitivitätsanalysen statt primärer Standardanpassung.
- Nullinflations- und Hürdenzählmodelle bleiben außerhalb des standardmäßigen Längsschnitt-/Panelmoduls, um umfangreiche optionale Paketabhängigkeiten zu vermeiden; überzählige Nullen werden als Prüfhinweise berichtet.
- Das alte Gerüst für verallgemeinerte Modelle wurde durch funktionierende GLM-Einrichtung, Ausführung, Koeffiziententabelle, Anpassungsstatistiken, robuste Standardfehler, Überdispersionsprüfung und optionale VIF-Diagnostik ersetzt.
- `run_app.R` wurde an `R/app_bootstrap.R` angepasst, sodass die Paketinstallation beim Start dieselbe Liste erforderlicher Pakete wie die Anwendungslaufzeit nutzt.
- Die Electron-Laufzeit wurde auf 39.8.6 festgelegt und die Sperrdatei nach der Paketprüfung aktualisiert.
- Das Electron-Betabuildskript findet Rscript auch außerhalb von PATH und scheitert nicht am optionalen, aus dem App-Paket ausgeschlossenen Latent-Mplus-Modul.
- README, koreanisches Benutzerhandbuch, Analysemethoden und Methodenhinweise für die neuen Längsschnitt-/Panel- und GLM-Abläufe aktualisiert.

## v0.9.38 - 2026-06-15

### Hinzugefügt

- Empirische Stichprobengrößenschätzungen für Mediation nach Fritz & MacKinnon (2007) für eine Teststärke von .80 hinzugefügt.
- Methodenspezifische Mediationsreferenzen für Stichprobengrößenberechnungen nach Fritz & MacKinnon, Monte Carlo, Bootstrap und Sobel hinzugefügt.

### Geändert

- Der Ergebnisbereich für Stichprobengrößen wurde bei großen Fenstern verbreitert; das bestehende dreiteilige Layout bleibt bei 1280px breiten Anzeigen erhalten.

## v0.9.37 - 2026-06-12

### Hinzugefügt

- Koreanische und englische Erkennungswörterbücher für vierstufige Likert-Skalen entsprechend den vorhandenen fünfstufigen Wörterbüchern hinzugefügt.
- Eine animierte Aktionsüberlagerungsführung mit eingebundenen Handbuchbildern wurde in das koreanische In-App-Benutzerhandbuch aufgenommen.

### Geändert

- Die Oberfläche benutzerdefinierter Likert-Erkennungswörterbücher wurde verbessert: registrierte Wörterbücher öffnen über eine Schaltfläche, erscheinen in einer Liste und zeigen Details im Seitenbereich.
- Exakte Likert-Stufenübereinstimmungen werden vor kompatiblen Obermengen bevorzugt, damit vierstufige Antworten als vierstufige Skalen erkannt werden.
- Zeitablauf und Positionen der Handbuchüberlagerung für Datenladen, t-Test / ANOVA und Ergebnisprüfung wurden angepasst.

### Behoben

- Die Scrollposition bleibt beim Öffnen oder Auswählen registrierter Likert-Erkennungswörterbücher erhalten.
- Spaltenbreiten für Auswahl und Erkennungsname in der Likert-Erkennungstabelle verbessert.


## v0.9.36 - 2026-06-11

### Hinzugefügt

- Ein bearbeitbarer Manager für benutzerdefinierte Likert-Erkennungswörterbücher mit Übersicht, Detailanzeige, Bearbeitung und Löschung registrierter Wörterbücher hinzugefügt.
- Die Validierung für logistische Analysen, Umkodierungen im Dateneditor, Faktorenanalyse/PCA, Korrelation, gepaarte Tests, Datenein-/ausgabe und Ergebnisverlauf erweitert.

### Geändert

- Die B5-orientierte Darstellung von Ergebnistabellen für logistische Regression, Faktorenanalyse, PCA, Reliabilität, gepaarte/Messwiederholungstests, Korrelation und gespeicherte Ergebnisse verbessert.
- Die Excel-Importprüfung wurde vereinfacht; die Arbeitsblattvorschau wurde in den Hauptprüfbereich verlegt und Importsteuerelemente vereinfacht.
- Die Likert-Konvertierung wurde angepasst, sodass konvertierte Variablen das gewünschte Messniveau nach der Konvertierung erhalten.

### Behoben

- Fehler bei der Bewegung von Auswahlschaltflächen in Variablenlisten des Dateneditors nach Excel-Import behoben.
- Automatische Fehlwerterkennung und Aktualisierung der Likert-Erkennung nach Konvertierung/Import korrigiert.
- Tabellenplatzierung, Referenzzellen, VIF-Anzeige und Konfidenzintervalloptionen der hierarchischen logistischen Regression korrigiert.
- Die Spaltenreihenfolge der Ladungen und kompakte Tabellenüberschriften für B5-Hochformat bei Faktorenanalyse/PCA korrigiert.

## v0.9.35 - 2026-06-10

### Hinzugefügt

- Den Entwicklerablauf Latent Mplus als optionales EasyFlow-Modul mit Daten-, Einrichtungs- und Ergebnisschritten hinzugefügt.
- Speichern/Laden latenter Rollen, Teilmengenbedingungen, Beibehaltung der Auswahlreihenfolge und Prüfung von Fortschrittsmeldungen im Ergebnisbereich hinzugefügt.
- Latente Ausgaben werden unter dem Ordner der geladenen Datendatei abgelegt, einschließlich Ergebnisse, temporärer Mplus-Dateien, Laufprotokolle, Excel-Tabellen und 600-dpi-Abbildungen.
- Anzeige ausgewählter nativer Mplus-Diagramme und farbige Varianten von Indikatorprofilabbildungen hinzugefügt.

### Geändert

- Latente Ergebnistabellen und Abbildungen wurden an einen B5-orientierten Rahmen mit linksbündiger Ausgabe, kompakten Tabellen und Abbildungsskalierung für B5-Hochformat angepasst.
- Zeitabläufe für Laden/Einstellungen/Zurücksetzen im Datenregister verbessert; die Registrierung des latenten Servers wird bis zum Öffnen einer latenten Registerkarte aufgeschoben.
- Nur für LCA bestimmte Ergebnistabellen werden bei LPA ausgeblendet; interne BCH-Klassenkennungstabellen wurden aus der Ergebnisanzeige entfernt.
- Die Electron-Betapaketierung schließt das ausschließlich für Entwickler bestimmte Latent-Mplus-Modul aus dem öffentlichen Beta-Staging aus.

### Behoben

- Voraussetzungsprüfung und Modellübersichtstabellen für t-Test / ANOVA wurden korrigiert, sodass Ergebnisse der Voraussetzungsprüfungen erscheinen.
- Latente Rollen/Ergebnisse werden beim Laden einer neuen Datendatei zurückgesetzt; ausdrücklich wiederhergestellte YAML-Einstellungen bleiben erhalten.
- Die Scrollposition der latenten Variablentabelle bleibt bei Rollenzuweisungen erhalten.
- Fortschrittsmeldungen latenter Analysen wurden während der Berechnung aus der Einrichtung in die Ergebnisse verlegt.

## v0.9.34 - 2026-06-09

### Geändert

- Ergebnistabellen gepaarter Tests und Messwiederholungen für Zusammenfassungsoptionen, Statistikenausrichtung, Effektgrößenbezeichnungen, Warnungen und Voraussetzungsprüfungen verbessert.
- Die Variablenreihenfolge gepaarter Messwiederholungen wurde korrigiert, sodass angezeigte Paare der Auswahlreihenfolge entsprechen.
- Optionsregister der gepaarten Einrichtung bleiben sichtbar; Messwiederholungsbeschriftungen werden erst ab drei Messungen aktiviert.
- Methodenhinweise zu Messwiederholungen stellen klar, dass Wilks' Lambda und Greenhouse-Geisser nicht als kombinierte Methode berichtet werden.
- Zitationsmetadaten auf den registrierten EasyFlow-Statistics-DOI aktualisiert.

## v0.9.33 - 2026-06-06

### Geändert

- ANCOVA-Voraussetzungsdiagnostik erweitert: Levene als Standard-Varianzprüfung, optionale Brown-Forsythe-/Breusch-Pagan-/White-Tests, Detailtabellen zur Steigungshomogenität, vollständige Fälle, Residuenlinearitätsdiagramme und Einflusssensitivitätsanalyse.
- ANCOVA-Steuerelemente zur automatischen Methodenauswahl hinzugefügt: automatische Auswahl beibehalten oder Voraussetzungswarnungen bei unverändertem Standard-ANCOVA-Modell berichten.
- Die gemeinsame Tabellendarstellung wurde verbessert: 9-pt-Schrift, feste Breiten im Hoch-/Querformat, 1,5-fache Bildschirmvorschau, gemeinsame Post-hoc-Markierungen, ES-Spaltenbezeichnungen und zweizeilige Post-hoc-Köpfe.
- ANCOVA-Optionen für Voraussetzungen / Modell / Ausgabe neu geordnet und Abstände, Einzüge sowie Ergebnishinweise abgestimmt.

## v0.9.32 - 2026-06-03

### Geändert

- ANCOVA-Ergebnisdarstellung mit strengerer Tabellenbreitensteuerung und rechtsbündigen Teststatistiken verbessert.
- Effektgrößenumrechnung nach SPSS-Art für LMM hinzugefügt: partielles Eta-Quadrat aus Omnibus-F/df und kovarianzbasiertes paarweises dz.
- GLMM-Effektgrößenumrechnung für binären Logit, Zähldaten mit Log-Link und gaußsche feste Effekte hinzugefügt.
- Koreanisches Benutzerhandbuch, Analysemethoden und Methodenhinweise für neue ANCOVA-, LMM-, GEE- und GLMM-Effektgrößenabläufe aktualisiert.
- Effektgrößeneingaben verbessert, sodass LMM-Omnibus- und paarweise Berechnungen mit jedem der verfügbaren Eingabesätze laufen können.

## v0.9.31 - 2026-06-02

### Geändert

- Speichern/Öffnen von Ergebnisverlaufsdateien mit eigener Typkennung `.efs-result` hinzugefügt.
- Gespeicherte Einstellungen in `.efs-settings` mit Typvalidierung getrennt.
- Ergebnis hinzufügen bewahrt nun die aktuell dargestellte Ergebnisaufnahme, statt die Ausgabe neu aufzubauen.
- Tabellenbreiten und Querformat-Exportregeln in Analyseausgaben, Ergebnisverlauf, HTML, PDF und Word vereinheitlicht.
- Modellübersichts- und Warntabellen für Korrelation, gepaarte Tests, Faktorenanalyse, Reliabilität und logistische Regression verbessert.
- Das Deckblatt des Word-Exports entfernt, sodass gespeicherte Ergebnisdokumente direkt mit Methoden und Ergebnissen beginnen.
- ANCOVA mit automatischer Auswahl zwischen Standard-, HC3-robusten, rangbasierten und Interaktionsmodellen sowie HTML-, PDF-, Excel- und Ergebnisverlaufsexport hinzugefügt.


## v0.9.30 - 2026-06-02

### Geändert

- Stichprobengrößen- und Effektgrößenrechner bereinigt, indem nicht auf Effektgrößen bezogene Abläufe aus den Effektgrößenmenüs entfernt wurden.
- Abbrechbare Hintergrundberechnungen der Stichprobengröße mit Fortschrittsanzeige hinzugefügt.
- LMM-Eingaben für unstrukturierte Korrelation und Schätzung der SEM/CFA-Freiheitsgrade anhand von Modellanzahlen hinzugefügt.
- Die erforderliche Stichprobengröße wird einheitlich mit ausdrücklichen `n`-Beschriftungen und einer Standardteststärke von 0.95 hervorgehoben.
- Koreanisches Benutzerhandbuch, Analysemethoden und Methodenhinweise zu Stichprobengröße, Teststärke und Effektgröße um Formeln und Referenzen erweitert.
- Koreanische Analysemethodendokumentation an die Ausgaben von 0.9.30 angepasst, einschließlich Modellübersichten für t-Test/ANOVA, gepaarte und nichtparametrische gepaarte Tests sowie Korrelation.
- Lokale MathJax-Dateien zur Offline-Formeldarstellung in Methodenhinweisen eingebunden.


## v0.9.29 - 2026-06-01

### Hinzugefügt

- Eigene Hauptmenüs Stichprobengröße und Effektgröße nach Analyse hinzugefügt.
- Literaturgestützte Rechner für Stichprobengröße, Teststärke und Effektgröße für t-Test, ANOVA / ANCOVA, GEE, LMM, nichtparametrische Verfahren, Anteile, Chi-Quadrat, McNemar, Regression, Überleben und weitere Planungsabläufe hinzugefügt.
- Gezielte Validierung für Berechnungsschnittstellen zu Stichprobengröße, erreichter Teststärke und Effektgröße hinzugefügt.

### Geändert

- Stichprobengrößen- und Effektgrößenseiten wurden auf den gemeinsamen dreiteiligen Ablauf der Analyseeinrichtung umgestellt.
- Die Menüfolge für Stichprobengröße und Effektgröße wurde nach Studiendesignfamilien geordnet.
- Die t-Test-Effektgrößenausgabe hebt die gewählte Methode hervor und zeigt umrechenbare Effektgrößen ohne Zwischenwerte, die keine Effektgrößen sind.

## v0.9.28 - 2026-05-30

### Behoben

- Windows-Öffnungsdialoge für Daten/Einstellungen erscheinen durch ein oberstes WinForms-Eigentümerfenster statt der nativen R-Dateiauswahl im Vordergrund.
- Ladungstabellen von Faktorenanalyse und PCA wurden auf Bildschirm sowie in HTML/PDF und Excel unmittelbar nach den Übersichtstabellen platziert.
- Informationen > Open-Source-Lizenzen korrigiert: erzeugte Drittanbieterhinweise werden aus dem eingebundenen App-Pfad gelesen; Lizenzmetadaten sind nach direkten EFS-Paketen, eingebundenen Abhängigkeiten, R-Basis-/empfohlenen Paketen und R-Laufzeit gruppiert.
- Die Portbereinigung im Windows-Startprogramm wurde durch `netstat` / `taskkill` ersetzt, um Hänger beim Schließen eines vorhandenen App-Prozesses auf Port 7894 zu vermeiden.


## v0.9.27 - 2026-05-29

### Geändert

- Sichtbare Standardexportdateinamen wurden für Ergebnisse, Daten, Einstellungen und erzeugte Exporte von `EasyFlow_Statistics_...` auf das Präfix `EFS_...` verkürzt.

### Hinzugefügt

- Informationen > Versionsverlauf hinzugefügt, damit die eingebundenen Versionshinweise in der Desktop-App gelesen werden können.


## v0.9.26 - 2026-05-29

### Hinzugefügt

- Zweistufigen Excel-Import mit Arbeitsblattauswahl, Startzelle im A1-Stil, Kopfzeilensteuerung und Vorschau vor dem Laden hinzugefügt.
- Ausgewählte Excel-Importoptionen werden in gespeicherten Einstellungen für erneut geöffnete Excel-Dateien erhalten.


## v0.9.25 - 2026-05-29

### Behoben

- Ergebnis hinzufügen / Word-Export der Regression wurden durch Erhaltung kategorialer Referenzzeilen, Wertelabels und einzeiliger Modellanpassungsübersicht an die Bildschirm-Koeffiziententabelle angepasst.
- Breite Regressionskoeffiziententabellen verwenden einen Word-Querformatabschnitt, damit gespeicherte Word-Ausgaben besser der angezeigten Tabelle entsprechen.


## v0.9.24 - 2026-05-29

### Behoben

- Die Darstellung von t-Test-/ANOVA- und nichtparametrischen Tabellen wurde korrigiert, sodass p-Werte wie `.008` und Effektgrößen wie `.022` drei Dezimalstellen behalten, wenn die Fußnotenmarkierung dieselbe Endziffer hat.


## v0.9.23 - 2026-05-29

### Behoben

- Die PowerShell-Dateiauswahl des Desktops wurde durch den nativen Windows-Dialog `choose.files()` von R ersetzt, damit Datendatei öffnen aus der installierten Electron-App zuverlässig erscheint.


## v0.9.22 - 2026-05-29

### Behoben

- Die Desktop-Dateiauswahl nutzt vor dem Tcl/Tk-Rückfall einen nativen Windows-Dialog, um hinter Electron versteckte oder ausbleibende Dialoge zu reduzieren.
- Excel-, SAS-, Stata-, CSV-, DAT- und SPSS-Filter bleiben in der Datendateiauswahl sichtbar.


## v0.9.21 - 2026-05-29

### Hinzugefügt

- Datenimport für ältere Excel-Dateien `.xls`, SAS `.sas7bdat` / `.xpt` und Stata `.dta` hinzugefügt.
- Datendateiauswahl, Texte des Datenregisters und Ein-/Ausgabevalidierung für die erweiterten Importformate aktualisiert.


## v0.9.20 - 2026-05-29

### Geändert

- Die anfängliche Shiny-Seitengröße wurde reduziert, indem die Inhalte von Dateneditor, Rechner, Analyse und Informationen erst beim Öffnen dieser Registerkarten gerendert werden.

## v0.9.19 - 2026-05-29

### Geändert

- Die Startzeit der installierten Desktop-App wurde durch anfängliches Einbinden nur von Shiny und DT verkürzt.
- Redundante Paketprüfungen der eingebundenen Laufzeit beim Electron-Start übersprungen; die Paketverfügbarkeit bleibt durch Build- und Release-Kurztests abgedeckt.
- Das Shiny-Bereitschaftsprüfintervall von Electron verkürzt und separate Ladezeitdiagnostik für BrowserWindow ergänzt.

## v0.9.18 - 2026-05-29

### Hinzugefügt

- Die standardmäßige Speichersteuerungszeile mit fünf Positionen zu Ergebnissen der logistischen Regression hinzugefügt.
- HTML-, PDF-, Excel- und gespeicherten Ergebnissammlungsexport für logistische Regression hinzugefügt.

## v0.9.17 - 2026-05-29

### Behoben

- Korrelation > Erweiterte Korrelationen geändert, sodass latente Variablenkorrelationen die primäre Methodenauswahl ersetzen statt eine separate doppelte Ergebnismenge zu erzeugen.
- Geeignete kontinuierlich-ordinale/binäre Paare zeigen bei aktivierten latenten Korrelationen Polyserial direkt in der Hauptmethodentabelle.

## v0.9.16 - 2026-05-29

### Behoben

- Umbrüche von p-Wert- und Effektgrößenfußnoten in t-Test-/ANOVA- und eigenständigen nichtparametrischen Ergebnistabellen korrigiert.

## v0.9.15 - 2026-05-29

### Behoben

- Die Gestaltung eingebetteter Fußnotenmarkierungen in t-Test / ANOVA korrigiert, sodass Effektgrößen ausgerichtet bleiben und führende Nullen weiterhin unterdrückt werden.

## v0.9.14 - 2026-05-29

### Geändert

- GPL-Anwendungslizenzierung, Quelltextangebot und Informationsseiten für eingebundene Desktop-Quelltext-/Lizenzhinweise hinzugefügt.
- Erzeugte OSS-Hinweise, Lizenzbericht, Sammlung eingebundener Lizenztexte und Release-Kurztests für Electron/R-Installationsbuilds hinzugefügt.
- Berichte zur Bereinigung der eingebundenen R-Laufzeit und exakte Versionsfestlegungen für Electron/electron-builder hinzugefügt.
- Den Startaufwand der installierten Desktop-App reduziert und Startzeitdiagnostik ergänzt.
- Die Shiny-Startsitzungs-Schließsperre entfernt, die die installierte Desktop-App auf einem deaktivierten grauen Bildschirm belassen konnte.
- Das Windows-Beta-Installationsprogramm für Version 0.9.14 neu gebaut.

## v0.9.13 - 2026-05-28

### Geändert

- Unterstützten Word-Ergebnisexport aktiviert und Exportregeln für Word, PDF und Excel vereinheitlicht.
- Publikationsorientierte Word-Ausgabe mit Deckblatt, Methodenseiten, Auswahl nur der Haupttabellen, Tabellennoten, hochgestellten Markierungen und B5-Hochformat als Standard ergänzt; nur breite Tabellen erhalten Querformat.
- Tabellenbreiten, Köpfe, Post-hoc-Spalten und Fußstatistiken gepaarter Tests, Messwiederholungen, t-Test/ANOVA, Korrelation, Regression, hierarchischer und logistischer Regression für PDF/Word verbessert.
- Excel-Export verbessert, sodass Titel, zweistufige Köpfe, Rahmen, zusammengeführte Notizen und feste Spaltenbreiten die dargestellte Tabellenstruktur bewahren.
- Das Electron-Startfenster vergrößert und die Ausrichtung der Aktionszeile der Regression nach Ergebnisdarstellung stabilisiert.
- Das Windows-Beta-Installationsprogramm für Version 0.9.13 neu gebaut.

## v0.9.12 - 2026-05-27

### Geändert

- PDF- und Publikationslayouts für gepaarte Tests, gepaarte Messwiederholungen, nichtparametrische gepaarte Tests, Faktorenanalyse, PCA, logistische Regression und t-Test / ANOVA verbessert.
- Tabellenabstände, Ausrichtung der Modellübersicht und Beta-Wasserzeichengröße in exportierten Berichten verbessert.
- Ergebnistexte gestrafft, sodass kompakte Übersichten Methoden, N, Voraussetzungen und kurze Entscheidungsnotizen hervorheben.
- PDF-Export auf A4 und Word-Export auf B5 vereinheitlicht; Tabellen passen in die druckbare Seitenbreite und behalten die dargestellten Ausrichtungsregeln.
- Querformatanpassung von PDF-Tabellen auf Effektgrößenspalten gepaarter Messwiederholungstests und hierarchische Regressionskoeffiziententabellen erweitert.
- Excel-Tabellenexportregeln vereinheitlicht, sodass zweistufige Köpfe, Titelzeilen, Tabellenlinien, feste Spaltenbreiten und zusammengeführte Notizzeilen dem dargestellten Layout entsprechen.
- Word-Exportschaltfläche im Ergebnisregister für Editionen mit Ergebnisexport aktiviert.
- Word-Schriftgestaltung von Tabellenköpfen/-körpern angepasst, sichtbaren Speicherschaltflächenstil aktiviert, Mittelwerte/Standardabweichungen gepaarter Übersichten mit zwei Dezimalstellen dargestellt und gemischte Übersichts-/Voraussetzungs-/Diagnosetabellen gepaarter Tests zusammengeführt.
- Zweistufige Tabellenköpfe im Word-Export erhalten, Berichtslogos aus dem Word-Hauptteil entfernt, Querformatabschnitte für breite Messwiederholungs-/Hierarchietabellen hinzugefügt und PDF-Ränder/-Spaltenbreiten gepaarter, Messwiederholungs- und hierarchischer Tabellen verkleinert.
- Modellübersichten für t-Test / ANOVA zeigen N, Analysemethode und Begründung als direkte Spalten; Regressionsübersichten mehrerer Modelle verwenden zweistufige Köpfe für abhängige Variable und Modell.
- PDF-Tabellen gepaarter Messwiederholungen auf Querformatseitenbreite gesetzt, Linien für Köpfe erster Ebene in Hierarchietabellen ergänzt und Word-Querformatabschnitte begrenzt, sodass das Dokument standardmäßig B5-Hochformat behält.
- Post-hoc- und Toleranzspalten verbreitert, PDF-Deckblätter vergrößert, Effektgrößen gepaarter Tests unter zweistufigen Köpfen gruppiert, hochgestellte Word-Notizmarkierungen und Tabellennoten wiederhergestellt und wiederholte Regressionsfußzeilen in Word zusammengeführt.
- Word-Tabellennotenschrift verkleinert, sämtliche dargestellten Tabellennoten übernommen, hierarchische Modellkopfmarkierungen hochgestellt und hierarchische Fußstatistiken einmal je Modell zusammengeführt.
- Nur Block 1 umfassende Läufe aus der hierarchischen Regression werden beim Hinzufügen zur Ergebnissammlung als gewöhnliche Regression bezeichnet.
- Word-Ergebnisexport auf publikationsreife Haupttabellen beschränkt: jede Tabelle beginnt auf einer eigenen Seite, Abbildungen behalten ihre dargestellte Größe ohne Hochskalieren, Querformat gilt nur für breite gepaarte/hierarchische Tabellen und Korrelationsmatrizen ab 10 Variablen.
- Regressionsfußstatistiken in Word zentriert und eine kräftige obere Linie über F(p) ergänzt; Residuenhomoskedastizität erscheint als einzelner Fußzeileneintrag x²(p).
- Electron-Startfenster vergrößert, Word-Deckblatt und Analysemethodenseiten hinzugefügt, Regressionsabbildungen paarweise pro Seite platziert und detaillierte Post-hoc-Tabellen von t-Test/ANOVA aus dem Word-Publikationstabellenexport ausgeschlossen.
- Abstände des Word-Tabellenexports, Häufigkeits-/Deskriptivspaltenbreiten, Regressionsübersichtszeilen, Querformatübergänge und Abbildungsgrößen verbessert, um Umbrüche, übermäßigen Leerraum und Leerseiten zu reduzieren.
- Die Regressionsaktionszeile nach Ergebnisdarstellung stabilisiert, Word-Post-hoc-Tabellenfilterung verstärkt, kombinierte n(%)/M±SD- und IQR-Spalten verbreitert und breite Korrelationstabellen kompakter gestaltet.

## v0.9.11 - 2026-05-27

### Geändert

- Kompakte Modellübersichten und Voraussetzungszusammenfassungen für gepaarte Tests, t-Test / ANOVA, Regression und logistische Regression hinzugefügt.
- Detaillierte Voraussetzungsdiagnostik in eigene Prüftabellen verlegt; Modellübersichten bleiben auf N, Analysemethode und kurze Begründungen beschränkt.
- Optionsregister für Faktorenanalyse, PCA und t-Test / ANOVA mit Erhaltung des Registerzustands und verbesserten Abständen hinzugefügt.
- Die Standardoptionen für Regressionseffektgrößen zeigen f2 standardmäßig an; sr2 bleibt abgewählt.


## v0.9.10 - 2026-05-27

### Geändert

- Electron-Betapaketierung mit eingebundener R-Laufzeit, Desktop-Fensterstarter, Installationsmetadaten und EasyFlow-Symbolen hinzugefügt.
- Koreanischen CSV- und Excel-Import durch Erprobung gängiger Kodierungen und Normalisierung importierter Namen und Zeichenwerte verbessert.
- Geprüfte binäre, kategoriale und ordinale Messniveaus bleiben beim Speichern/Laden von Einstellungen erhalten, sodass Analysemenüs dieselben Variablentypen wie die Datenprüfung verwenden.
- Ergebnisspalten für Häufigkeiten / Deskriptive Statistiken auf Statistiken begrenzt, die den gewählten Variablentypen entsprechen.
- Beta-Wasserzeichen und Word-Exportplatzhalter im Ergebnisexport verbessert.


## v0.9.9 - 2026-05-27

### Geändert

- Die Umkodierung derselben Variable wurde mit einem vorgemerkten Schritt `Add` und einem abschließenden Schritt `Apply` neu gestaltet, damit Regeln vor Datenänderungen geprüft werden können.
- Bearbeitbare vorgemerkte Umkodierungsregeln mit Zeilenauswahl, Löschfunktionen, automatischen Variablentypvorgaben und automatischer Ableitung des Ausgabemessniveaus hinzugefügt.
- Einzelwertumkodierung für beobachtete Kategorien, Fehlwertmarkierungen, Hinweise auf nicht übereinstimmende Werte und Umkodierung nach oder von `NA` hinzugefügt.
- Steuerelemente zur kategorisierenden Umkodierung, Bereichsoperatoren, Bereichsausrichtung, Aktionsschaltflächen und Abstände beim Umkodieren von Variablen verbessert.


## v0.9.8 - 2026-05-26

### Geändert

- Das Informationsmenü mit Übersicht, Benutzerhandbuch, Analysemethoden, Methodenhinweisen und Anwendungsinformationen hinzugefügt.
- Koreanische Dokumentation zu Benutzerführung, implementierten Analysemethoden, Methodenhinweisen, Paket-/Laufzeitübersicht, Kriterien und Referenzen erweitert.
- Formulierungen zur Residuenhomoskedastizität in Regressionen und Methodenbezeichnungen für Kreuztabellentrends präzisiert.
- 20.000 als dokumentierte Bootstrap-Wiederholungsoption hinzugefügt; 50.000 bleibt empfohlen.
- Die Benennung in der Dokumentation vereinheitlicht, sodass **EasyFlow Statistics** stets ausgeschrieben und einheitlich hervorgehoben wird.

## v0.9.7 - 2026-05-26

### Geändert

- Automatische Korrelationsmethodenauswahl hinzugefügt: Pearson für normalverteilte kontinuierliche Paare, Spearman für nicht normalverteilte oder ordinale Paare.
- Schutzbedingungen hinzugefügt, damit Korrelation, gepaarte Tests, t-Test / ANOVA, Regression und logistische Regression ungültige Variablen/Modelle überspringen statt die gesamte Analyse abzubrechen.
- Warnungen und Ausgaben zu übersprungenen Ergebnissen für geringe Stichprobengröße, Nullvarianz, ausschließlich Bindungen, dünn besetzte Zellen, Separationsrisiko, Rangdefizienz und VIF-Grenzen hinzugefügt.
- Pearson-/polychorische Matrixoptionen für Faktorenanalyse und PCA mit Hinweisen für ordinale Daten und Stichprobengrößenwarnungen hinzugefügt.
- Hilfsfunktionen für Warnungen und übersprungene Ergebnisse in Ergebnisansichten und Excel-Exporten zusammengeführt.


## v0.9.6 - 2026-05-25

### Geändert

- Ein eigenständiges Menü für nichtparametrische gepaarte Tests mit Wilcoxon-Vorzeichen-Rang- und Friedman-Tests hinzugefügt.
- Bonferroni- und Holm-Bonferroni-Optionen für gepaarte Post-hoc-Tests hinzugefügt; Bonferroni ist Standard in gepaarten Menüs.
- Median-, Q1~Q3-Ausgabe und Wilcoxon-Effektgrößenhinweise für nichtparametrische gepaarte Ergebnisse hinzugefügt.
- Tabellenköpfe, Fußnotenmarkierungen, Exporte und Aktionsschaltflächen gepaarter und nichtparametrischer gepaarter Tests abgestimmt.


## v0.9.5 - 2026-05-25

### Geändert

- Ein eigenständiges Analysemenü für nichtparametrische Tests mit Mann-Whitney U und Kruskal-Wallis hinzugefügt.
- Median- und Quartilzusammenfassungen für eigenständige nichtparametrische Tests hinzugefügt.
- Cliff's Delta als Effektgröße für Mann-Whitney-U-Ergebnisse hinzugefügt.
- Kompakte Post-hoc-Buchstaben korrigiert, sodass gemeinsam nicht signifikante Gruppen kombinierte Buchstaben erhalten.
- p-Wert- und Effektgrößenfußnotenmarkierungen erscheinen für stabile Ausrichtung in schmalen Nachbarspalten.
- Abstände der Optionsbereiche für nichtparametrische und gepaarte Tests verbessert.


## v0.9.4 - 2026-05-25

### Geändert

- Nur für Entwickler bestimmte HTML/PDF-Wasserzeichen mit horizontaler EasyFlow- und StatEdu-Marke verbessert.
- Abbildungsexport für Streudiagrammmatrizen und Korrelationsheatmaps aktiviert.


## v0.9.3 - 2026-05-25

### Geändert

- Die PCA-Ladungstabelle an die Faktorenanalyse angepasst: h², Komplexität, Eigenwert, Varianz, kumulierte Varianz und KMO-/Bartlett-Diagnosezeilen; Reliabilitätsspalten werden ausgelassen.
- PCA-Einrichtung für Matrixwahl, Komponentenauswahl nach kumulierter Varianz und ausgerichtete Zahlenfelder zur Komponentenauswahl verbessert.
- Die Diagnosetabelle der Faktorenanalyse gestaltet und die Platzierung der KMO-/Bartlett-Zusammenfassungszeile abgeschlossen.
- Alle fünf Analysespeicherfunktionen bleiben in Entwicklerbuilds sichtbar; PDF / Ergebnis hinzufügen für die übrigen Analysemodule ergänzt.
- PDF-Deckblattdekoration und interne Dateinamen-/Datumsdruckbeschriftungen entfernt; Seitennummerierung unten rechts hinzugefügt.
- Editionsabhängige Identität auf PDF-Deckblättern hinzugefügt, einschließlich StatEdu-Logo und Name StatEdu Statistical Research Institute für Entwicklerbuilds.
- PDF-Ausgabedatum unter dem Speicherdatum auf dem Berichtsdeckblatt hinzugefügt.
- Nur in der Entwicklung verwendete Wasserzeichen für exportierte HTML- und PDF-Berichte hinzugefügt.


## v0.9.1 - 2026-05-24

### Geändert

- Explorative Faktorenanalyse mit sortierten Ladungsmatrizen, optionaler Filterung kleiner Ladungen, Hervorhebung problematischer Werte, Kommunalitäten, Komplexität, Eigenwert-/Varianzübersichten und schiefwinkligen Strukturmatrizen verbessert.
- Optionale Reliabilitätsübersichten für Teilfaktoren direkt neben der Faktorladungsmatrix hinzugefügt.
- Faktorenanalysediagnostik für normalitätsabhängige Extraktionswahl, hohe feste Faktoranzahlen, fehlende/unendliche Werte und Reliabilitätsprobleme einzelner Items verbessert.
- Den Optionsbereich der Faktorenanalyse verdichtet, sodass alle Optionen in den standardmäßigen dreispaltigen Einrichtungsblock passen.


## v0.9.0 - 2026-05-24

### Geändert

- Sammlung im Ergebnisregister hinzugefügt, sodass unterstützte Analyseausgaben über Ergebnis hinzufügen in Reihenfolge gesammelt werden können.
- Export der Ergebnissammlung nach HTML, PDF, Excel und Word hinzugefügt.
- Faktorenanalyse und PCA bleiben von Ergebnis hinzufügen ausgeschlossen, bis ihr endgültiges Ergebnistabellenformat feststeht.


## v0.8.12

### Geändert

- Explorative Faktorenanalyse mit Hauptachsen- und Maximum-Likelihood-Extraktion, Varimax-/Oblimin-Rotation, Eigenwert- oder fester Faktorwahl, normalitätsabhängiger Methodenwahl, KMO-/Bartlett-Diagnostik, Scree-Diagrammen und Export hinzugefügt.
- PCA mit Korrelations-/Kovarianzmatrix, Komponentenauswahl nach Eigenwert, fester Anzahl oder kumulierter Varianz, optionaler Rotation, Scree-/Komponentendiagrammen, Diagnostik und Export hinzugefügt.
- Validierung für Berechnungen und Exporte von Faktorenanalyse und PCA hinzugefügt.


## v0.8.11

### Geändert

- Die Navigationsmarke verwendet wieder das horizontale EasyFlow-Statistics-Logobild statt einer Kombination aus Symbol und HTML-Text.

## v0.8.10

### Geändert

- Den Kontrast der Navigationsmarke korrigiert, sodass EasyFlow-Statistics-Logotext und Version auf dem hellen Kopfbereich sichtbar bleiben.
- Geordnete Post-hoc-Signifikanznotation vereinheitlicht, sodass gemeinsame Vergleichsmuster einschließlich `3, 2>1` und `3>2, 1` konsistent erscheinen.
- Variablenübertragung in t-Test / ANOVA beim Zurückverschieben aus abhängigen/unabhängigen Listen verbessert.
- Automatische Messniveauableitung verbessert: numerische Variablen mit Dezimalwerten werden nicht allein wegen weniger eindeutiger Werte als kategorial klassifiziert.
- Die Bereinigung des Startprogramms vor einer neuen EasyFlow-Statistics-Sitzung auf den App-Port begrenzt.
- Automatische Fehlwerterkennung mit geprüfter Umwandlung in `NA` hinzugefügt.
- Formelbasierte Variablentransformation zur Erzeugung neuer Variablen aus numerischen, Text-, Statistik-, Datums- und bedingten Ausdrücken hinzugefügt.
- Dateneditorbefehle neu geordnet und Umkodieren in einen Ablauf mit gleicher oder neuer Zielvariable zusammengeführt.

## v0.8.7

### Geändert

- Automatische Likert-Texterkennung und Stapelkonvertierung importierter Befragungsdaten hinzugefügt.
- Gruppierte Likert-Prüfsteuerelemente für Itemtext, ursprüngliche Beschriftungen, Zahlenwerte, Umpolung und Variablentyp nach Konvertierung hinzugefügt.
- Teilweise beobachtete Likert-Stufen besser behandelt, sodass Items mit fehlenden Antwortstufen an der vollständig erkannten Skala ausgerichtet bleiben.
- Kompakte Statistikspalten der hierarchischen Regression für bessere Übersicht verschmälert.


## v0.8.6

### Geändert

- Variablenprüfung in Schritt 3 mit Ansichten für Beschriftungen / Variablen und einem gemeinsamen Anwenden-Ablauf für Werte-/Variablenbeschriftungen und Messniveauänderungen hinzugefügt.
- Messniveauänderungen in Schritt 3 werden nach Anwenden an Analysemenüs weitergegeben.
- Prüfsteuerelemente von Schritt 3 an das aktuelle Datenablauflayout angepasst.


## v0.8.4

### Geändert

- Tabellenlayouts für Häufigkeiten, t-Test-/ANOVA-Notizen, Korrelations-p-Werte/-KI, Reliabilitäts-Itemanalyse, Durbin-Watson-Abstände, hierarchische Methodenhinweise und instabile logistische Regression verbessert.
- Stapelbearbeitung des Messniveaus für markierte Variablen auf der aktuellen Datenseite in Schritt 2 hinzugefügt.
- Quellmessniveaus bleiben erhalten, wenn automatische Umpolung neue Variablen erzeugt oder vorhandene überschreibt.


## v0.8.3

### Geändert

- Dateneditorabläufe für Kodierungsfehlerprüfung, automatische Umpolung, Umkodierung in andere Variablen und zeilenweise Variablenberechnung hinzugefügt.
- Funktionen zum Anwenden von Korrekturen, Vorschau erzeugter Variablen, Datenspeicherung nach Variablenerstellung und Validierung für Umkodierung und Lesen kopierter CSV-/DAT-Dateien hinzugefügt.
- Einrichtungs-/Ergebnislayouts in Dateneditor, Rechner und Analyse vereinheitlicht, einschließlich gemeinsamer Schaltflächenpositionen und Ersatzverhalten der Ansicht ausgewählter Daten.
- Standardanalyseoptionen und nichtparametrische Post-hoc-Steuerungen aktualisiert, einschließlich Standardverhalten des ordinalen Alpha in Reliabilitätsanalysen und Abständen in t-Test / ANOVA.
- Cloud-synchronisierte Datendateien werden vor dem Import von SAV, CSV und DAT in einen temporären Lesepfad kopiert.
- Mehrfachauswahl in Übertragungslisten mit Ctrl / Shift / Ctrl+A wiederhergestellt; stabile Shiny-Eingabesynchronisierung bleibt erhalten.


## v0.8.2

### Geändert

- Häufig verwendete Optionen in gepaarten Tests, Häufigkeiten, Korrelation, Reliabilität, logistischer Regression und t-Test / ANOVA standardmäßig aktiviert.
- Unabhängige nichtparametrische Post-hoc-Korrekturoptionen für Kruskal-Wallis-Folgevergleiche hinzugefügt: Bonferroni als Standard, Holm Bonferroni verfügbar.
- Abstände im t-Test-/ANOVA-Optionsbereich verbessert, sodass Post-hoc- und Effektgrößensteuerung in den Einrichtungsbereich passen.
- Umkodierung derselben Variable im Dateneditor hinzugefügt.

## v0.8.1

### Geändert

- Gepaarter Test (2) und Gepaarter Test (3+) wurden in einer Einrichtung zusammengeführt, die anhand der Messwiederholungsanzahl die passende Analyse aufruft.
- Hierarchische Regression wurde in Regression umbenannt und das separate Regressionsmenü entfernt; Einzelblock- und Mehrblockverhalten bleiben erhalten.
- Bootstrap-Fortschritt und Abbruchfunktionen im gemeinsamen Regressionsablauf hinzugefügt.
- Größen des Layouts gepaarter Tests und die Marke im Datenregister aktualisiert.


## v0.8.0

### Geändert

- Logistische Regression für binäre, ordinale und multinomiale abhängige Variablen mit hierarchischen Blockmodellen, OR/KI, Pseudo-R2-Optionen, VIF, Modellanpassungszeilen und Warnhinweisen hinzugefügt.
- Gemeinsame Zurücksetzen-Funktionen in Analyseeinrichtungen hinzugefügt, die nur bei Variablen im Analysezuweisungsblock aktiv sind.
- Zugriff auf ausgewählte Daten, Entfernen per Doppelklick aus Übertragungslisten und dreiteilige Rasterabstände in Analysemenüs vereinheitlicht.
- Blockbehandlung in Regression und hierarchischer Regression geändert, sodass leere Anfangsblöcke vor Modellberechnung verdichtet werden.


## v0.7.11

### Geändert

- Kreuztabelleneinrichtung geändert: Spaltenvariablen stehen über Zeilenvariablen; die Bereiche sind für die erwarteten Variablenanzahlen bemessen.
- PDF-Export für Kreuztabellen hinzugefügt und alle Speicheraktionen in Entwicklerbuilds standardmäßig aktiviert.
- Kreuztabellenlayout mit oben ausgerichteten Statistiken, zentrierten Spaltenköpfen, linksbündigen Zeilenwerten und nummerierten Effektgrößenhinweisen vereinheitlicht.
- Effektgrößen in allen Ergebnissen auf drei Dezimalstellen ohne führende Null vereinheitlicht.
- Nummerierte Hinweise zu p-Werten, Effektgrößen und Trends in t-Test / ANOVA hinzugefügt; Markierungen werden hochgestellt.
- Validierung der Notizdarstellung für t-Test / ANOVA hinzugefügt und Kreuztabellenvalidierung erweitert.


## v0.7.10

### Geändert

- Kreuztabellenanalyse für binäre, ordinale und kategoriale Variablen mit Pearson-Chi-Quadrat, exaktem Fisher-/Monte-Carlo-Ersatz und Trendanalyse hinzugefügt.
- Zuweisung mehrerer Zeilen-/Spaltenvariablen mit Sortierung, spaltengruppierten Tabellen, Zeilen-/Spalten-/Gesamtprozenten und optional getrennten n-/Prozentzellen hinzugefügt.
- Methodenfußnoten für p-Werte, p für Trend mit methodenspezifischen Notizen, Effektgrößenhinweise und HTML-/Excel-Export für Kreuztabellen hinzugefügt.
- Validierung für Kreuztabellenstatistik, Darstellung, Variablenreihenfolge und Exporthilfen hinzugefügt.


## v0.7.9

### Geändert

- Abstände der EQ-5D-, metabolisches-Syndrom- und metabolischer-Schweregrad-Rechner verdichtet und abgestimmt.
- Die Standardkriterientabelle für metabolisches Syndrom wird bei Auswahl benutzerdefinierter Kriterien ausgeblendet.
- Den Formelbereich des metabolischen Schweregrads an andere Rechnerreferenzbereiche angepasst und den Ausgabebereich getrennt.
- Rechnervalidierung für HINT8, EQ-5D, metabolisches Syndrom, FRS, ASCVD10 und metabolischen Schweregrad hinzugefügt.


## v0.7.7

### Geändert

- Anfangswerte im HINT8-Rechner werden als kompakte Item-nach-Stufe-Matrix angezeigt.
- HINT8-Einrichtungsbereiche bleiben nach dem Laden von Daten sichtbar, auch wenn keine ordinalen Variablen verfügbar sind.
- Abstände im HINT8-Anfangswertbereich verkleinert.

## v0.7.6

### Geändert

- Anfangswerte im EQ-5D-Rechner werden statt einer langen Referenzliste als kompakte Dimension-nach-Stufe-Matrix angezeigt.

## v0.7.5

### Geändert

- Beschriftungsanwendung in Datenschritt 3 korrigiert, sodass Variablen-/Wertelabels und Messniveaus mit einem Klick angewendet und in Einstellungen gespeichert werden.
- Kostenpflichtige Exportfreigaben für PDF, Excel und Ergebnis hinzufügen ergänzt; HTML und Abbildungsexport bleiben im kostenlosen Modus verfügbar.
- PDF-Berichtsexport für Regression und hierarchische Regression mit Deckblatt, gemischtem Hoch-/Querformat, skalierten breiten Tabellen und zweispaltigen Diagrammseiten hinzugefügt.
- Gespeicherte HTML-Ausgaben als Betrachter mit horizontalem Tabellenscrollen verbessert; das ursprüngliche Bildschirmlayout bleibt erhalten.
- Speicherschaltflächenlayout für Regression und hierarchische Regression vereinheitlicht; sr2, f2 und VIF standardmäßig aktiviert.
- Wiederholte Speicherdialoge nach Abbruch korrigiert und Fehler bei Aktivierung von Analysemenüregisterkarten reduziert.

## v0.7.4

### Geändert

- Die Hauptnavigation in Daten, Dateneditor, Rechner, Analyse, Ergebnisse und Informationen gegliedert.
- Gruppierungen für Dateneditor- und Analysemenüs einschließlich verschachtelter Menüs für gepaarte Tests und Regression hinzugefügt.
- Verschachtelte Menüs verbessert, sodass Analyse- und Rechneruntermenüs die normale Shiny-Registeraktivierung verwenden.
- Einrichtungsverzögerungen durch Vermeidung unnötiger Tabellenübertragungen aus Datenschritt 3 und wiederholter Variablenübersichten beim Menüwechsel reduziert.

## v0.7.3

### Geändert

- Rechnermodule für HINT8, EQ5D, Metabolic Syndrome, Metabolic Severity, FRS und ASCVD10 hinzugefügt.
- Berechnete Rechnerausgaben werden den aktuell geladenen Daten hinzugefügt und stehen in Analysemenüs zur Verfügung.
- Veraltete Reste der Datenschritte 4/5 entfernt und Variablenbeschriftungsbearbeitung in Schritt 3 abgeschlossen.
- Tabellennoten in Analyseausgaben vereinheitlicht, sodass ihre Breite der Tabelle entspricht.


## v0.7.2

### Geändert

- Reliabilitäts-Teilfaktorblöcke mit Gesamtzeilen, kombinierter Itemanalyse und Item-entfernt-Diagnostik über alle Items hinzugefügt.
- Reliabilitätsoptionen angepasst, sodass Omega-Statistiken nur bei aktivierter Omega-Option angezeigt und validiert werden.
- Größen der Reliabilitätslisten, Tabellenbreiten und Kopfausrichtung angepasst.


## v0.7.1

### Geändert

- Messwiederholungsbeschriftungen, gruppierte Ergebnisüberschriften, Post-hoc-Notizen und Effektgrößenangaben für Gepaarter Test (3+) verbessert.
- Höhen der Übertragungslisten und Ausrichtung der Übertragungsschaltflächen in Reliabilität, Häufigkeiten, gepaarten Tests, t-Test/ANOVA, Korrelation, Regression und hierarchischen Abläufen vereinheitlicht.

## v0.7.0

### Hinzugefügt

- Register Gepaarter Test (3+) für mindestens drei Messwiederholungen mit Weiterleitung an RM ANOVA, Friedman und Cochran's Q einschließlich Voraussetzungsprüfungen und Post-hoc-Vergleichen hinzugefügt.
- Effektgrößen für Messwiederholungen hinzugefügt: partielles Eta-Quadrat, Kendall's W, Hedges' g und Wilcoxon r.

### Geändert

- Ergebnistabellen gepaarter Tests, Post-hoc-Notation, Effektgrößenpositionen und HTML-/Excel-Layouts verbessert.
- Auswahlbereiche gepaarter Tests nutzen gruppierte Messwiederholungszeilen und kompaktere Ziellistenhöhen.


## v0.6.8

### Hinzugefügt

- Register für zwei gepaarte Messungen hinzugefügt: gepaarter t-Test/Wilcoxon, McNemar/exakter McNemar bei binären Paaren und Stuart-Maxwell/Bowker bei kategorialen Paaren.
- Optionale Voraussetzungsprüfung gepaarter Differenzen mit Shapiro-Wilk oder Schiefe-/Kurtosisdiagnostik sowie Ausreißerprüfung nach 3*IQR hinzugefügt.
- HTML- und Excel-Export für Tabellen gepaarter Tests und Voraussetzungsnotizen hinzugefügt.


## v0.6.7

### Geändert

- Analyseübertragungsschaltflächen durch Entfernen des gemeinsamen Abwärtsversatzes neu zentriert; Zweischaltflächenlayouts von Regression/t-Test an ihre Zielblockzeilen angepasst.


## v0.6.6

### Geändert

- Regression und hierarchische Regression zeigen kategoriale Koeffizienten als `variable:level` und enthalten die Standardreferenzzeile auch ohne expliziten Referenzwert im Datenregister.


## v0.6.5

### Geändert

- Ausrichtung der Analyseübertragungsschaltflächen aus der Einrichtungsgeometrie von 0.5.7 wiederhergestellt und auf das neue Reliabilitätsregister angewendet.
- Die Speicherschaltflächen der hierarchischen Regression wieder an der Aktionszeilenposition von 0.5.7 platziert.


## v0.6.4

### Geändert

- Die Korrelationskoeffizientenmatrix zeigt Signifikanzsterne, wenn die Option Signifikanzniveaus gewählt ist.


## v0.6.3

### Geändert

- Messniveauänderungen in Schritt 3 korrigiert, sodass beim Wechsel zu Analyseregistern aktuelle Messniveaus zusammen mit Beschriftungen übertragen werden.
- Messniveauauswahlfelder der Kategorienbeschriftungen aus Schritt 3 in die serverseitige direkte Eingabeerfassung aufgenommen.
- Den Speicherschaltflächenblock der hierarchischen Regression wieder unter den dritten Einrichtungsblock verlegt.


## v0.6.2

### Geändert

- Filterung abhängiger Variablen in Regression und hierarchischer Regression korrigiert, sodass Messniveauüberschreibungen aus Schritt 3 bei Auswahl kontinuierlicher Zielvariablen berücksichtigt werden.
- Analyseübertragungsschaltflächen nach Änderungen der gemeinsamen Einrichtungsgeometrie neu ausgerichtet.


## v0.6.1

### Geändert

- Messniveauweitergabe korrigiert, sodass Variablentypänderungen aus Schritt 3 sofort in den Einrichtungslisten von Reliabilität, Häufigkeiten, t-Test/ANOVA, Korrelation, Regression und Hierarchie wirksam werden.


## v0.6.0

### Geändert

- Reliabilitätsanalyse mit Auswahl gleich skalierter Items, automatischer KR-20/Cronbach's-Alpha/Omega-Auswahl, ordinalem Alpha/Omega, Itemdiagnostik und normalitätsabhängigen Methodenhinweisen hinzugefügt.
- Analysespeicherfunktionen über Ergebnisregister vereinheitlicht, mit editionsabhängigen HTML-/Abbildungs-/Excel-/Ergebnis-hinzufügen-Schaltflächen.
- HTML- und Excel-Ergebnisexport verbessert, sodass Tabellennoten mit passenden Tabellenbreiten und lesbaren Excel-Spaltenbreiten gespeichert werden.
- `psych` als Reliabilitätsengine für Alpha, Omega und polychorisch basierte ordinale Koeffizienten hinzugefügt.


## v0.5.7

### Geändert

- Hierarchische Regression neu eingerichtet: abhängige Variablen und jeweils ein aktiver Block, Vor-/Zurück-Navigation bei Erhaltung des Variablenzustands von Block 1/2/3.
- Höhen der hierarchischen Einrichtungsbereiche, Listengrößen und Blocknavigation für ein kompakteres, ausgerichtetes Layout angepasst.
- Trennlinien zwischen Koeffizienten- und Modellanpassungszeilen in hierarchischen Regressionstabellen verbessert.
- Spaltenbreiten und Innenabstände hierarchischer Regressionstabellen für breite Drei-Modell-Ausgaben mit Effektgrößenspalten abgestimmt.


## v0.5.6

### Geändert

- Gemeinsamen HTML-Export über Analyseergebnisregister hinzugefügt und gespeicherte Tabellen an den Regressionsstil der App angepasst.
- Korrelationsanalyse erweitert: automatische Methodenwahl nach Messniveau, optionale latente Korrelationen, Methoden-/Begründungsmatrizen, p-Wert-/95%-KI-Matrizen und größere Streu-/Heatmapabbildungen.
- Excel-/HTML-Exportgestaltung für Regression und hierarchische Regression mit zweistufigen Köpfen, Zahlenausrichtung, Notizen und Speicherdialogverhalten verbessert.
- Optionssteuerung von Regression und hierarchischer Regression während Bootstrap-Abläufen stabilisiert.
- Blockgeometrie der Einrichtung für nicht hierarchische Analyseregister vereinheitlicht.


## v0.5.5

### Geändert

- Korrelationsanalyse mit paarweisen Korrelationen, optionalen Normalitätsprüfungen, p-Werten, Konfidenzintervallen, Signifikanzmarkierungen, Matrixtabellen und Diagrammen implementiert.
- Excel-Tabellenexport für t-Test-/ANOVA-Ergebnisse hinzugefügt.
- Excel-Tabellenexport und Export von Residuen-Diagnoseabbildungen für hierarchische Regression hinzugefügt.
- Paketanforderungen lokaler Ausführung für neue Analyseabhängigkeiten aktualisiert.


## v0.5.4

### Geändert

- Effektgrößen, Trendanalyse, geordnete Signifikanznotation und erweiterte Post-hoc-Behandlung für t-Test / ANOVA hinzugefügt.
- Normalitätsoptionen, Modellübersichtsbezeichnungen, Statistikbezeichnungen, p-Wert-Notizen und Tabellenlayout von t-Test / ANOVA verbessert.
- Duncan-Mehrfachbereichstest über agricolae hinzugefügt und erforderliches Paketladen aktualisiert.
- Optionale Statistikspalten in Häufigkeiten / Deskriptive Statistiken korrigiert und Tabellen auf kompakte Regressionsbreiten eingestellt.
- Abstände, Trennlinien und Chi-Quadrat-Beschriftung hierarchischer Regressionstabellen verbessert.


## v0.5.2

### Geändert

- Bei Bootstrap-Regression werden Stichprobenanzahl und Startwert in der Modellübersicht angegeben.
- Regressionsvariablenübertragung bei Shift-Auswahl des ersten Elements stabilisiert und auswahlbedingte Scrollrücksetzungen reduziert.
- Die verfügbare Variablenliste der Regression auf eine Höhe von 20 Variablen vergrößert.
- SVG-Entwürfe des EasyFlow-Statistics-Logos hinzugefügt und verbessert.


## v0.5.1

### Geändert

- Regressionsvariablenübertragung einschließlich Mehrfachauswahl, Ctrl+A, Bewegungsrichtung und Reihenfolgeerhaltung stabilisiert.
- Regressionseinrichtung, feste Listenhöhen, Übertragungsschaltflächen und Beibehaltung von Optionsmarkierungen verbessert.
- Gemeinsames Tabellen- und Abbildungsexportverhalten für Analyseausgaben hinzugefügt.
- Einrichtungs- und Ausgabegerüst für Häufigkeiten / Deskriptive Statistiken mit gemeinsamer Variablenübertragungsoberfläche hinzugefügt.
- Zitationsmetadaten für EasyFlow Statistics aktualisiert.


## v0.5.0

### Hinzugefügt

- Gerüst einer Hierarchie-Registerkarte für hierarchische multiple Regression mit einer abhängigen Variablen und Prädiktoren in Block 1/2/3 hinzugefügt.
- Variablenübertragung von Block 2 nach Block 3 für die künftige hierarchische Regressionseinrichtung hinzugefügt.
- Gerüst einer Registerkarte für künftige verallgemeinerte Regressionsmodelle hinzugefügt.

### Geändert

- Optionen für verallgemeinerte GLM-artige Modelle aktualisiert; ausschließlich für OLS bestimmte Bootstrap- und sr2/f2-Optionen entfernt.
- Zählmodelle als Poisson / Negative binomial / Zero-inflated gruppiert; Gamma bleibt für positive kontinuierliche Ergebnisse erhalten.
- Berichtsoptionen verallgemeinerter Modelle verwenden exp(B) als IRR / Verhältnis.

## v0.4.1

### Geändert

- Regressionsregister und Seitenüberschriften von EasyFlow Statistics in Regression umbenannt.

### Behoben

- Leere VIF-Warntexte korrigiert, die `missing value where TRUE/FALSE needed` auslösen konnten.

## v0.4.0

### Hinzugefügt

- Native Windows-Speicherdialoge für Excel-Tabellenexport und Auswahl des Abbildungsordners hinzugefügt.
- Excel-Arbeitsmappenexport im Zeitschriftentabellenstil mit Koeffiziententabellen, Modellanpassungszeilen, Diagnostik und Notizen hinzugefügt.
- VIF-basierte Multikollinearitätswarnungen mit Hinweisen für schwerwiegende VIF-Werte hinzugefügt.
- Ridge-, LASSO- und Elastic-Net-Analysen mit Kreuzvalidierung für schwere Multikollinearität hinzugefügt.
- SCI-orientierte Tabellen regularisierter Regression für Modellleistung, OLS-/regularisierte Koeffizientenvergleiche und beibehaltene Prädiktoren hinzugefügt.

### Geändert

- Excel-Formatierung der Modellübersicht mit verbundenen Zellen gemeinsamer unabhängiger Variablen, Umbruch und kompakten Breiten verbessert.
- Residuen- und Durbin-Watson-Diagnostik werden bei Anzeige regularisierter Regressionsergebnisse ausgeblendet.
- Regressions-Ergebnisblattnamen nutzen nun Beschriftungen oder Namen der abhängigen Variablen.

### Behoben

- Fehler beim Speichern von Einstellungen ohne ausgewählte kategoriale Variablen behoben.
- Das Speichern leerer Messniveaunamen in Messniveauüberschreibungen verhindert.

## v0.3.1

### Geändert

- Modellübersicht für mehrere abhängige Variablen in einer Tabelle zusammengeführt.
- Regressionsausgabe so geordnet, dass zuerst alle Koeffiziententabellen und dann Diagnosegrafiken erscheinen.
- Voraussetzungsprüfungen und Durbin-Watson-Ergebnisse über abhängige Variablen jeweils in einer Tabelle zusammengeführt.
- Abhängige Variablen werden bei vorhandenem Label nur damit, andernfalls mit ihrem Variablennamen angezeigt.
- Effektgrößenrichtlinien einmal nach den Koeffiziententabellen angezeigt.

## v0.2.0

### Hinzugefügt

- Sequenzielle Regressionsausgabe für mehrere abhängige Variablen hinzugefügt.
- Bootstrap-Fortschritt und Abbruchfunktionen im Regressionseinrichtungsbereich hinzugefügt.
- Optionale Ausgaben für sr2, f2 und VIF-/Kollinearitätsdiagnostik hinzugefügt.
- Referenzen für Effektgrößenrichtlinien zu sr2 und Cohen's f2 hinzugefügt.
- Nebeneinander angeordnete Residuen-Diagnosegrafiken hinzugefügt.

### Geändert

- Regressionseinrichtung mit Variablen, abhängigen Variablen, unabhängigen Variablen und Bootstrap-Steuerung neu gestaltet.
- Modellübersicht aktualisiert: abhängige Variable, unabhängige Variablen, N, R2(adj. R2), F(p) und gewählte Methode.
- Residuenhomoskedastizitätsdiagramme und Anzeige der Ausreißergrenzen vereinheitlicht.
- Hoch-/Tiefstellungsnotation in Regressionsausgaben verbessert.

### Behoben

- Beschriftungsbearbeitung korrigiert, sodass Texteingaben nicht mehr bei jedem Zeichen zurückgesetzt werden.
- Laden gespeicherter Einstellungen und Weitergabe von Variablenlabels, Messniveaus, Referenzen und Wertelabels zwischen Schritten korrigiert.
- Bootstrap-Abbruchbehandlung und Position der Fortschrittsanzeige korrigiert.

## v0.1.2

### Hinzugefügt

- Auf-/Ab-Steuerelemente unter abhängigen Variablen in der Regressionseinrichtung hinzugefügt.
- Die Reihenfolge abhängiger Variablen bleibt in gespeicherten Einstellungen und Übersichten erhalten.

## v0.1.1

### Behoben

- Die Kopfzeilenschaltfläche `selected` in Schritt 3 aktiviert, um alle sichtbaren Variablen der aktiven Rolle auszuwählen oder abzuwählen.
- Die Anwenden-Schaltfläche in Schritt 3 übermittelt nun den aktuellen DataTables-Kontrollkästchenzustand.
- Bearbeitungen von `var_label`, `reference`, `value` und `label` bleiben beim Neuzeichnen von DataTables erhalten und synchronisiert.

## v0.1.0

### Hinzugefügt

- Erster Shiny-App-Prototyp.
- CSV-Upload und Variablenauswahl.
- Multiple Regressionsanalyse.
- Kolmogorov-Smirnov-Test mit Lilliefors-Korrektur zur Residuen-Normalitätsprüfung.
- Breusch-Pagan-Homoskedastizitätstest.
- HC3-robuste Standardfehler.
- Bootstrap-Konfidenzintervalle.
- Durbin-Watson-dL/dU-Nachschlagen mittels `C:/StatEdu/easyflow_statistics/easyflow_statistics_3.0.xlsx`.

