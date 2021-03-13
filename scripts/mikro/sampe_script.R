
********************************************************************************************************************
  ************************************************ Musterprogramm Stata **********************************************
  
  *** Hinweis: Das Musterprogramm dient Ihnen zur Orientierung bei der Erstellung Ihres Programmcodes.             ***
  *** Das Ziel ist Ihre Outputfiles bestmöglich für die Geheimhaltungsprüfung aufzubereiten.                       ***
  *** Bitte beachten Sie: Dieses Musterprogramm enthält keine statistikspezifischen Hilfestellungen zur            ***
  *** Aufbereitung und Auswertung der von Ihnen beantragten Daten.                                                 ***
  
  ********************************************************************************************************************
  
  clear all

*** Version festlegen
version <13> /*Version einfügen*/
  
  *** Bildschirmausgabe steuern
set more off
set logtype text
set linesize 255 

*** Ado-Pfad festlegen
sysdir set PERSONAL "<Pfad wird im Forschungsdatenzentrum ergänzt>"
mata mata mlib index

*** Makros für Pfade
global datenpfad "<Pfad wird im Forschungsdatenzentrum ergänzt>\"
global outputpfad "<Pfad wird im Forschungsdatenzentrum ergänzt>\"

*** Makros für Datei- und Outputnamen
global dateiname <dateiname.dta> /*Dateiname einfügen*/
global outputname <outputname>   /*Outputname einfügen*/


********************************************************************************************************************
*** Aufzeichung in Protokoll starten.
capture log close
log using "$outputpfad\$outputname.log", replace


********************************************************************************************************************
********************************************************************************************************************
********************************************************************************************************************


********************************************************************************************************************
********************************************************************************************************************
*** Titel des Projekts: 			<Frauen und Arbeit in der Bundesrepublik>
*** Datengrundlage: 				<Mikrozensus 2012>
***
*** Dateiname des Programmcodes: 	<syntaxname.do>
*** erstellt: 						<Datum> 
*** von: 							<Name> 
*** E-Mail: 						<E-Mail-Adresse> 
*** Tel.: 							<Telefonnummer> 
*** 
*** Dateiname des Output-Files: 	<outputname.log> 
*** 
*** 
*** Grundriss des Programms: 
***			<Programm zur Untersuchung von mehrfachen Personensätzen (Check der Datenbasis), 
***          deskriptive Analysen> 
***
*** 
*** Verwendete Variablen: 
*** Originalvariablen: 	
***			   <EF1:   	Land der Bundesrepublik 
*** 			EF2:   	Regierungsbezirk 
*** 			EF3s:  	Auswahlbezirks-Nummer 
*** 			EF4s:  	systemfreie Lfd. Nr. d. Haushalts 
*** 			EF5s:  	systemfreie Lfd. Nr. d. Person im Haushalt
*** 			EF25:  	systemfreie Lfd. Nr. d. Familie im Haushalt
*** 			EF30:  	Bevölkerung am Hauptwohnsitz
*** 			EF31:  	Bevölkerung in Privathaushalten
*** 			EF44:  	Alter
***				EF46:  	Geschlecht 
*** 			EF49:  	Familienstand
***				EF310: 	Höchster allgemeiner Schulabschluss
***             EF952: 	Standardhochrechnungsfaktor Jahr (Basis: Zensus 2011)>
*** 
***
*** Neu angelegte Variablen in dieser Syntax <syntaxname.sas>:  
***			   <verh:       dichotome Variable Verheiratet ja/nein
***             persnr:  	Personennummer
*** 			famnr:  	Familiennummer 
*** 			hhnr:   	Haushaltsnummer
*** 			nrdiff:  	Differenz Haushaltsnummer - Familiennummer 
*** 			piddiff: 	Test einmalige Personennummer>
*** 
*** Anmerkung: Falls Variablen verwendet werden, die in einer vorherigen Syntax erstellt wurden, 
*** diese bitte in separaten Blöcken auflisten.   
***            
*** Gewichtungsvariable: 	EF952
***
***
********************************************************************************************************************
********************************************************************************************************************



********************************************************************************************************************
*** I. Datenaufbereitung
********************************************************************************************************************


*** a. Datensatz einlesen
*** Speicher- und laufzeiteffizient die Auswahl der benötigten Variablen und Fälle direkt vornehmen.

*** Bevölkerung in Privathaushalten (EF31 = 1) am Ort der Hauptwohnung (EF30 < 3)

use EF1 - EF5s EF25 EF30 EF31 EF44 EF46 EF49 EF310 EF952 ///
using $datenpfad\$dateiname if EF31 == 1 &  EF30 < 3, clear


*** b. Generierung dichotome Variable verh, die angibt, ob Person verheiratet ist. 

recode EF49 (2 = 1) (1 3 4 5 6 7 = 0), gen (verh)


*** c. Generierung Haushalts- und Familiennummer aus EF1, EF2, EF3s, EF4s, EF5s, EF25
*** persnr: EF1 bis EF5s
*** hh: EF1 bis EF4s
*** famnr: EF1 bis EF4s + EF25

generate double persnr=(EF1*10000000000) + (EF2*1000000000) + (EF3s*100000) + (EF4s*1000) + (EF5s*10)
generate double hhnr=(EF1*10000000000) + (EF2*1000000000) + (EF3s*100000) + (EF4s*1000) + 1
generate double famnr=(EF1*10000000000) + (EF2*1000000000) + (EF3s*100000) + (EF4s*1000) + (EF25*1)
format %13.0g persnr hhnr famnr


*** d. Vorbereitung Test: Haushaltsnummer ungleich Familiennummer
*** Wenn: Familiennummer (famnr) ungleich der Haushaltsnummer (hhnr)
*** Dann: Differenz Haushaltsnummer - Familiennummer (nrdiff) gleich 1

generate nrdiff = 0

replace nrdiff = 1 if famnr ~= hhnr


*** e. Vorbereitung Test: mehrfache Personennummer
*** Personennummer (persnr) gleich der des Vorgängers dann Personennummertest (piddiff) gleich 1 

sort persnr

generate piddiff = 0
replace piddiff = 1 if persnr == persnr[_n-1]


*** f. Beschriftung der Variablen und Werte

label variable verh     "Verheiratet"

label variable hhnr 	"Haushaltsnummer"
label variable famnr 	"Familiennummer"
label variable persnr 	"Personennummer"

label variable piddiff 	"Test einmalige Personennummer"
label variable nrdiff 	"Differenz Haushaltsnummer - Familiennummer"

label define verh       1 "ja" ///
                        0 "nein"
label values verh verh
						
label define nrdiffer 	1 "Haushaltsnummer und Familiennummer unterschiedlich" ///
						0 "Haushaltsnummer und Familiennummer gleich"
label values nrdiff nrdiffer

label define piddiffer 	1 "Personennummer mehrfach" ///
						0 "Personennummer einfach"
label values piddiff piddiffer



********************************************************************************************************************
*** II. Datenauswertung
********************************************************************************************************************

*** Für jedes erzeugte Ergebnis muss angegeben werden, ob es auf Geheimhaltung geprüft und freigegeben werden soll 
*** oder ob es lediglich den Prüfprozessen der FDZ dient. Diese Angabe kann pauschal in einem Kommentar erfolgen, 
*** beispielweise "Alle erzeugten Auswertungen werden benötigt." oder "Es werden ausschließlich gewichtete 
*** Auswertungen benötigt.", solange die Formulierung eindeutig ist.

*** Output Nr. 1: Haushaltsnummer ungleich Familiennummer

tabulate nrdiff


*** Output Nr. 2: Test mehrfache Personennummer

tabulate piddiff


********************************************************************************************************************
*** a. gewichtete Auswertungen ***

*** Für die Geheimhaltungsprüfung ist es erforderlich, bei gewichteten Ausgaben zusätzlich auch 
*** das ungewichtete Ergebnis mit ausgeben zu lassen.

*** Output Nr. 3a: ungewichtete Häufigkeit des Bundeslandes

tabulate EF1


*** Output Nr. 3b: gewichtete Häufigkeit des Bundeslandes

tabulate EF1 [iw = EF952]


*** Output Nr. 4a: ungewichtete Regression mit der abhängigen Variablen Alter (EF44) und 
***                den beiden unabhängigen Variablen Geschlecht (EF46) und Verheiratet (verh)

reg EF44 EF46 verh


*** Output Nr. 4b: gewichtete Regression mit der abhängigen Variablen Alter (EF44) und 
***                den beiden unabhängigen Variablen Geschlecht (EF46) und Verheiratet (verh)

reg EF44 EF46 verh [aweight = EF952]


********************************************************************************************************************
*** b. Auswertungen mit Filterbedingungen ***

*** Restgrößen, die sich als Differenz zu "Insgesamt" erschließen, sind zu vermeiden.
*** Beispiel: Sie erzeugen eine "Insgesamt"-Tabelle und eine Tabelle "Männlich".
*** Die Restgröße/Differenz ist "Weiblich" und muss mit ausgegeben werden. 

*** Output Nr.5a: Häufigkeit des höchsten allgemeinen Schulabschlusses nach gesamt

tabulate EF310


*** Output Nr.5b: Häufigkeit des höchsten allgemeinen Schulabschlusses nach männlich

tabulate EF310 if EF46 == 1


*** Output Nr.5c: Häufigkeit des höchsten allgemeinen Schulabschlusses nach weiblich

tabulate EF310 if EF46 == 2 


*** Hinweis: Alternativ kann für diese Art der Auswertung auch eine Kreuztabelle ohne vorherige Fallauswahl 
*** verwendet werden. Durch die Verwendung einer Kreuztabelle werden Restgrößen direkt vermieden.


********************************************************************************************************************
*** c. Ausgabe deskriptiver Kennzahlen ***

*** Aus Geheimhaltungsgründen sind Minima und Maxima metrischer Variablen zu unterdrücken. 
*** Die Verwendung des tabulate-Befehls ist bei der Ausgabe von deskriptiven Kennzahlen zu vermeiden.

*** Output Nr. 6: Anzahl, Mittelwert und Standardabweichung des Alters aller ledigen Personen (EF49==1)

*** Variante 1: Verwendung des tabstat-Befehls

tabstat EF44 if EF49 == 1, statistics(n mean sd) 


*** Variante 2: Verwendung des sum-Befehls
*** Bei Verwendung des sum-Befehls ist ein Umschreiben des Sum-Befehls nötig. Die Verwendung der Option detail 
*** ist zu vermeiden. Vorab muss das Ado-File "makematrix.ado" geladen werden.

makematrix, from(r(N) r(mean) r(sd)): sum EF44 if EF49 == 1


*** Hinweis: Bei wirtschaftsstatistischen Daten ist bei der Ausgabe von deskriptiven Kennzahlen für die 
*** Geheimhaltungsprüfung das Maximum, der zweitgrößte Wert und die Gesamtsumme der Variablen 
*** (z.B. Umsatz, Einkünfte) mit auszugeben.


********************************************************************************************************************
*** d. Auswertungen von Dummy-Variablen ***

*** Zusätzlich zum Sum-Befehl ist die Häufigkeitstabelle auszugeben, da die Fallzahl der Merkmalsausprägung 
*** für die Geheimhaltungsprüfung benötigt wird.
 
*** Output Nr. 7a:  Anzahl, Mittelwert und Standardabweichung des Geschlechts aller über 65-Jährigen (EF44 > 65)

summarize EF46 if EF44 > 65


*** Output Nr. 7b:  Häufigkeitsauszählung des Geschlechts aller über 65-Jährigen (EF44 > 65)

tabulate EF46 if EF44 > 65


********************************************************************************************************************
*** e. Ausgabe von Wertetabellen ***

*** Bei der Erstellung von Wertetabellen ist stets die zugrundeliegende Fallzahl mit anzugeben.

*** Erstellen einer benutzerdefinierten Tabelle mit Anzahl, Mittelwert und gültige Häufigkeit (N) des Alters (EF44) 
*** je Ausprägung Familienstand (EF49)

*** Output Nr. 8: Kreuztabelle Alter und Familienstand

tabstat EF44, statistics(mean count) by (EF49)


********************************************************************************************************************

*** Die vorliegende Mustersyntax stellt ein Beispiel für eine Erstsyntax dar. Wenn in den folgenden Syntaxen 
*** Auswertungen gemacht werden, die einen inhaltlichen Bezug zu bereits erstellten Ergebnissen haben, sind die 
*** Bezüge zu den entsprechenden vorherigen Syntaxen sowohl bei der Datenaufbereitung als auch bei der Auswertung in 
*** einem Kommentar kenntlich zu machen.

********************************************************************************************************************
********************************************************************************************************************

log close

exit




