# Datenauswertung zum Grundkurs

# Hinweise zum Skript
# Ich habe versucht, das Skript möglichst gut zu gliedern. Die wichtigsten Abschnitte sind durch eine gestrichelte
# Linie voneinander abgegrenzt. Die Verweisnummer nach dem Rautenzeichen in der Überschrift verweist verweist auf
# die entsprechende Tabelle oder Abbildung im Paper. Bei Tabellen folgt auf die Raute (#) einfach die Nummer der 
# Tabelle. Beispiel: "#02". Die Überschrift versucht die Tabelle zu beschreiben. Bei Abbildingen folgt auf die 
# Raute noch ein "ab". Beispiel "#ab01". Auswertung von Daten im Text werden durch # und eine dreistellige Ziffer
# gekennzeichnet, z.B "#001". Auch hier gefolgt von einer Beschreibung. Darüber hinaus lassen sich die 
# Abbildungen als jpeg ausgeben. Diese Abschnitte sind im Skript eingerückt. 

#--------------------------------------------------------------------------
# Einlesen der Daten und nötige Korrekturen

daten.1<-read.table("300_daten/daten1.csv", header=TRUE, sep=";")# Datensatz mit den 99 Teilnehmer/-innen des Grundkurses einlesen
daten.2<-read.table("300_daten/daten2.csv", header=TRUE, sep=";", na="-99") # Datensatz mit den 11 Interviewten einlesen

#--------------------------------------------------------------------------


#01: Die Tabelle 1 gibt die Teilnahmevariablen nach Klinger wider. Daher gibt es es keine relevanten Abschnitt
# zu dieser Tabelle in diesem Skript.

#--------------------------------------------------------------------------


#02 Aussage zu der Fakultätszugehörigkeit der Interviewten 11 Personen (Tabelle 02)
x5<-table(daten.2$fak)
x5
# [1]	Philosophische Fakultät und Fachbereich Theologie
# [2]	Naturwissenschaftliche Fakultät
# [3]	Rechts- und Wirtschaftswissenschaftliche Fakultät
# [4]	Technische Fakultät
# [5]	Medizinische Fakultät
x6<-round(prop.table(x5)*100,1)
x7<-rbind(x5,x6)
dimnames(x7)<-list(c("abs. Haufigkeit", "rel. Häufigkeit"),c("PhilFak","NatFak","ReWi","TechFak","MedFak"))
tabelle02<-x7
write.table(x7, file="clipboard", sep="\t", row.names=FALSE)
rm(x5,x6,x7)
tabelle02

#--------------------------------------------------------------------------


#03 Aussage zu den Kenndaten der Interviewten (Tabelle 03)
x1<-summary(daten.2$age)
x2<-sd(daten.2$age)
x3<-summary(daten.2$experience)
x4<-sd(daten.2$experience)
x5<-summary(daten.2$mitarbeit)
x6<-sd(daten.2$mitarbeit)
x11<-round(x1[4],1)
x22<-round(x2, 1)
x12<-cbind(x11,x22)
x33<-round(x3[4],1)
x44<-round(x4,1)
x13<-cbind(x33,x44)
x55<-round(x5[4],1)
x66<-round(x6,1)
x14<-cbind(x55,x66)
x15<-rbind(x12,x13,x14)
str(x15)
dimnames(x15)<-list(c("Alter","Lehrerfahrung","Uni.-Mitarbeiter"), c("x quer", "SD"))
tabelle03<-x15
tabelle03
rm(x1,x2,x3,x4,x5,x6,x11,x22,x33,x44,x55,x66,x12,x13,x14,x15)

#--------------------------------------------------------------------------


# Tabelle 4 gibt die Lernziele im Grundkurs wider. Daher hier nicht relevant.

#--------------------------------------------------------------------------

#05 Anzahl der Zertifikate (Tabelle 05)
x1<-c(12,24,20,19)
x2<-c(35,35,20,30)
x3<-cbind(x1,x2)
x4<-addmargins(x3)
dimnames(x4)<-list(c(2016,2017,2018,2019,"Summe"),c("Grundkurs","Offenes Programm","Summe"))
x5<-round(prop.table(x3,margin=1),3)*100
x6<-prop.table(round(colSums(x3),3))*100
x7<-rbind(x5,x6)
x8<-rowSums(x7)
x9<-cbind(x7,x8)
x10<-round(x9,1)
x11<-cbind(x4,x10)
dimnames(x11)<-list(c(2016,2017,2018,2019,"Summe"),c("Grundkurs","Offenes Programm","Summe","Grundkurs","Offenes Programm","Summe"))
tabelle05<-x11
tabelle05
rm(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11)
write.table(tabelle05, file="clipboard", sep="\t", row.names=TRUE)

#--------------------------------------------------------------------------

#001: Universitätszugehörigkeit
round(prop.table(table(daten.1$uni))*100, 1) # Zugehörigkeit zur FAU, "1" steht für die FAU

#002 Alter und Geschlecht der Teilnehmer
summary(daten.1$age) # Wichtigste Kennzahlen der Teilnehmer: Min, Quartile, Durchschnitt, Max
with(daten.1, max(age)-min(age)) # Spannweite
round(sd(daten.1$age),1) # Standardabweichung
table(daten.1$sex.fak) # Geschlecht, absoluter Anteil
SexKurs<-round(prop.table(table(daten.1$sex.fak)),3)*100 # Geschlecht, Relativer Anteil 
SexKurs

	#ab01 Boxplot erzeugen für Altersverteilung (Abbildung 01)
	daten.1$sex.fak<-factor(daten.1$sex, levels=c(1,2), labels=c("männlich","weiblich"))

	jpeg("500_grafik/001AlterGeschlecht.jpeg")
	boxplot(daten.1$age, main="Altersverteilung (n=99)")
	dev.off()

	#ab02 Boxplot erzeugen für nach Geschlecht getrennte Altersverteilung (Abbildung 02)
	jpeg("500_grafik/002AlterGeschlecht.jpeg")
	boxplot(daten.1$age~daten.1$sex.fak, bty="n", main="Altersverteilung nach Geschlecht",xlab="Geschlecht",ylab="Alter",las=1) # Gruppierter Boxplot für Alter und Geschlecht. Zeigt, dass die Frauen
	dev.off()

#--------------------------------------------------------------------------

#06 Aussage: Die Fakultäten lassen sich in eine Reihenfolge bringen, und zwar in Abhängigkeit vom Alter (Tabelle 06)
x0<-with(daten.1, table(fac))
x1<-with(daten.1, round(tapply(age, fac, mean),1))
x2<-with(daten.1, round(tapply(age, fac, sd),1))
x3<-with(daten.1, round(tapply(age, fac, min),1))
x4<-with(daten.1, round(tapply(age, fac, max),1))
x5<-with(daten.1, round(tapply(age, fac, median),1))
x6<-cbind(x0,x1,x2,x3,x4,x5)
dimnames(x6)<-list(c("Philosophische Fakultät","Naturwissenschaftliche Fakultät","Rechts- u. Wirtschaftswissenschaftliche Fakultät","Technische Fakultät","Medizinische Fakultät","Zentrale Einrichtungen","Externe Teilnehmer"), c("Häufigkeit","Alter","Standardabweichung","Miniumum","Maxiumum","Median"))
x7<-sort.list(x6[,2])
x8<-x6[x7,]
tabelle06<-x8
tabelle06
write.table(tabelle06, file="clipboard", sep="\t", row.names=TRUE)
rm(x0,x1,x2,x3,x4,x5,x6,x7,x8)

#--------------------------------------------------------------------------

#ab03 AussageFaktultätszugehörigkeit der Teilnehmer/-innen (nach Geschlecht) (Abbildung 3)
daten.1$fac.fak<-factor(daten.1$fac, levels=c(1,2,3,4,5,6,7), labels=c("PhilFak","NatFak","ReWi","TechFak","MedFak","ZentralE","NB"))
# daten.1$sex.fak<-factor(daten.1$sex, levels=c(1,2), labels=c("männlich","weiblich"))
x1<-with(daten.1, table(fac))
dimnames(x1)<-list(c("PhilFak","NatFak","ReWi","TechFak","MedFak","ZentralE","NB"))
x2<-with(daten.1, table(sex, fac))
dimnames(x2)<-list(c("männlich","weiblich"),c("PhilFak","NatFak","ReWi","TechFak","MedFak","ZentralE","NB"))
sex.fakultaet<-x2

	
	
	jpeg("500_grafik/003SexFakultaet.jpeg")
	barplot(x2,legend=TRUE, main="Fakultätszugehörigkeit (und Geschlecht)",sub="n=99", las=1) 
	dev.off()
rm(x1,x2)

#--------------------------------------------------------------------------

#003 Aussage zu Dropout (Abbildung 04)
table(daten.1$finished) 						# abs. Häufigkeiten Dropout ("2")
x3<-table(daten.1$sex.fak,daten.1$finished) 			# absolute Häufigkeiten nach Geschlecht
dimnames(x3)<-list(c("männlich","weiblich"),c("abgeschlossen","dropout"))
x3 										# Tabelle mit der abs. Häufigkeit Dropout nach Geschlecht
round(prop.table(table(daten.1$finished))*100,1) 		# prozentualer Anteil derjenigen, die den Kurs beendet haben

#004 Dropout-Gruppe nach Durchschnittsaler, SD und Median
x1<- round(with(daten.1,tapply(age, finished, mean)),1) 	# Durchschnittsalter in der Dropout-Gruppe ("2")
x2<- round(with(daten.1,tapply(age, finished, sd)),1)   	# Standardabweichung Dropout-Gruppe ("2")
x3<- round(with(daten.1,tapply(age, finished, median)),1)	# Median der Dropout-Gruppe ("2")
x4<- cbind(x1,x2,x3)
dimnames(x4)<-list(c("Erfolgreiche","Dropout"),c("Durchschnittsalter","SD","Median"))
dropout<-x4
dropout 					# tabellarische Darstellung der Dropout-Gruppe den wichtigsten Kennzahlen

#005  Dropout abs. nach Fakultäten
x4<-table(daten.1$finished,daten.1$fac)
dimnames(x4)<-list(c("abgeschlossen","dropout"),c("PhilFak","NatFak","ReWi","TechFak","MedFak","ZentralE","NB"))
dropout.fac<-x4
dropout.fac


	#ab04	
	jpeg("500_grafik/004fakultaet.jpeg")
	barplot(dropout.fac, legend=TRUE, main="Teilnehmer/-innen nach Fakultät mit Dropout", sub="n=99")
	dev.off()

rm(x1,x2,x3,x4)

	# Das Durchschnittsalter derjenigen, die den Kurs beendet haben, ist 33.9 Jahre. Das Durchschnittsalter derjenigen,
	# die den Kurs abgebrochen haben ist 38.2 Jahre. Die Standardabweichung bei der ersten Gruppe ist mit 6,6 Jahren 
	# höher als bei der zweiten Gruppe mit 5,7 Jahren.

#--------------------------------------------------------------------------


#07 Zufriedenheit mit dem Kurs (Abbildung 5)

	# n, erstellt die Anzahl der Teilnehmer, die auf diese Frage geantwortet haben
auswahl.1<-with(daten.2,data.frame(hospitation, vortrag, entwurf, refbericht, beratung)) # Auswahl erstellen
x1<-table(is.na(auswahl.1[,1]))
x2<-table(is.na(auswahl.1[,2]))
x3<-table(is.na(auswahl.1[,3]))
x4<-table(is.na(auswahl.1[,4]))
x5<-table(is.na(auswahl.1[,5]))
x6<-cbind(x1,x2,x3,x4,x5)
rownames(x6)<-c("n","na")
colnames(x6)<-c("Hospitation", "Vortrag", "Lehrentwurf", "Reflexionsbericht", "Beratung")
zufriedenheit.n<-x6[1,]
write.table(zufriedenheit.n, file="clipboard", sep="\t", row.names=TRUE)

	# erstellt die Werte für die Abb. 5: Zufriedenheit mit den einzelnen Elementen
zufriedenheit.1<-round(colMeans(auswahl.1,na.rm=TRUE),1)
zufriedenheit.2<-6-zufriedenheit.1
names(zufriedenheit.2)<- c("Hospitation", "Vortrag", "Lehrentwurf", "Reflexionsbericht", "Beratung")

x7<-round(sd(auswahl.1[,1],na.rm=TRUE),2)
x8<-round(sd(auswahl.1[,2],na.rm=TRUE),2)
x9<-round(sd(auswahl.1[,3],na.rm=TRUE),2)
x10<-round(sd(auswahl.1[,4],na.rm=TRUE),2)
x11<-round(sd(auswahl.1[,5],na.rm=TRUE),2)
x12<-cbind(x7,x8,x9,x10,x11)
x13<-rbind(zufriedenheit.2,x12,zufriedenheit.n)
dimnames(x13)<- list(c("x_quer","SD","n"), c("Hospitation", "Vortrag", "Lehrentwurf", "Reflexionsbericht", "Beratung"))
tabelle07<-x13

	#ab05 Zufriedenheit mit Elementen im Kurs (Abbildung 5)
	jpeg("500_grafik/005Zufriedenheit.jpeg") #öffnen
	barplot(zufriedenheit.2, ylim=c(0,5), las=1, ylab="Zufriedenheit", main="Zufriedenheit mit einzelnen Elementen des Grundkurses", sub="5 = Höchste Zufriedenheit") # mit las=1 werden die Ziffern an der x-Achse senkrecht dargestellt
	abline(h=5,lty=3) # mit abline wird eine Linie auf der Höhe 5 eingefügt, die 3 steht für die gepunktete Linie
	dev.off()

tabelle07


rm(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13)
rm(auswahl.1,zufriedenheit.n,zufriedenheit.1,zufriedenheit.2)

#--------------------------------------------------------------------------


#08 Tabelle erstellen zu Motivation und Lernverhalten

x1<-table(daten.2$motiv)
x11<-round(prop.table(table(daten.2$motiv)),3)
x2<-table(daten.2$nachbereit)
x22<-round(prop.table(table(daten.2$nachbereit)),3)
x3<-table(daten.2$litrecherche)
x33<-round(prop.table(table(daten.2$litrecherche)),3)
x4<-table(daten.2$beoblehrender)
x44<-round(prop.table(table(daten.2$beoblehrender)),3)
x5<-table(daten.2$kollaustauch)
x55<-round(prop.table(table(daten.2$kollaustauch)),3)
x6<-table(daten.2$studaustauch)
x66<-round(prop.table(table(daten.2$studaustauch)),3)
x7<-rbind(x1,x2,x3,x4,x5,x6)
x8<-rbind(x11,x22,x33,x44,x55,x66)
x9<-cbind(x7,(x8*100))
dimnames(x9)<-list(c("Motiv: intrinsisch/extrinsisch","Nachbereitung","Literaturrecherche","Beobachtung Lehrender","Kollegialer Austausch","Austausch mit Studierenden"),c("Ja","Nein","Ja","Nein"))
tabelle08<-x9
write.table(tabelle08, file="clipboard", sep="\t", row.names=TRUE)

rm(x1,x11,x2,x22,x3,x33,x4,x44,x5,x55,x6,x66,x7,x8,x9)

	# ab 06: Abbildung 06 Mitarbeit im Grundkurs
	lernverhalten<-tabelle08[2:6,3:4]
	dimnames(lernverhalten)<-list(c("Nachbereitung","Litrecherche","Lehrende beob.","Kolleg. Aust.","Aust. Stud."),c("Ja","Nein"))
	x1<-(t(lernverhalten))

	jpeg("500_grafik/006Mitarbeit.jpeg") #öffnen
	barplot(x1, main="Lernverhalten in der Stichprobe", sub="Grau=Nein, Schwarz=Ja,(n=11)", beside=FALSE) 
	dev.off()
rm(x1)

#--------------------------------------------------------------------------

#09 Kreuztabelle Literaturrecherche und Kursnachbereitung (Tabelle 9)
x1<-table(daten.2$litrecherche,daten.2$nachbereit)
x2<-addmargins(x1)
x3<-round(prop.table(x1)*100,1)
x4<-addmargins(x3)
x5<-cbind(x2,x4)
dimnames(x5)<-list(c("Ja","Nein","Summe"),c("Ja","Nein","Summe","Ja","Nein","Summe"))
tabelle09<-x5
rm(x1,x2,x3,x4,x5)
tabelle09
write.table(tabelle09, file="clipboard", sep="\t", row.names=TRUE)

#--------------------------------------------------------------------------

#10 Weitere Kursteilnahme bei den Absolventen des Grundkurses (Tabelle 10)
x1<-c(21,11)
x2<-c(14,6)
x3<-cbind(x1,x2)
x4<-addmargins(x3)
dimnames(x4)<-list(c("Keine weiteren Kurse","Weitere Kurse","Summe"),c("Offenes Programm","Grundkurs","Summe"))
zertifikate.2018<-x4
zertifikate.2018

x5<-round(prop.table(x3, margin=2),3)*100
x6<-zertifikate.2018[1:2,3]
x7<-round(prop.table(x6),3)*100
x8<-cbind(x5,x7)
x9<-colSums(x8)
x10<-rbind(x8,x9)
dimnames(x10)<-list(c("Keine weiteren Kurse","Weitere Kurse","Summe"),c("Offenes Programm","Grundkurs","Summe"))
zertifikate.2018proz<-x10
x11<-cbind(zertifikate.2018,zertifikate.2018proz)
tabelle10<-x11

rm(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,zertifikate.2018,zertifikate.2018proz)
write.table(tabelle10, file="clipboard", sep="\t", row.names=TRUE)
	
	# ab07 Weitere Teilnahme an HD-Veranstaltungen
	jpeg("500_grafik/007Zertifikate.jpeg")
	barplot(tabelle10[1:2,4:6], main="Weitere Teilnahme an HD-Veranstaltungen", sub="(n=52)")
	dev.off()

#--------------------------------------------------------------------------

#11 Verbleib an der FAU
x1<-subset(daten.1,subset=kurs<8)
x2<-table(x1$kurs,x1$fau_start)
x3<-x2[,1]
x4<-table(x1$kurs,x1$fau_nov)
x5<-x4[,1]
x6<-round(x5/x3*100,0)
x7<-rbind(x3,x5,x6)
x8<-t(x7)
x3<-cbind(x2,rowSums(x2))
dimnames(x8)<-list(c("1_2016","2_2017","3_2017","4_2018","5_2018","6_2019","7_2019"),c("TN der FAU","noch immer FAU","noch FAU in %"))
tabelle11<-x8
tabelle11

write.table(tabelle11, file="clipboard", sep="\t", row.names=TRUE)

rm(x1,x2,x3,x4,x5,x6,x7,x8)

#--------------------------------------------------------------------------
tabelle02
tabelle03
tabelle05
tabelle06
tabelle07
tabelle08
tabelle09
tabelle10
tabelle11

#--------------------------------------ENDE--------------------------------