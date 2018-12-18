pfad="C:\\Users\\foit\\Desktop\\SPEAR_Calculator_20180919" #Pfad muss entsprechend angepasst werden...

#Monitoring-Daten aus der SPEAR-Berechnungsdatei der Software Indicate:
m=read.table(file.path(pfad,"MonitoringdatenSPEARcalculator.txt"),header=TRUE,sep="\t",stringsAsFactors=FALSE)
head(m)
#Taxa-Einträge werden für die folgenden Berechnungen angepasst
m$Taxa[grep("dae",m$Taxa)]=paste(m$Taxa[grep("dae",m$Taxa)]," Gen. sp.",sep="")
m$Taxa[which(m$Taxa == "Curculionoidea")]="Curculionidae Gen. sp."
m$Taxa
#Aus den zwei Namen wird eine Sample-ID generiert
m$sample=paste(m$Name_1,m$Name_2,sep="_")


#Die notwendigen Tabellen der SPEAR-Berechnung, die in der SPEAR-Berechnungsdatei der Software 'indicate' hinterlegt sind:
#Trait-Werte
t=read.table(file.path(pfad,"CE_5_aggregation.txt"),header=TRUE,quote="",sep="\t",stringsAsFactors = FALSE)
head(t);nrow(t)
#Link-Tabelle
l=read.table(file.path(pfad,"CE_6_link_aqem_aggregate.txt"),header=TRUE,quote="",sep="\t",stringsAsFactors = FALSE)
head(l);nrow(l)


#Schritt 1: Taxa für die Aggregierung anhängen
m=merge(m,l[,c("taxa","LINK")],by.x="Taxa",by.y="taxa",all.x=TRUE,all.y=FALSE)
head(m)

#Schritt 2: Aggregieren, d.h. alle Abundanzen pro Sample-ID mit gleichem Taxon (s. Spalte LINK) werden summiert:
require(plyr)
m$sampleTaxa=paste(m$sample,m$LINK,sep="_")#ist die gleich in der Funktion verwendete gruppierende Variable...
m2=ddply(m, .(sampleTaxa),function(x) { data.frame(
Taxa=x$LINK[1],
Name_1=x$Name_1[1],
Name_2=x$Name_2[1],
sample=x$sample[1],
Abundance=sum(x$Abundance)
)
})
nrow(m)
nrow(m2)#Anzahl würde sich durch diesen Schritt eigentlich reduzieren...nur in diesem Bsp nicht

#Schritt 3: Logarithmieren (s. angehängter Publikation 'Knillmann et al. 2018')
m2$abuL=log10(4*m2$Abundance+1)

#Schritt 4: Traits anhängen
m2=merge(m2,t[,c("taxa","gt","sens","exp","refu","SPEAR")],by.x="Taxa",by.y="taxa",all.x=TRUE,all.y=FALSE)
head(m2)

#Schritt 5: SPEAR pro Probenahme berechnen
m3=ddply(m2, .(sample),function(x) { data.frame(
sample=x$sample[1],
abuSum=sum(x$abuL,na.rm=TRUE),
abuSens=sum(x$abuL[x$SPEAR==1],na.rm=TRUE)
)
})
m3

#Schritt 6: SPEAR-Berechnung, nach der alten Logik:
m3$spear=100/m3$abuSum*m3$abuSens
m3

#Schritt 7: Neuer Zusatzschritt Normierung: SPEAR-Wert 34% wird auf 1 gesetzt...
m3$spearNormiert= m3$spear/34
m3

#Schritt 8: SPEAR-Klassen
#1: >0.9759456 (I)
#2: 0.9759456 - 0.7641648 (II)
#3: 0.7641648 - 0.5523841 (III)
#4: 0.5523841 - 0.3406033 (IV)
#5: 0.3406033 - 0 (V)
m3$spearKlasse=cut(m3$spearNormiert,breaks=c(0,0.3406033,0.5523841,0.7641648,0.9759456,100),right = FALSE)
levels(m3$spearKlasse)=c("V","IV","III","II","I")
m3

#Schritt 9: TU-Berechnung:
#Es gibt nur noch eine Formel zur TU-Berechnung, da nicht mehr zwischen Stellen mit und ohne Refugien im Oberlauf unterschieden wird...
#(s. angehängte Publikation von Knillmann et al. 2018)
#SPEAR = 0.1288 - 0.2118*TU
#TU = (0.1288-SPEAR)/0.2118
m3$tu=(0.1288-m3$spearNormiert)/0.2118
m3$tu=round(m3$tu,2)
m3

#--------------------------------
#Ergebnis, dass Ende des Monats (9/2018) mit dem Ergebnis aus 'Indicate' verglichen werden kann
write.table(m3,file.path(pfad,"SPEARresults.txt"),sep="\t",dec=".",row.names=FALSE)

#--------------------------------
