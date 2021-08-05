library(foreign)
library(ggplot2)
library(stringr)
library(reshape)
library(gridExtra)
library(psych)

#######################################################################
################ Sprungmarken Plot nach Analyse #######################
#######################################################################

setwd("/Users/nadine.boele/Documents/Uni/Videoauswertung/Auswertung Validierung Sprungmarken")
Gesamt <- read.csv2("Sprungmarken Gesamt (ohne Kommadopplungen).csv")
Gesamt <- Gesamt[,-16]

Gesamt$Thema <- NA
Gesamt$Thema[which(Gesamt$Schule==1 | Gesamt$Schule==2 | Gesamt$Schule==3)] <- 1
Gesamt$Thema[which(Gesamt$Schule==4 | Gesamt$Schule==5)] <- 2

Differenz <- Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Gleich.ungleich==1)] - Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Gleich.ungleich==1)]
######## Informationen zum Datensatz #######
length(Gesamt$Zeit) # Sprungmarken gesamt
length(Gesamt$Zeit[which(Gesamt$Thema==2)])
length(Gesamt$Zeit[which(Gesamt$Thema==1)])
length(Gesamt$Zeit[which(Gesamt$Gleich.ungleich==1)])/2 # Sprungmarken gleich je Kodierer
length(Gesamt$Zeit[which(Gesamt$Gleich.ungleich==1 & Gesamt$Muster_Vergleich==0)])/2 # Fehlerhaft als gleich zugeordnete Sprungmarken
length(Gesamt$Zeit[which(Gesamt$Gleich.ungleich==0)]) # Sprungmarken ungleich gesamt
length(Gesamt$Zeit[which(Gesamt$Gleich.ungleich==0 & Gesamt$Kodierer==1)]) # Sprungmarken ungleich Nadine
length(Gesamt$Zeit[which(Gesamt$Gleich.ungleich==0 & Gesamt$Kodierer==2)]) # Sprungmarken gleich Jenny

check <- data.frame(Gesamt$Zeit[which(Gesamt$Gleich.ungleich==1 & Gesamt$Muster_Vergleich == 1 & Gesamt$Kodierer==1)], Gesamt$Text[which(Gesamt$Gleich.ungleich==1 & Gesamt$Muster_Vergleich == 1 & Gesamt$Kodierer==1)], Gesamt$Zeit[which(Gesamt$Gleich.ungleich==1 & Gesamt$Muster_Vergleich == 1 & Gesamt$Kodierer==2)], Gesamt$Text[which(Gesamt$Gleich.ungleich==1 & Gesamt$Muster_Vergleich == 1 & Gesamt$Kodierer==2)])

setwd("/Users/nadine.boele/Documents/Uni/Videoauswertung/Sprungmarken Analyse")
write.csv2(check, "Check.csv")

######### Holsti Koeffizient #########
übereinstimmung_ohne <- length(Gesamt$Zeit[which(Gesamt$Gleich.ungleich==1 & Gesamt$Muster_Vergleich != 0)])
übereinstimmung_mit <- length(Gesamt$Zeit[which(Gesamt$Gleich.ungleich==1)])
übereinstimmung_nur2 <- length(Gesamt$Zeit[which(Gesamt$Gleich.ungleich==1 & Gesamt$Muster_Vergleich == 2)])

kodierer1 <- length(Gesamt$Zeit[which(Gesamt$Kodierer==1)])
kodierer2 <- length(Gesamt$Zeit[which(Gesamt$Kodierer==2)])

holsti_mit <- (übereinstimmung_mit)/(length(Gesamt$Zeit))
holsti_mit
holsti_ohne <- (übereinstimmung_ohne)/(length(Gesamt$Zeit))
holsti_ohne
(holsti_mit - holsti_ohne)/holsti_ohne
holsti_nach_warning <- (übereinstimmung_mit-2*9)/(length(Gesamt$Zeit))
holsti_nach_warning
(holsti_nach_warning - holsti_ohne)/holsti_ohne

######## Lernunterstützung deskriptiv ########
## Gesamt ##
Gesamt <- read.csv2("Sprungmarkenanalyse Gesamt.csv", header = T)
Gesamt <- Gesamt[,-1]

Gesamt$Thema <- NA
Gesamt$Thema[which(Gesamt$Schule==1 | Gesamt$Schule==2 | Gesamt$Schule==3)] <- 1
Gesamt$Thema[which(Gesamt$Schule==4 | Gesamt$Schule==5)] <- 2

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 1 & Gesamt$Muster_Relevanz!=0)]) # Anzahl kognitive Aktivierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 1 & Gesamt$Muster_Relevanz!=0)]) # Anzahl kognitive Aktivierung Jenny
length(Gesamt$Zeit[which(Gesamt$Muster_Kategorie == 1 & Gesamt$Muster_Relevanz!=0)]) # Anzahl kognitive Aktivierung

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 2 & Gesamt$Muster_Relevanz!=0)]) # Anzahl inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 2 & Gesamt$Muster_Relevanz!=0)]) # Anzahl inhaltliche Struktirierung Jenny
length(Gesamt$Zeit[which(Gesamt$Muster_Kategorie == 2 & Gesamt$Muster_Relevanz!=0)]) # Anzahl inhaltliche Struktirierung

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 3 & Gesamt$Muster_Relevanz!=0)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 3 & Gesamt$Muster_Relevanz!=0)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Jenny
length(Gesamt$Zeit[which(Gesamt$Muster_Kategorie == 3 & Gesamt$Muster_Relevanz!=0)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 4 & Gesamt$Muster_Relevanz!=0)]) # Anzahl Klassenführung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 4 & Gesamt$Muster_Relevanz!=0)]) # Anzahl Klassenführung Jenny
length(Gesamt$Zeit[which(Gesamt$Muster_Kategorie == 4 & Gesamt$Muster_Relevanz!=0)]) # Anzahl Klassenführung

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Relevanz == 2)]) # Anzahl sehr relevant Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Relevanz == 2)]) # Anzahl sehr relevant Jenny
length(Gesamt$Zeit[which(Gesamt$Muster_Relevanz == 2)]) # Anzahl sehr relevant

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Relevanz == 1)]) # Anzahl relevant Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Relevanz == 1)]) # Anzahl relevant Jenny
length(Gesamt$Zeit[which(Gesamt$Muster_Relevanz == 1)]) # Anzahl relevant

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Relevanz == -1)]) # Anzahl negativ relevant Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Relevanz == -1)]) # Anzahl negativ relevant Jenny
length(Gesamt$Zeit[which(Gesamt$Muster_Relevanz == -1)]) # Anzahl negativ relevant

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Relevanz == -2)]) # Anzahl negativ sehr relevant Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Relevanz == -2)]) # Anzahl negativ sehr relevant Jenny
length(Gesamt$Zeit[which(Gesamt$Muster_Relevanz == -2)]) # Anzahl negativ sehr relevant

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Relevanz == 0)]) # Anzahl nicht relevant Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Relevanz == 0)]) # Anzahl nicht relevant Jenny
length(Gesamt$Zeit[which(Gesamt$Muster_Relevanz == 0)]) # Anzahl nicht relevant

## Atombau ##
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz!=0)]) # Anzahl kognitive Aktivierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz!=0)]) # Anzahl kognitive Aktivierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz!=0)]) # Anzahl inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz!=0)]) # Anzahl inhaltliche Struktirierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz!=0)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz!=0)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz!=0)]) # Anzahl Klassenführung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz!=0)]) # Anzahl Klassenführung Jenny



length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==2)]) # Anzahl kognitive Aktivierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==2)]) # Anzahl kognitive Aktivierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==2)]) # Anzahl inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==2)]) # Anzahl inhaltliche Struktirierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==2)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==2)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==2)]) # Anzahl Klassenführung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==2)]) # Anzahl Klassenführung Jenny


length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==1)]) # Anzahl kognitive Aktivierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==1)]) # Anzahl kognitive Aktivierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==1)]) # Anzahl inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==1)]) # Anzahl inhaltliche Struktirierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==1)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==1)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==1)]) # Anzahl Klassenführung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==1)]) # Anzahl Klassenführung Jenny


length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==-1)]) # Anzahl kognitive Aktivierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==-1)]) # Anzahl kognitive Aktivierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==-1)]) # Anzahl inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==-1)]) # Anzahl inhaltliche Struktirierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==-1)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==-1)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==-1)]) # Anzahl Klassenführung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==-1)]) # Anzahl Klassenführung Jenny


length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==-2)]) # Anzahl kognitive Aktivierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==-2)]) # Anzahl kognitive Aktivierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==-2)]) # Anzahl inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==-2)]) # Anzahl inhaltliche Struktirierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==-2)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==-2)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==-2)]) # Anzahl Klassenführung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz==-2)]) # Anzahl Klassenführung Jenny



###### Relevanzen ohne gleiche Atombau

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==2)])

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==2)])

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==2)])

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==2)])

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==2)])


length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==2)])

length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==2)])

length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==2)])

length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==1 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==2)])


## Chemische Reaktion ##
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz!=0)]) # Anzahl kognitive Aktivierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz!=0)]) # Anzahl kognitive Aktivierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz!=0)]) # Anzahl inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz!=0)]) # Anzahl inhaltliche Struktirierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz!=0)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz!=0)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz!=0)]) # Anzahl Klassenführung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz!=0)]) # Anzahl Klassenführung Jenny



length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==2)]) # Anzahl kognitive Aktivierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==2)]) # Anzahl kognitive Aktivierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==2)]) # Anzahl inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==2)]) # Anzahl inhaltliche Struktirierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==2)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==2)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==2)]) # Anzahl Klassenführung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==2)]) # Anzahl Klassenführung Jenny


length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==1)]) # Anzahl kognitive Aktivierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==1)]) # Anzahl kognitive Aktivierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==1)]) # Anzahl inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==1)]) # Anzahl inhaltliche Struktirierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==1)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==1)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==1)]) # Anzahl Klassenführung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==1)]) # Anzahl Klassenführung Jenny


length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==-1)]) # Anzahl kognitive Aktivierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==-1)]) # Anzahl kognitive Aktivierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==-1)]) # Anzahl inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==-1)]) # Anzahl inhaltliche Struktirierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==-1)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==-1)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==-1)]) # Anzahl Klassenführung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==-1)]) # Anzahl Klassenführung Jenny


length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==-2)]) # Anzahl kognitive Aktivierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==-2)]) # Anzahl kognitive Aktivierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==-2)]) # Anzahl inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==-2)]) # Anzahl inhaltliche Struktirierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==-2)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==-2)]) # Anzahl kongitive Aktivierung & inhaltliche Struktirierung Jenny

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==-2)]) # Anzahl Klassenführung Nadine
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz==-2)]) # Anzahl Klassenführung Jenny

###### Relevanzen ohne gleiche CR

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==2)])

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==2)])

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==2)])

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==2)])


length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 1 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==2)])

length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 2 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==2)])

length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 3 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==2)])

length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz>0 & Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==1)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie == 4 & Gesamt$Thema==2 & Gesamt$Muster_Relevanz<0 & Gesamt$Muster_Vergleich==2)])


######## Lernunterstützung neue Auswertung ######
###Gesamtzahlen Kategorie
length(Gesamt$Zeit[which(Gesamt$Muster_Kategorie==1 & Gesamt$Muster_Relevanz!=0)])
length(Gesamt$Zeit[which(Gesamt$Muster_Kategorie==2 & Gesamt$Muster_Relevanz!=0)])
length(Gesamt$Zeit[which(Gesamt$Muster_Kategorie==3 & Gesamt$Muster_Relevanz!=0)])
length(Gesamt$Zeit[which(Gesamt$Muster_Kategorie==4 & Gesamt$Muster_Relevanz!=0)])


### Nadine Kategorie
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie==1 & Gesamt$Muster_Relevanz!=0)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie==2 & Gesamt$Muster_Relevanz!=0)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie==3 & Gesamt$Muster_Relevanz!=0)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie==4 & Gesamt$Muster_Relevanz!=0)])


### Jenny Kategorie
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie==1 & Gesamt$Muster_Relevanz!=0)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie==2 & Gesamt$Muster_Relevanz!=0)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie==3 & Gesamt$Muster_Relevanz!=0)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie==4 & Gesamt$Muster_Relevanz!=0)])


### Aufgeteilt nach Thema und Kategorie
length(Gesamt$Zeit[which(Gesamt$Thema==1 & Gesamt$Muster_Kategorie==1 & Gesamt$Muster_Relevanz!=0 & Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Thema==1 & Gesamt$Muster_Kategorie==2 & Gesamt$Muster_Relevanz!=0)])
length(Gesamt$Zeit[which(Gesamt$Thema==1 & Gesamt$Muster_Kategorie==3 & Gesamt$Muster_Relevanz!=0)])
length(Gesamt$Zeit[which(Gesamt$Thema==1 & Gesamt$Muster_Kategorie==4 & Gesamt$Muster_Relevanz!=0)])

length(Gesamt$Zeit[which(Gesamt$Thema==2 & Gesamt$Muster_Kategorie==1 & Gesamt$Muster_Relevanz!=0)])
length(Gesamt$Zeit[which(Gesamt$Thema==2 & Gesamt$Muster_Kategorie==2 & Gesamt$Muster_Relevanz!=0)])
length(Gesamt$Zeit[which(Gesamt$Thema==2 & Gesamt$Muster_Kategorie==3 & Gesamt$Muster_Relevanz!=0)])
length(Gesamt$Zeit[which(Gesamt$Thema==2 & Gesamt$Muster_Kategorie==4 & Gesamt$Muster_Relevanz!=0)])


### Nadine - Aufgeteilt nach Thema und Kategorie
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Thema==1 & Gesamt$Muster_Kategorie==1 & Gesamt$Muster_Relevanz!=0& Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Thema==1 & Gesamt$Muster_Kategorie==2 & Gesamt$Muster_Relevanz!=0& Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Thema==1 & Gesamt$Muster_Kategorie==3 & Gesamt$Muster_Relevanz!=0& Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Thema==1 & Gesamt$Muster_Kategorie==4 & Gesamt$Muster_Relevanz!=0& Gesamt$Muster_Vergleich==2)])

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Thema==2 & Gesamt$Muster_Kategorie==1 & Gesamt$Muster_Relevanz!=0& Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Thema==2 & Gesamt$Muster_Kategorie==2 & Gesamt$Muster_Relevanz!=0& Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Thema==2 & Gesamt$Muster_Kategorie==3 & Gesamt$Muster_Relevanz!=0& Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Thema==2 & Gesamt$Muster_Kategorie==4 & Gesamt$Muster_Relevanz!=0& Gesamt$Muster_Vergleich==2)])

### Jenny - Aufgeteilt nach Thema und Kategorie
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Thema==1 & Gesamt$Muster_Kategorie==1 & Gesamt$Muster_Relevanz!=0& Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Thema==1 & Gesamt$Muster_Kategorie==2 & Gesamt$Muster_Relevanz!=0& Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Thema==1 & Gesamt$Muster_Kategorie==3 & Gesamt$Muster_Relevanz!=0& Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Thema==1 & Gesamt$Muster_Kategorie==4 & Gesamt$Muster_Relevanz!=0& Gesamt$Muster_Vergleich==2)])

length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Thema==2 & Gesamt$Muster_Kategorie==1 & Gesamt$Muster_Relevanz!=0& Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Thema==2 & Gesamt$Muster_Kategorie==2 & Gesamt$Muster_Relevanz!=0& Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Thema==2 & Gesamt$Muster_Kategorie==3 & Gesamt$Muster_Relevanz!=0& Gesamt$Muster_Vergleich==2)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Thema==2 & Gesamt$Muster_Kategorie==4 & Gesamt$Muster_Relevanz!=0& Gesamt$Muster_Vergleich==2)])



### Gleiche Situation & gleiche Wertung Kategorien
length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie==1 & Gesamt$Muster_Vergleich==2 & Gesamt$Muster_Relevanz!=0)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie==1 & Gesamt$Muster_Vergleich==2 & Gesamt$Muster_Relevanz!=0)])

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie==2 & Gesamt$Muster_Vergleich==2 & Gesamt$Muster_Relevanz!=0)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie==2 & Gesamt$Muster_Vergleich==2 & Gesamt$Muster_Relevanz!=0)])

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie==3 & Gesamt$Muster_Vergleich==2 & Gesamt$Muster_Relevanz!=0)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie==3 & Gesamt$Muster_Vergleich==2 & Gesamt$Muster_Relevanz!=0)])

length(Gesamt$Zeit[which(Gesamt$Kodierer==1 & Gesamt$Muster_Kategorie==4 & Gesamt$Muster_Vergleich==2 & Gesamt$Muster_Relevanz!=0)])
length(Gesamt$Zeit[which(Gesamt$Kodierer==2 & Gesamt$Muster_Kategorie==4 & Gesamt$Muster_Vergleich==2 & Gesamt$Muster_Relevanz!=0)])




######## Lernunterstützung Mann-Wilcoxon-Test #######
wilcox.test(Gesamt$Muster_Kategorie ~ Gesamt$Thema) # Unterschied zwischen den Themen hinsichtlich der Kategorien
Relevanz.pos.neg <- NA
Relevanz.pos.neg[which(Gesamt$Muster_Relevanz==1 | Gesamt$Muster_Relevanz==2)] <- 1
Relevanz.pos.neg[which(Gesamt$Muster_Relevanz==-1 | Gesamt$Muster_Relevanz==-2)] <- 0

wilcox.test(Relevanz.pos.neg ~ Gesamt$Thema) # Unterschied zwischen den Themen hinsichtlich der Relevanzen (positiv vs. negativ)


######Plots #######
Schulname <- c("AAG", "Straubing", "Viechtach", "Schwandorf I", "Schwandorf II")

for(i in 1:5){
  for(j in 1:3){
    ## Vektor x mit allen Zeiten von Nadine und Jenny
    x <- (Gesamt$Zeit[which(Gesamt$Muster_Relevanz!=0 & Gesamt$Schule==i & Gesamt$Videonummer==j)])*24*60
    x <- round(x, 5) 
    x <- c(0.1, x)
    
    ## Vektor y für die Zuordnung der Zeiten zu den jeweiligen Kodierern: 1 für Nadine und 0 für Jenny, jeweils so oft wiederholen, wie der jeweilige Zeitvektor lang ist
    y <- Gesamt$Kodierer[which(Gesamt$Muster_Relevanz!=0 & Gesamt$Schule==i & Gesamt$Videonummer==j)]
    y[which(y==1)] <- 0
    y[which(y==2)] <- 0.5
    y <- c(0, y)
    
    ## Vektor mit allen Farbzuordnungen (Übereinstimmungen) der beiden Kodierer
    vergleich <- as.numeric(Gesamt$Muster_Vergleich[which(Gesamt$Muster_Relevanz!=0 & Gesamt$Schule==i & Gesamt$Videonummer==j)])
    vergleich[which(is.na(vergleich))] <- 0
    vergleich <- vergleich+1
    vergleich <- c(1, vergleich)
    vergleich <- factor(vergleich)
    
    kategorie <- factor(Gesamt$Muster_Kategorie[which(Gesamt$Muster_Relevanz!=0 & Gesamt$Schule==i & Gesamt$Videonummer==j)])
    kategorie <- c(1, kategorie)
    kategorie <- factor(kategorie)
    relevanz <- as.numeric(Gesamt$Muster_Relevanz[which(Gesamt$Muster_Relevanz!=0 & Gesamt$Schule==i & Gesamt$Videonummer==j)])
    relevanz <- relevanz+3
    relevanz <- c(1, relevanz)
    relevanz <- factor(relevanz)
    
    ## Dataframe mit den für die Graphik relevanten Vektoren x, y und farbe
    plot <- data.frame(x,y,vergleich, kategorie, relevanz)
    
    ## Erstellen eines Vektors mit den Intervallgrenzen für den Zeitmarkenplot (5 Minuten-Abstände)
    grenzen <- seq(from = 0, to = 130, by = 5)
    
    plots1 = marrangeGrob(
      lapply(1 : max(which(max(plot$x) > grenzen)), function(x){
        ggplot(plot, aes(x=x, y=y)) + geom_point(aes(fill=vergleich, shape=kategorie), color="#b4e1ff01", size=8)  + geom_point(data=plot, aes(x=x, y=y, color=relevanz), size=2) + scale_colour_manual(values=c("#870000ff", "#e57e00ff", "#ffdd55ff", "#73ff73ff")) + scale_fill_manual(values=c("#a5cff5ff", "#517eb7ff", "#182b41ff")) + scale_shape_manual(values = c(21:24)) + ylim(-0.5, 1) + xlim(grenzen[x], grenzen[x+1]) + labs(x="", y="") + theme(legend.position="none") + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) + guides(fill=guide_legend(override.aes=list(shape=21)))
      }),
      nrow = 3, ncol=1, top=""
    )

    Dateiname <- paste(Schulname[i], " ", j, ".pdf", sep="")
    ggsave(Dateiname, plots1, width = 25, height = 15, units = "cm")
    
    
  }
}

1-length(Gesamt$Muster_Vergleich[which(Gesamt$Gleich.ungleich==1 & Gesamt$Muster_Vergleich==0)])/length(Gesamt$Muster_Vergleich[which(Gesamt$Gleich.ungleich==1)])


write.csv2(data.frame(Gesamt$Zeit[which(Gesamt$Gleich.ungleich==1 & Gesamt$Muster_Vergleich==0)],Gesamt$Kodierer[which(Gesamt$Gleich.ungleich==1 & Gesamt$Muster_Vergleich==0)], Gesamt$Schule[which(Gesamt$Gleich.ungleich==1 & Gesamt$Muster_Vergleich==0)], Gesamt$Videonummer[which(Gesamt$Gleich.ungleich==1 & Gesamt$Muster_Vergleich==0)]), "Fehlerhaft zugeirdnete Zeiten.csv")
