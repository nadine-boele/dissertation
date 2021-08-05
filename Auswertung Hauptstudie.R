library(stringr)
library(tidyr)
library(psych)

setwd("/Users/nadineboele/Desktop/Promotion/Hauptstudie")

###### Abbildung Sprungmarken #####
library(ggplot2)

Gesamt <- data.frame(
  Gleich.ungleich = c(rep("gleich",818), rep("gleich",818), rep("ungleich",833), rep("ungleich",274)),
  Kodierer = c(rep("J", 818), rep("N", 818), rep("N", 833), rep("J", 274))
)

ggplot(data=Gesamt, aes(x=Gleich.ungleich, y=stat(count), group=factor(Kodierer), fill=factor(Kodierer))) + 
  geom_bar()+
  scale_fill_manual(values = c("steelblue", "indianred"), labels = c("Kodierer:in 1", "Kodierer:in 2"))+
  labs(y="Häufigkeit", fill="", x="")+
  geom_text(aes(label=stat(count)), stat="count", position = position_stack(vjust=0.5))




###### Datensatz einlesen #####
### Hauptstudie & Experten2
file.edit('/Users/nadineboele/Desktop/Promotion/Hauptstudie/import HS und Exp.r')
data.hs <- ds

data.hs$code <- paste(data.hs$SD01_01, data.hs$SD01_02, data.hs$SD01_03, sep="")
data.hs <- data.hs[order(data.hs$QUESTNNR),]

# Zusammenfassen der beiden Themen 
num.c <- c(grep("C3", names(data.hs)), grep("C5", names(data.hs)), grep("C6", names(data.hs)), 
           which(names(data.hs)%in%c("CK01", "CK02", "CK03", "CK04", "CK05", "CK06", "CK07", "CK08", "CK09")))
dup <- which(duplicated(data.hs$code)==T)

pos1 <- c()
pos2 <- c()
for(i in dup){
  pos1[i] <- which(data.hs$code==data.hs$code[i])
  pos2[i] <- i
}
pos1 <- pos1[!is.na(pos1)]
pos2 <- pos2[!is.na(pos2)]

data.hs[pos1,num.c] <- data.hs[pos2,num.c]
data.hs <- data.hs[pos1,]


###
data.hs$Studie <- NA
data.hs$Studie[which(data.hs$QUESTNNR=="A_E2")] <- "Exp"
data.hs$Studie[which(data.hs$QUESTNNR=="A_HS")] <- "HS"
data.hs$Studie[which(data.hs$SD04=="Lehrer*in ")] <- "HS_LP"

data.hs$Status <- NA
data.hs$Status[which(data.hs$SD04=="Student*in des Lehramts")] <- "Stud"
data.hs$Status[which(data.hs$SD04=="Lehrer*in")] <- "LP"
data.hs$Status[which(data.hs$SD23=="Seminarlehrer*in")] <- "SemLP"
data.hs$Status[which(data.hs$SD23=="Fachdidaktiker*in (Hochschule)")] <- "Prom"


## Unnötige Variablen entfernen
data.hs <- data.hs[,-which(names(data.hs)%in%names(data.hs[grep("103_", names(data.hs))]))]
data.hs <- data.hs[,-which(names(data.hs)%in%names(data.hs[grep("203_", names(data.hs))]))]
data.hs <- data.hs[,-which(names(data.hs)%in%names(data.hs[grep("403_", names(data.hs))]))]
data.hs <- data.hs[,-which(names(data.hs)%in%names(data.hs[grep("303_", names(data.hs))]))]
data.hs <- data.hs[,-which(names(data.hs)%in%names(data.hs[grep("503_", names(data.hs))]))]
data.hs <- data.hs[,-which(names(data.hs)%in%names(data.hs[grep("603_", names(data.hs))]))]
data.hs <- data.hs[,-which(names(data.hs)%in%names(data.hs[grep("104_", names(data.hs))]))]
data.hs <- data.hs[,-which(names(data.hs)%in%names(data.hs[grep("204_", names(data.hs))]))]
data.hs <- data.hs[,-which(names(data.hs)%in%names(data.hs[grep("404_", names(data.hs))]))]
data.hs <- data.hs[,-which(names(data.hs)%in%names(data.hs[grep("304_", names(data.hs))]))]
data.hs <- data.hs[,-which(names(data.hs)%in%names(data.hs[grep("504_", names(data.hs))]))]
data.hs <- data.hs[,-which(names(data.hs)%in%names(data.hs[grep("604_", names(data.hs))]))]

data.hs <- data.hs[,-which(names(data.hs)%in%c("SD23","SD20_06","SD05_06","SD10","SD22", "SD20", "SD11_01", "SD12", "SD12_01",  "SD12_02",  "SD12_03" , "SD12_04" ,
                                               "SD12_05"  ,"SD12_06"  ,"SD12_07"  ,"SD12_07a" ,"SD21"     ,"SD21_01"  ,"SD21_02" ,
                                               "SD21_03"  ,"SD21_04"  ,"SD21_05"  ,"SD21_06"  ,"SD21_07"  ,"SD21_07a" ,"SD13", "SD13_01"  ,"SD13_01a" ,"SD13_02"  ,"SD13_03"  ,"SD13_04"  ,"SD13_05"  ,"SD13_06" , "SD16"    ,"SD16_01"  ,"SD16_02"  ,"SD16_03"  ,"SD16_04"  ,"SD16_05"  ,"SD16_06" , "SD16_07" ,"SD17"     ,"SD18_01" ))]

data.hs <- data.hs[,-which(names(data.hs)%in%names(data.hs[grep("TIME", names(data.hs))]))]
data.hs <- data.hs[-which(data.hs$SD04=="Mitarbeiter*in bzw. Dozent*in an einer Hochschule"),]


### Hauptstude Rgbg
file.edit('/Users/nadineboele/Desktop/Promotion/Hauptstudie/import HS Rgbg.r')
data.hs.rgbg <- ds

data.hs.rgbg$code <- paste(data.hs.rgbg$SD01_01, data.hs.rgbg$SD01_02, data.hs.rgbg$SD01_03, sep="")

data.hs.rgbg <- data.hs.rgbg[,-which(names(data.hs.rgbg)%in%names(data.hs.rgbg[grep("103_", names(data.hs.rgbg))]))]
data.hs.rgbg <- data.hs.rgbg[,-which(names(data.hs.rgbg)%in%names(data.hs.rgbg[grep("203_", names(data.hs.rgbg))]))]
data.hs.rgbg <- data.hs.rgbg[,-which(names(data.hs.rgbg)%in%names(data.hs.rgbg[grep("403_", names(data.hs.rgbg))]))]
data.hs.rgbg <- data.hs.rgbg[,-which(names(data.hs.rgbg)%in%names(data.hs.rgbg[grep("303_", names(data.hs.rgbg))]))]
data.hs.rgbg <- data.hs.rgbg[,-which(names(data.hs.rgbg)%in%names(data.hs.rgbg[grep("503_", names(data.hs.rgbg))]))]
data.hs.rgbg <- data.hs.rgbg[,-which(names(data.hs.rgbg)%in%names(data.hs.rgbg[grep("603_", names(data.hs.rgbg))]))]
data.hs.rgbg <- data.hs.rgbg[,-which(names(data.hs.rgbg)%in%names(data.hs.rgbg[grep("104_", names(data.hs.rgbg))]))]
data.hs.rgbg <- data.hs.rgbg[,-which(names(data.hs.rgbg)%in%names(data.hs.rgbg[grep("204_", names(data.hs.rgbg))]))]
data.hs.rgbg <- data.hs.rgbg[,-which(names(data.hs.rgbg)%in%names(data.hs.rgbg[grep("404_", names(data.hs.rgbg))]))]
data.hs.rgbg <- data.hs.rgbg[,-which(names(data.hs.rgbg)%in%names(data.hs.rgbg[grep("304_", names(data.hs.rgbg))]))]
data.hs.rgbg <- data.hs.rgbg[,-which(names(data.hs.rgbg)%in%names(data.hs.rgbg[grep("504_", names(data.hs.rgbg))]))]
data.hs.rgbg <- data.hs.rgbg[,-which(names(data.hs.rgbg)%in%names(data.hs.rgbg[grep("604_", names(data.hs.rgbg))]))]

data.hs.rgbg <- data.hs.rgbg[,-which(names(data.hs.rgbg)%in%names(data.hs.rgbg[grep("TIME", names(data.hs.rgbg))]))]

data.hs.rgbg$Studie <- "HS"
data.hs.rgbg$Status <- "Stud"


### Gesamtfragebogen Hauptstudie
data <- rbind(data.hs, data.hs.rgbg)

###### Datensatz aufbereiten ######
######## Variablennamen anpassen ###
names(data)[grep("117_", names(data))] <- str_replace(names(data)[grep("117_", names(data))], "117_", "1.N")
names(data)[grep("118_", names(data))] <- str_replace(names(data)[grep("118_", names(data))], "118_", "1.I")
names(data)[grep("103_", names(data))] <- str_replace(names(data)[grep("103_", names(data))], "103_", "1.Z")
names(data)[grep("104_", names(data))] <- str_replace(names(data)[grep("104_", names(data))], "104_", "1.TXT")
names(data)[grep("105_", names(data))] <- str_replace(names(data)[grep("105_", names(data))], "105_", "1.time.Kontext")
names(data)[grep("106_", names(data))] <- str_replace(names(data)[grep("106_", names(data))], "106_", "1.time.Video")
names(data)[grep("107_", names(data))] <- str_replace(names(data)[grep("107_", names(data))], "107_", "1.time.SA")
names(data)[grep("108_", names(data))] <- str_replace(names(data)[grep("108_", names(data))], "108_", "1.time.KBR")

names(data)[grep("217_", names(data))] <- str_replace(names(data)[grep("217_", names(data))], "217_", "2.N")
names(data)[grep("218_", names(data))] <- str_replace(names(data)[grep("218_", names(data))], "218_", "2.I")
names(data)[grep("203_", names(data))] <- str_replace(names(data)[grep("203_", names(data))], "203_", "2.Z")
names(data)[grep("204_", names(data))] <- str_replace(names(data)[grep("204_", names(data))], "204_", "2.TXT")
names(data)[grep("205_", names(data))] <- str_replace(names(data)[grep("205_", names(data))], "205_", "2.time.Kontext")
names(data)[grep("206_", names(data))] <- str_replace(names(data)[grep("206_", names(data))], "206_", "2.time.Video")
names(data)[grep("207_", names(data))] <- str_replace(names(data)[grep("207_", names(data))], "207_", "2.time.SA")
names(data)[grep("208_", names(data))] <- str_replace(names(data)[grep("208_", names(data))], "208_", "2.time.KBR")

names(data)[grep("417_", names(data))] <- str_replace(names(data)[grep("417_", names(data))], "417_", "4.N")
names(data)[grep("418_", names(data))] <- str_replace(names(data)[grep("418_", names(data))], "418_", "4.I")
names(data)[grep("403_", names(data))] <- str_replace(names(data)[grep("403_", names(data))], "403_", "4.Z")
names(data)[grep("404_", names(data))] <- str_replace(names(data)[grep("404_", names(data))], "404_", "4.TXT")
names(data)[grep("405_", names(data))] <- str_replace(names(data)[grep("405_", names(data))], "405_", "4.time.Kontext")
names(data)[grep("406_", names(data))] <- str_replace(names(data)[grep("406_", names(data))], "406_", "4.time.Video")
names(data)[grep("407_", names(data))] <- str_replace(names(data)[grep("407_", names(data))], "407_", "4.time.SA")
names(data)[grep("408_", names(data))] <- str_replace(names(data)[grep("408_", names(data))], "408_", "4.time.KBR")

names(data)[grep("316_", names(data))] <- str_replace(names(data)[grep("316_", names(data))], "316_", "3.N")
names(data)[grep("317_", names(data))] <- str_replace(names(data)[grep("317_", names(data))], "317_", "3.I")
names(data)[grep("303_", names(data))] <- str_replace(names(data)[grep("303_", names(data))], "303_", "3.Z")
names(data)[grep("304_", names(data))] <- str_replace(names(data)[grep("304_", names(data))], "304_", "3.TXT")
names(data)[grep("305_", names(data))] <- str_replace(names(data)[grep("305_", names(data))], "305_", "3.time.Kontext")
names(data)[grep("306_", names(data))] <- str_replace(names(data)[grep("306_", names(data))], "306_", "3.time.Video")
names(data)[grep("307_", names(data))] <- str_replace(names(data)[grep("307_", names(data))], "307_", "3.time.SA")
names(data)[grep("308_", names(data))] <- str_replace(names(data)[grep("308_", names(data))], "308_", "3.time.KBR")

names(data)[grep("516_", names(data))] <- str_replace(names(data)[grep("516_", names(data))], "516_", "5.N")
names(data)[grep("517_", names(data))] <- str_replace(names(data)[grep("517_", names(data))], "517_", "5.I")
names(data)[grep("503_", names(data))] <- str_replace(names(data)[grep("503_", names(data))], "503_", "5.Z")
names(data)[grep("504_", names(data))] <- str_replace(names(data)[grep("504_", names(data))], "504_", "5.TXT")
names(data)[grep("505_", names(data))] <- str_replace(names(data)[grep("505_", names(data))], "505_", "5.time.Kontext")
names(data)[grep("506_", names(data))] <- str_replace(names(data)[grep("506_", names(data))], "506_", "5.time.Video")
names(data)[grep("507_", names(data))] <- str_replace(names(data)[grep("507_", names(data))], "507_", "5.time.SA")
names(data)[grep("508_", names(data))] <- str_replace(names(data)[grep("508_", names(data))], "508_", "5.time.KBR")

names(data)[grep("616_", names(data))] <- str_replace(names(data)[grep("616_", names(data))], "616_", "6.N")
names(data)[grep("617_", names(data))] <- str_replace(names(data)[grep("617_", names(data))], "617_", "6.I")
names(data)[grep("603_", names(data))] <- str_replace(names(data)[grep("603_", names(data))], "603_", "6.Z")
names(data)[grep("604_", names(data))] <- str_replace(names(data)[grep("604_", names(data))], "604_", "6.TXT")
names(data)[grep("605_", names(data))] <- str_replace(names(data)[grep("605_", names(data))], "605_", "6.time.Kontext")
names(data)[grep("606_", names(data))] <- str_replace(names(data)[grep("606_", names(data))], "606_", "6.time.Video")
names(data)[grep("607_", names(data))] <- str_replace(names(data)[grep("607_", names(data))], "607_", "6.time.SA")
names(data)[grep("608_", names(data))] <- str_replace(names(data)[grep("608_", names(data))], "608_", "6.time.KBR")

## Entfernen von Items
items <- c("A1.N01",
           "A1.N08",
           "A1.N11",
           "A1.N12",
           "A1.I02",
           "A1.I06",
           "A1.I10",
           "A1.I11",
           "A1.I12",
           "A1.I13",
           "A1.I16",
           "A2.N02",
           "A2.N06",
           "A2.N07",
           "A2.N08",
           "A2.N09",
           "A2.N11",
           "A2.I01",
           "A2.I04",
           "A2.I08",
           "A2.I09",
           "A2.I14",
           "A2.I15",
           "A4.N04",
           "A4.N05",
           "A4.N07",
           "A4.N15",
           "A4.I01",
           "A4.I04",
           "A4.I05",
           "A4.I09",
           "A4.I13",
           "A4.I14",
           "A4.I16",
           "C3.N08",
           "C3.N10",
           "C3.N12",
           "C3.I01",
           "C3.I08",
           "C3.I12",
           "C3.I13",
           "C5.N07",
           "C5.I02",
           "C5.I05",
           "C5.I06",
           "C5.I08",
           "C5.I09",
           "C5.I11",
           "C5.I14",
           "C6.N03",
           "C6.N08",
           "C6.N10",
           "C6.I03",
           "C6.I05",
           "C6.I07",
           "C6.I08",
           "C6.I09")

j=1
pos <- NA
for(i in items){
  pos[j] <- which(names(data)==i)
  j=j+1
}

data <- data[,-pos]


###### Einlesen und Aufbereiten der offenen Antworten ######
data.offen1 <- read.csv2("/Users/nadineboele/Desktop/Promotion/Hauptstudie (doppelt kodiert).csv")
data.offen1 <- data.offen1[-which(data.offen1$cid%in%c(7:25,30:33,37)),]
data.offen1$cid[which(data.offen1$cid%in%c(4,6,35))] <- 6
data.offen1$cid[which(data.offen1$cid==5)] <- 4
data.offen1$cid[which(data.offen1$cid==34)] <- 5
data.offen1$cid[which(data.offen1$cid==36)] <- 7
data.offen1$cid[which(data.offen1$cid==26)] <- 8
data.offen1$cid[which(data.offen1$cid==27)] <- 9
data.offen1$cid[which(data.offen1$cid==28)] <- 10
data.offen1$cid[which(data.offen1$cid==29)] <- 11
data.offen1$fid[which(data.offen1$fid==11)] <- 1
data.offen1$fid[which(data.offen1$fid==12)] <- 2
data.offen1$fid[which(data.offen1$fid==13)] <- 3
data.offen1$fid[which(data.offen1$fid==14)] <- 4
data.offen1$fid[which(data.offen1$fid==15)] <- 5
data.offen1$fid[which(data.offen1$fid==16)] <- 6
data.offen1$fid[which(data.offen1$fid==17)] <- 7
data.offen1$fid[which(data.offen1$fid==18)] <- 8
data.offen1$fid[which(data.offen1$fid==19)] <- 9
data.offen1$fid[which(data.offen1$fid==20)] <- 10 
data.offen1$fid <- data.offen1$fid+98

data.offen2 <- read.csv2("/Users/nadineboele/Desktop/Promotion/Hauptstudie.csv")
data.offen <- rbind(data.offen2,data.offen1)
fid <- read.csv2("/Users/nadineboele/Desktop/Promotion/Code.csv")

for(i in 1:nrow(fid)){
  data.offen$fid[which(data.offen$fid==fid$id[i])] <- fid$name[i]
}

data$freq_KF <- NA
data$freq_LU <- NA
data$freq_FD <- NA
data$freq_SA <- NA
data$freq_AB <- NA
data$freq_T <- NA
data$freq_S <- NA
data$freq_Beschreiben <- NA
data$freq_Bewerten <- NA
data$freq_Begründen <- NA
data$freq_Alternative <- NA
data$freq_BeschreibenT <- NA
data$freq_BewertenT <- NA
data$freq_BegründenT <- NA
data$freq_AlternativeT <- NA
data$freq_BewertenF <- NA
data$freq_BegründenF <- NA
data$freq_AlternativeF <- NA

fid$freq_KF <- NA
fid$freq_LU <- NA
fid$freq_FD <- NA
fid$freq_SA <- NA
fid$freq_AB <- NA
fid$freq_T <- NA
fid$freq_S <- NA
fid$freq_FF <- NA
fid$freq_Beschreiben <- NA
fid$freq_Bewerten <- NA
fid$freq_Begründen <- NA
fid$freq_Alternative <- NA
fid$freq_BeschreibenT <- NA
fid$freq_BewertenT <- NA
fid$freq_BegründenT <- NA
fid$freq_AlternativeT <- NA
fid$freq_BewertenF <- NA
fid$freq_BegründenF <- NA
fid$freq_AlternativeF <- NA

### Kommentare Transkript markieren
pos <- NA
a=1
for(i in 1:nrow(data.offen)){
  if(data.offen$cid[i]==7){
    file <- which(data.offen$fid[i]==data.offen$fid) #andere Kommentare der Gleichen Person
    i1 <- which(data.offen$selfirst[i]<=data.offen$selfirst)
    i2 <- which(data.offen$selend[i]>=data.offen$selend)
    
    for(j in 1:length(file)){
      if(any(file[j]==i1)){
        if(any(file[j]==i2)){
          pos[a] <- file[j]
          a=a+1
        }
      }
    }
  }
}

data.offen$Transkript <- NA
data.offen$Transkript[pos] <- TRUE

pos <- which(data.offen$cid[which(data.offen$Transkript==T)]=="8") #Position Beschreiben & Transkript
data.offen$cid[pos] <- 13
pos <- which(data.offen$cid[which(data.offen$Transkript==T)]=="9") #Position Bewerten & Transkript
data.offen$cid[pos] <- 14
pos <- which(data.offen$cid[which(data.offen$Transkript==T)]=="10") #Position Begründen & Transkript
data.offen$cid[pos] <- 15
pos <- which(data.offen$cid[which(data.offen$Transkript==T)]=="11") #Position Alternative & Transkript
data.offen$cid[pos] <- 16


### Kommentare fachlicher Fehler markieren
pos <- NA
a=1
for(i in 1:nrow(data.offen)){
  if(data.offen$cid[i]==12){
    file <- which(data.offen$fid[i]==data.offen$fid) #andere Kommentare der Gleichen Person
    i1 <- which(data.offen$selfirst[i]<=data.offen$selfirst)
    i2 <- which(data.offen$selend[i]>=data.offen$selend)
    
    for(j in 1:length(file)){
      if(any(file[j]==i1)){
        if(any(file[j]==i2)){
          pos[a] <- file[j]
          a=a+1
        }
      }
    }
  }
}

data.offen$Fehler <- NA
data.offen$Fehler[pos] <- TRUE

pos <- which(data.offen$cid[which(data.offen$Fehler==T)]=="9") #Position Bewerten & Transkript
data.offen$cid[pos] <- 17
pos <- which(data.offen$cid[which(data.offen$Fehler==T)]=="10") #Position Begründen & Transkript
data.offen$cid[pos] <- 18
pos <- which(data.offen$cid[which(data.offen$Fehler==T)]=="11") #Position Alternative & Transkript
data.offen$cid[pos] <- 19

##Summenwerte der einzelnen Ausprägungen bilden
same.fid <- NA
for(i in 1:nrow(fid)){
  if(length(which(data.offen$fid==fid$name[i]))!=0){
    same.fid <- which(data.offen$fid==fid$name[i])
    table <- table(data.offen$cid[same.fid])
    for(j in 1:length(names(table))){
      if(names(table)[j]=="1"){
        fid$freq_KF[i] <- table[j]
      }
      if(names(table)[j]=="2"){
        fid$freq_LU[i] <- table[j]
      }
      if(names(table)[j]=="3"){
        fid$freq_FD[i] <- table[j]
      }
      if(names(table)[j]=="4"){
        fid$freq_SA[i] <- table[j]
      }
      if(names(table)[j]=="5"){
        fid$freq_AB[i] <- table[j]
      }
      if(names(table)[j]=="6"){
        fid$freq_S[i] <- table[j]
      }
      if(names(table)[j]=="7"){
        fid$freq_T[i] <- table[j]
      }
      if(names(table)[j]=="8"){
        fid$freq_Beschreiben[i] <- table[j]
      }
      if(names(table)[j]=="9"){
        fid$freq_Bewerten[i] <- table[j]
      }
      if(names(table)[j]=="10"){
        fid$freq_Begründen[i] <- table[j]
      }
      if(names(table)[j]=="11"){
        fid$freq_Alternative[i] <- table[j]
      }
      if(names(table)[j]=="12"){
        fid$freq_FF[i] <- table[j]
      }
      if(names(table)[j]=="13"){
        fid$freq_BeschreibenT[i] <- table[j]/2
      }
      if(names(table)[j]=="14"){
        fid$freq_BewertenT[i] <- table[j]/2
      }
      if(names(table)[j]=="15"){
        fid$freq_BegründenT[i] <- table[j]/2
      }
      if(names(table)[j]=="16"){
        fid$freq_AlternativeT[i] <- table[j]/2
      }
      if(names(table)[j]=="17"){
        fid$freq_BewertenF[i] <- table[j]*0
      }
      if(names(table)[j]=="18"){
        fid$freq_BegründenF[i] <- table[j]*0
      }
      if(names(table)[j]=="19"){
        fid$freq_AlternativeF[i] <- table[j]*0
      }
    }
  }
}

## Häufigkeiten in Datensatz kopieren ###
for(i in 1:nrow(fid)){
  pos <- which(data$code==fid$name[i])
  data$freq_KF[pos] <- fid$freq_KF[i]
  data$freq_LU[pos] <- fid$freq_LU[i]
  data$freq_FD[pos] <- fid$freq_FD[i]
  data$freq_SA[pos] <- fid$freq_SA[i]
  data$freq_AB[pos] <- fid$freq_AB[i]
  data$freq_S[pos] <- fid$freq_S[i]
  data$freq_T[pos] <- fid$freq_T[i]
  data$freq_Beschreiben[pos] <- fid$freq_Beschreiben[i]
  data$freq_Bewerten[pos] <- fid$freq_Bewerten[i]
  data$freq_Begründen[pos] <- fid$freq_Begründen[i]
  data$freq_Alternative[pos] <- fid$freq_Alternative[i]
  data$freq_BeschreibenT[pos] <- fid$freq_BeschreibenT[i]
  data$freq_BewertenT[pos] <- fid$freq_BewertenT[i]
  data$freq_BegründenT[pos] <- fid$freq_BegründenT[i]
  data$freq_AlternativeT[pos] <- fid$freq_AlternativeT[i]
  data$freq_BewertenF[pos] <- fid$freq_BewertenF[i]
  data$freq_BegründenF[pos] <- fid$freq_BegründenF[i]
  data$freq_AlternativeF[pos] <- fid$freq_AlternativeF[i]
}

## relative Häufigkeiten
data$rel_KF <- data$freq_KF/rowSums(data[,c("freq_KF","freq_LU","freq_FD","freq_SA","freq_AB","freq_S")], na.rm = T)
data$rel_LU <- data$freq_LU/rowSums(data[,c("freq_KF","freq_LU","freq_FD","freq_SA","freq_AB","freq_S")], na.rm = T)
data$rel_FD <- data$freq_FD/rowSums(data[,c("freq_KF","freq_LU","freq_FD","freq_SA","freq_AB","freq_S")], na.rm = T)
data$rel_SA <- data$freq_SA/rowSums(data[,c("freq_KF","freq_LU","freq_FD","freq_SA","freq_AB","freq_S")], na.rm = T)
data$rel_AB <- data$freq_AB/rowSums(data[,c("freq_KF","freq_LU","freq_FD","freq_SA","freq_AB","freq_S")], na.rm = T)
data$rel_S <- data$freq_S/rowSums(data[,c("freq_KF","freq_LU","freq_FD","freq_SA","freq_AB","freq_S")], na.rm = T)

data$rel_KF[which(is.na(data$rel_KF))] <- 0
data$rel_LU[which(is.na(data$rel_LU))] <- 0
data$rel_FD[which(is.na(data$rel_FD))] <- 0
data$rel_SA[which(is.na(data$rel_SA))] <- 0
data$rel_AB[which(is.na(data$rel_AB))] <- 0
data$rel_S[which(is.na(data$rel_S))] <- 0


data$rel_Beschreiben <- data$freq_Beschreiben/(rowSums(data[,c("freq_Beschreiben","freq_Bewerten","freq_Begründen", "freq_Alternative")],na.rm = T)+rowSums(data[,c("freq_BewertenT","freq_BegründenT", "freq_AlternativeT")],na.rm = T)*0.5+rowSums(data[,c("freq_BewertenF","freq_BegründenF", "freq_AlternativeF")],na.rm = T)*0)
data$rel_Bewerten <- data$freq_Bewerten/(rowSums(data[,c("freq_Beschreiben","freq_Bewerten","freq_Begründen", "freq_Alternative")],na.rm = T)+rowSums(data[,c("freq_BewertenT","freq_BegründenT", "freq_AlternativeT")],na.rm = T)*0.5+rowSums(data[,c("freq_BewertenF","freq_BegründenF", "freq_AlternativeF")],na.rm = T)*0)
data$rel_Begründen <- data$freq_Begründen/(rowSums(data[,c("freq_Beschreiben","freq_Bewerten","freq_Begründen", "freq_Alternative")],na.rm = T)+rowSums(data[,c("freq_BewertenT","freq_BegründenT", "freq_AlternativeT")],na.rm = T)*0.5+rowSums(data[,c("freq_BewertenF","freq_BegründenF", "freq_AlternativeF")],na.rm = T)*0)
data$rel_Alternative <- data$freq_Alternative/(rowSums(data[,c("freq_Beschreiben","freq_Bewerten","freq_Begründen", "freq_Alternative")],na.rm = T)+rowSums(data[,c("freq_BewertenT","freq_BegründenT", "freq_AlternativeT")],na.rm = T)*0.5+rowSums(data[,c("freq_BewertenF","freq_BegründenF", "freq_AlternativeF")],na.rm = T)*0)
data$rel_Beschreiben[which(is.na(data$rel_Beschreiben))] <- 0
data$rel_Beschreiben[which(data$rel_Beschreiben==0 & is.na(data$rel_Bewerten) & is.na(data$rel_Begründen) & is.na(data$rel_Alternative))] <- NA

data$freq_Beschreiben[which(is.na(data$freq_Beschreiben))] <- 0
data$freq_BeschreibenT[which(is.na(data$freq_BeschreibenT))] <- 0
data$freq_Bewerten[which(is.na(data$freq_Bewerten))] <- 0
data$freq_BewertenT[which(is.na(data$freq_BewertenT))] <- 0
data$freq_Begründen[which(is.na(data$freq_Begründen))] <- 0
data$freq_BegründenT[which(is.na(data$freq_BegründenT))] <- 0
data$freq_Alternative[which(is.na(data$freq_Alternative))] <- 0
data$freq_AlternativeT[which(is.na(data$freq_AlternativeT))] <- 0
data$freq_BewertenF[which(is.na(data$freq_BewertenF))] <- 0
data$freq_BegründenF[which(is.na(data$freq_BegründenF))] <- 0
data$freq_AlternativeF[which(is.na(data$freq_AlternativeF))] <- 0

## Score Ebene
data$Score.Ebene <- (rowSums(data[,c("freq_Bewerten","freq_Begründen", "freq_Alternative")],na.rm = T)+rowSums(data[,c("freq_BewertenT","freq_BegründenT", "freq_AlternativeT")],na.rm = T)*0.5+rowSums(data[,c("freq_BewertenF","freq_BegründenF", "freq_AlternativeF")],na.rm = T)*0)*2+data$freq_Beschreiben+data$freq_BeschreibenT*0.5 #mit Beschreiben

###### Stichprobendaten #####
table(data$SD04) # Statusgruppe
table(data$SD03) # Bundesland
table(data$SD05) # Schulart
table(data$SD06_01) # Fachsemester
table(data$SD09_02) # Anzahl Biologie
table(data$SD09_03) # Anzahl Physik
table(data$SD09_04) # Anzahl Mathematik
table(data$SD09_05) # Anzahl Geo
table(data$SD09_06) # Anzahl Englisch
table(data$SD09_07) # Anzahl Sonstiges
table(data$SD09_07a)
length(which(data$SD09_01==T & data$SD09_0==T))

###### Umkodierung der Items pU ######
data$r_A1.N02<-NA
data$r_A1.N03<-NA
data$r_A1.N04<-NA
data$r_A1.N05<-NA
data$r_A1.N07<-NA
data$r_A1.N10<-NA
data$r_A1.N13<-NA
data$r_A2.N03<-NA
data$r_A2.N04<-NA
data$r_A2.N05<-NA
data$r_A2.N12<-NA
data$r_A2.N13<-NA
data$r_A4.N01<-NA
data$r_A4.N02<-NA
data$r_A4.N03<-NA
data$r_A4.N06<-NA
data$r_A4.N08<-NA
data$r_A4.N09<-NA
data$r_A4.N10<-NA
data$r_A4.N12<-NA
data$r_A4.N13<-NA
data$r_A4.N14<-NA
data$r_C3.N01<-NA
data$r_C3.N02<-NA
data$r_C3.N03<-NA
data$r_C3.N04<-NA
data$r_C3.N05<-NA
data$r_C3.N06<-NA
data$r_C3.N07<-NA
data$r_C3.N09<-NA
data$r_C3.N11<-NA
data$r_C3.N13<-NA
data$r_C5.N01<-NA
data$r_C5.N02<-NA
data$r_C5.N03<-NA
data$r_C5.N04<-NA
data$r_C5.N05<-NA
data$r_C5.N06<-NA
data$r_C5.N08<-NA
data$r_C5.N09<-NA
data$r_C5.N10<-NA
data$r_C5.N11<-NA
data$r_C5.N12<-NA
data$r_C6.N01<-NA
data$r_C6.N02<-NA
data$r_C6.N04<-NA
data$r_C6.N05<-NA
data$r_C6.N06<-NA
data$r_C6.N07<-NA
data$r_C6.N09<-NA
data$r_C6.N11<-NA
data$r_C6.N12<-NA
data$r_A1.I03<-NA
data$r_A1.I04<-NA
data$r_A1.I05<-NA
data$r_A1.I07<-NA
data$r_A1.I08<-NA
data$r_A1.I09<-NA
data$r_A1.I14<-NA
data$r_A1.I15<-NA
data$r_A2.I02<-NA
data$r_A2.I03<-NA
data$r_A2.I10<-NA
data$r_A2.I11<-NA
data$r_A2.I12<-NA
data$r_A2.I13<-NA
data$r_A4.I02<-NA
data$r_A4.I06<-NA
data$r_A4.I07<-NA
data$r_A4.I08<-NA
data$r_A4.I10<-NA
data$r_A4.I11<-NA
data$r_A4.I12<-NA
data$r_A4.I15<-NA
data$r_C3.I02<-NA
data$r_C3.I03<-NA
data$r_C3.I04<-NA
data$r_C3.I05<-NA
data$r_C3.I06<-NA
data$r_C3.I07<-NA
data$r_C3.I09<-NA
data$r_C3.I10<-NA
data$r_C3.I14<-NA
data$r_C5.I01<-NA
data$r_C5.I03<-NA
data$r_C5.I04<-NA
data$r_C5.I07<-NA
data$r_C5.I10<-NA
data$r_C5.I12<-NA
data$r_C5.I13<-NA
data$r_C6.I01<-NA
data$r_C6.I02<-NA
data$r_C6.I04<-NA
data$r_C6.I06<-NA
data$r_C6.I10<-NA
data$r_C6.I11<-NA

data$r_A1.N02[which(data$A1.N02%in% c(1,2))] <- 2
data$r_A1.N03[which(data$A1.N03%in% c(1,2))] <- 2
data$r_A1.N04[which(data$A1.N04%in% c(1,2))] <- 2
data$r_A1.N05[which(data$A1.N05%in% c(4))] <- 2
data$r_A1.N07[which(data$A1.N07%in% c(4))] <- 2
data$r_A1.N10[which(data$A1.N10%in% c(1,2))] <- 2
data$r_A1.N13[which(data$A1.N13%in% c(4))] <- 2
data$r_A2.N03[which(data$A2.N03%in% c(3,4))] <- 2
data$r_A2.N04[which(data$A2.N04%in% c(1,2))] <- 2
data$r_A2.N05[which(data$A2.N05%in% c(3,4))] <- 2
data$r_A2.N12[which(data$A2.N12%in% c(4))] <- 2
data$r_A2.N13[which(data$A2.N13%in% c(4))] <- 2
data$r_A4.N01[which(data$A4.N01%in% c(1,2))] <- 2
data$r_A4.N02[which(data$A4.N02%in% c(1,2))] <- 2
data$r_A4.N03[which(data$A4.N03%in% c(1,2))] <- 2
data$r_A4.N06[which(data$A4.N06%in% c(1))] <- 2
data$r_A4.N08[which(data$A4.N08%in% c(1))] <- 2
data$r_A4.N09[which(data$A4.N09%in% c(1))] <- 2
data$r_A4.N10[which(data$A4.N10%in% c(1,2))] <- 2
data$r_A4.N12[which(data$A4.N12%in% c(1))] <- 2
data$r_A4.N13[which(data$A4.N13%in% c(1))] <- 2
data$r_A4.N14[which(data$A4.N14%in% c(4))] <- 2
data$r_C3.N01[which(data$C3.N01%in% c(1,2))] <- 2
data$r_C3.N02[which(data$C3.N02%in% c(1,2))] <- 2
data$r_C3.N03[which(data$C3.N03%in% c(1))] <- 2
data$r_C3.N04[which(data$C3.N04%in% c(1,2))] <- 2
data$r_C3.N05[which(data$C3.N05%in% c(4))] <- 2
data$r_C3.N06[which(data$C3.N06%in% c(1,2))] <- 2
data$r_C3.N07[which(data$C3.N07%in% c(1,2))] <- 2
data$r_C3.N09[which(data$C3.N09%in% c(1,2))] <- 2
data$r_C3.N11[which(data$C3.N11%in% c(3,4))] <- 2
data$r_C3.N13[which(data$C3.N13%in% c(1))] <- 2
data$r_C5.N01[which(data$C5.N01%in% c(1,2))] <- 2
data$r_C5.N02[which(data$C5.N02%in% c(3,4))] <- 2
data$r_C5.N03[which(data$C5.N03%in% c(3,4))] <- 2
data$r_C5.N04[which(data$C5.N04%in% c(1,2))] <- 2
data$r_C5.N05[which(data$C5.N05%in% c(1))] <- 2
data$r_C5.N06[which(data$C5.N06%in% c(1,2))] <- 2
data$r_C5.N08[which(data$C5.N08%in% c(1,2))] <- 2
data$r_C5.N09[which(data$C5.N09%in% c(3,4))] <- 2
data$r_C5.N10[which(data$C5.N10%in% c(1,2))] <- 2
data$r_C5.N11[which(data$C5.N11%in% c(1,2))] <- 2
data$r_C5.N12[which(data$C5.N12%in% c(3,4))] <- 2
data$r_C6.N01[which(data$C6.N01%in% c(1,2))] <- 2
data$r_C6.N02[which(data$C6.N02%in% c(3,4))] <- 2
data$r_C6.N04[which(data$C6.N04%in% c(1,2))] <- 2
data$r_C6.N05[which(data$C6.N05%in% c(1,2))] <- 2
data$r_C6.N06[which(data$C6.N06%in% c(1,2))] <- 2
data$r_C6.N07[which(data$C6.N07%in% c(4))] <- 2
data$r_C6.N09[which(data$C6.N09%in% c(1,2))] <- 2
data$r_C6.N11[which(data$C6.N11%in% c(3,4))] <- 2
data$r_C6.N12[which(data$C6.N12%in% c(3,4))] <- 2
data$r_A1.I03[which(data$A1.I03%in% c(3,4))] <- 2
data$r_A1.I04[which(data$A1.I04%in% c(3,4))] <- 2
data$r_A1.I05[which(data$A1.I05%in% c(3,4))] <- 2
data$r_A1.I07[which(data$A1.I07%in% c(1,2))] <- 2
data$r_A1.I08[which(data$A1.I08%in% c(1,2))] <- 2
data$r_A1.I09[which(data$A1.I09%in% c(1,2))] <- 2
data$r_A1.I14[which(data$A1.I14%in% c(3,4))] <- 2
data$r_A1.I15[which(data$A1.I15%in% c(3,4))] <- 2
data$r_A2.I02[which(data$A2.I02%in% c(1,2))] <- 2
data$r_A2.I03[which(data$A2.I03%in% c(3,4))] <- 2
data$r_A2.I10[which(data$A2.I10%in% c(1,2))] <- 2
data$r_A2.I11[which(data$A2.I11%in% c(3,4))] <- 2
data$r_A2.I12[which(data$A2.I12%in% c(3,4))] <- 2
data$r_A2.I13[which(data$A2.I13%in% c(1,2))] <- 2
data$r_A4.I02[which(data$A4.I02%in% c(1))] <- 2
data$r_A4.I06[which(data$A4.I06%in% c(1,2))] <- 2
data$r_A4.I07[which(data$A4.I07%in% c(1,2))] <- 2
data$r_A4.I08[which(data$A4.I08%in% c(3,4))] <- 2
data$r_A4.I10[which(data$A4.I10%in% c(3,4))] <- 2
data$r_A4.I11[which(data$A4.I11%in% c(1,2))] <- 2
data$r_A4.I12[which(data$A4.I12%in% c(3,4))] <- 2
data$r_A4.I15[which(data$A4.I15%in% c(1,2))] <- 2
data$r_C3.I02[which(data$C3.I02%in% c(1,2))] <- 2
data$r_C3.I03[which(data$C3.I03%in% c(1,2))] <- 2
data$r_C3.I04[which(data$C3.I04%in% c(1))] <- 2
data$r_C3.I05[which(data$C3.I05%in% c(1))] <- 2
data$r_C3.I06[which(data$C3.I06%in% c(4))] <- 2
data$r_C3.I07[which(data$C3.I07%in% c(1,2))] <- 2
data$r_C3.I09[which(data$C3.I09%in% c(1))] <- 2
data$r_C3.I10[which(data$C3.I10%in% c(1,2))] <- 
data$r_C3.I14[which(data$C3.I14%in% c(1,2))] <- 2
data$r_C5.I01[which(data$C5.I01%in% c(1,2))] <- 2
data$r_C5.I03[which(data$C5.I03%in% c(1,2))] <- 2
data$r_C5.I04[which(data$C5.I04%in% c(3,4))] <- 2
data$r_C5.I07[which(data$C5.I07%in% c(1,2))] <- 2
data$r_C5.I10[which(data$C5.I10%in% c(3,4))] <- 2
data$r_C5.I12[which(data$C5.I12%in% c(3,4))] <- 2
data$r_C5.I13[which(data$C5.I13%in% c(1,2))] <- 2
data$r_C6.I01[which(data$C6.I01%in% c(3,4))] <- 2
data$r_C6.I02[which(data$C6.I02%in% c(2))] <- 2
data$r_C6.I04[which(data$C6.I04%in% c(1,2))] <- 2
data$r_C6.I06[which(data$C6.I06%in% c(1,2))] <- 2
data$r_C6.I10[which(data$C6.I10%in% c(3))] <- 2
data$r_C6.I11[which(data$C6.I11%in% c(1,2))] <- 2


data$r_A1.N05[which(data$A1.N05%in% c(3))] <- 1
data$r_A1.N07[which(data$A1.N07%in% c(3))] <- 1
data$r_A1.N13[which(data$A1.N13%in% c(3))] <- 1
data$r_A2.N12[which(data$A2.N12%in% c(3))] <- 1
data$r_A2.N13[which(data$A2.N13%in% c(3))] <- 1
data$r_A4.N06[which(data$A4.N06%in% c(2))] <- 1
data$r_A4.N08[which(data$A4.N08%in% c(2))] <- 1
data$r_A4.N09[which(data$A4.N09%in% c(2))] <- 1
data$r_A4.N12[which(data$A4.N12%in% c(2))] <- 1
data$r_A4.N13[which(data$A4.N13%in% c(2))] <- 1
data$r_A4.N14[which(data$A4.N14%in% c(3))] <- 1
data$r_C3.N03[which(data$C3.N03%in% c(2))] <- 1
data$r_C3.N05[which(data$C3.N05%in% c(3))] <- 1
data$r_C3.N13[which(data$C3.N13%in% c(2))] <- 1
data$r_C5.N05[which(data$C5.N05%in% c(2))] <- 1
data$r_C6.N07[which(data$C6.N07%in% c(3))] <- 1
data$r_A4.I02[which(data$A4.I02%in% c(2))] <- 1
data$r_C3.I04[which(data$C3.I04%in% c(2))] <- 1
data$r_C3.I05[which(data$C3.I05%in% c(2))] <- 1
data$r_C3.I06[which(data$C3.I06%in% c(3))] <- 1
data$r_C3.I09[which(data$C3.I09%in% c(2))] <- 1
data$r_C6.I02[which(data$C6.I02%in% c(1))] <- 1
data$r_C6.I10[which(data$C6.I10%in% c(4))] <- 1

data$r_A1.N02[which(data$A1.N02%in% c(3,4))] <- 0
data$r_A1.N03[which(data$A1.N03%in% c(3,4))] <- 0
data$r_A1.N04[which(data$A1.N04%in% c(3,4))] <- 0
data$r_A1.N05[which(data$A1.N05%in% c(1,2))] <- 0
data$r_A1.N07[which(data$A1.N07%in% c(1,2))] <- 0
data$r_A1.N10[which(data$A1.N10%in% c(3,4))] <- 0
data$r_A1.N13[which(data$A1.N13%in% c(1,2))] <- 0
data$r_A2.N03[which(data$A2.N03%in% c(1,2))] <- 0
data$r_A2.N04[which(data$A2.N04%in% c(3,4))] <- 0
data$r_A2.N05[which(data$A2.N05%in% c(1,2))] <- 0
data$r_A2.N12[which(data$A2.N12%in% c(1,2))] <- 0
data$r_A2.N13[which(data$A2.N13%in% c(1,2))] <- 0
data$r_A4.N01[which(data$A4.N01%in% c(3,4))] <- 0
data$r_A4.N02[which(data$A4.N02%in% c(3,4))] <- 0
data$r_A4.N03[which(data$A4.N03%in% c(3,4))] <- 0
data$r_A4.N06[which(data$A4.N06%in% c(3,4))] <- 0
data$r_A4.N08[which(data$A4.N08%in% c(3,4))] <- 0
data$r_A4.N09[which(data$A4.N09%in% c(3,4))] <- 0
data$r_A4.N10[which(data$A4.N10%in% c(3,4))] <- 0
data$r_A4.N12[which(data$A4.N12%in% c(3,4))] <- 0
data$r_A4.N13[which(data$A4.N13%in% c(3,4))] <- 0
data$r_A4.N14[which(data$A4.N14%in% c(1,2))] <- 0
data$r_C3.N01[which(data$C3.N01%in% c(3,4))] <- 0
data$r_C3.N02[which(data$C3.N02%in% c(3,4))] <- 0
data$r_C3.N03[which(data$C3.N03%in% c(3,4))] <- 0
data$r_C3.N04[which(data$C3.N04%in% c(3,4))] <- 0
data$r_C3.N05[which(data$C3.N05%in% c(1,2))] <- 0
data$r_C3.N06[which(data$C3.N06%in% c(3,4))] <- 0
data$r_C3.N07[which(data$C3.N07%in% c(3,4))] <- 0
data$r_C3.N09[which(data$C3.N09%in% c(3,4))] <- 0
data$r_C3.N11[which(data$C3.N11%in% c(1,2))] <- 0
data$r_C3.N13[which(data$C3.N13%in% c(3,4))] <- 0
data$r_C5.N01[which(data$C5.N01%in% c(3,4))] <- 0
data$r_C5.N02[which(data$C5.N02%in% c(1,2))] <- 0
data$r_C5.N03[which(data$C5.N03%in% c(1,2))] <- 0
data$r_C5.N04[which(data$C5.N04%in% c(3,4))] <- 0
data$r_C5.N05[which(data$C5.N05%in% c(3,4))] <- 0
data$r_C5.N06[which(data$C5.N06%in% c(3,4))] <- 0
data$r_C5.N08[which(data$C5.N08%in% c(3,4))] <- 0
data$r_C5.N09[which(data$C5.N09%in% c(1,2))] <- 0
data$r_C5.N10[which(data$C5.N10%in% c(3,4))] <- 0
data$r_C5.N11[which(data$C5.N11%in% c(3,4))] <- 0
data$r_C5.N12[which(data$C5.N12%in% c(1,2))] <- 0
data$r_C6.N01[which(data$C6.N01%in% c(3,4))] <- 0
data$r_C6.N02[which(data$C6.N02%in% c(1,2))] <- 0
data$r_C6.N04[which(data$C6.N04%in% c(3,4))] <- 0
data$r_C6.N05[which(data$C6.N05%in% c(3,4))] <- 0
data$r_C6.N06[which(data$C6.N06%in% c(3,4))] <- 0
data$r_C6.N07[which(data$C6.N07%in% c(1,2))] <- 0
data$r_C6.N09[which(data$C6.N09%in% c(3,4))] <- 0
data$r_C6.N11[which(data$C6.N11%in% c(1,2))] <- 0
data$r_C6.N12[which(data$C6.N12%in% c(1,2))] <- 0
data$r_A1.I03[which(data$A1.I03%in% c(1,2))] <- 0
data$r_A1.I04[which(data$A1.I04%in% c(1,2))] <- 0
data$r_A1.I05[which(data$A1.I05%in% c(1,2))] <- 0
data$r_A1.I07[which(data$A1.I07%in% c(3,4))] <- 0
data$r_A1.I08[which(data$A1.I08%in% c(3,4))] <- 0
data$r_A1.I09[which(data$A1.I09%in% c(3,4))] <- 0
data$r_A1.I14[which(data$A1.I14%in% c(1,2))] <- 0
data$r_A1.I15[which(data$A1.I15%in% c(1,2))] <- 0
data$r_A2.I02[which(data$A2.I02%in% c(3,4))] <- 0
data$r_A2.I03[which(data$A2.I03%in% c(1,2))] <- 0
data$r_A2.I10[which(data$A2.I10%in% c(3,4))] <- 0
data$r_A2.I11[which(data$A2.I11%in% c(1,2))] <- 0
data$r_A2.I12[which(data$A2.I12%in% c(1,2))] <- 0
data$r_A2.I13[which(data$A2.I13%in% c(3,4))] <- 0
data$r_A4.I02[which(data$A4.I02%in% c(3,4))] <- 0
data$r_A4.I06[which(data$A4.I06%in% c(3,4))] <- 0
data$r_A4.I07[which(data$A4.I07%in% c(3,4))] <- 0
data$r_A4.I08[which(data$A4.I08%in% c(1,2))] <- 0
data$r_A4.I10[which(data$A4.I10%in% c(1,2))] <- 0
data$r_A4.I11[which(data$A4.I11%in% c(3,4))] <- 0
data$r_A4.I12[which(data$A4.I12%in% c(1,2))] <- 0
data$r_A4.I15[which(data$A4.I15%in% c(3,4))] <- 0
data$r_C3.I02[which(data$C3.I02%in% c(3,4))] <- 0
data$r_C3.I03[which(data$C3.I03%in% c(3,4))] <- 0
data$r_C3.I04[which(data$C3.I04%in% c(3,4))] <- 0
data$r_C3.I05[which(data$C3.I05%in% c(3,4))] <- 0
data$r_C3.I06[which(data$C3.I06%in% c(1,2))] <- 0
data$r_C3.I07[which(data$C3.I07%in% c(3,4))] <- 0
data$r_C3.I09[which(data$C3.I09%in% c(3,4))] <- 0
data$r_C3.I10[which(data$C3.I10%in% c(3,4))] <- 0
data$r_C3.I14[which(data$C3.I14%in% c(3,4))] <- 0
data$r_C5.I01[which(data$C5.I01%in% c(3,4))] <- 0
data$r_C5.I03[which(data$C5.I03%in% c(3,4))] <- 0
data$r_C5.I04[which(data$C5.I04%in% c(1,2))] <- 0
data$r_C5.I07[which(data$C5.I07%in% c(3,4))] <- 0
data$r_C5.I10[which(data$C5.I10%in% c(1,2))] <- 0
data$r_C5.I12[which(data$C5.I12%in% c(1,2))] <- 0
data$r_C5.I13[which(data$C5.I13%in% c(3,4))] <- 0
data$r_C6.I01[which(data$C6.I01%in% c(1,2))] <- 0
data$r_C6.I02[which(data$C6.I02%in% c(3,4))] <- 0
data$r_C6.I04[which(data$C6.I04%in% c(3,4))] <- 0
data$r_C6.I06[which(data$C6.I06%in% c(3,4))] <- 0
data$r_C6.I10[which(data$C6.I10%in% c(1,2))] <- 0
data$r_C6.I11[which(data$C6.I11%in% c(3,4))] <- 0

###### Umkodierung der Items CK #####
#data$R_CK01 <- NA
data$R_CK02 <- NA
data$R_CK03 <- NA
data$R_CK04 <- NA
#data$R_CK06 <- NA
data$R_CK07 <- NA
data$R_CK08 <- NA
data$R_CK09 <- NA
data$R_CK10 <- NA
data$R_CK11 <- NA
data$R_CK12 <- NA
data$R_CK13 <- NA
data$R_CK14 <- NA
#data$R_CK15 <- NA
data$R_CK16 <- NA
data$R_CK17 <- NA
data$R_CK18 <- NA

#data$R_CK01[which(data$CK01==levels(data$CK01)[4])] <- 1
#data$R_CK01[which(data$CK01!=levels(data$CK01)[4])] <- 0

data$R_CK02[which(data$CK02==levels(data$CK02)[4])] <- 1
data$R_CK02[which(data$CK02!=levels(data$CK02)[4])] <- 0

data$R_CK03[which(data$CK03==levels(data$CK03)[4])] <- 1
data$R_CK03[which(data$CK03!=levels(data$CK03)[4])] <- 0

data$R_CK04[which(data$CK04==levels(data$CK04)[4])] <- 1
data$R_CK04[which(data$CK04!=levels(data$CK04)[4])] <- 0

#data$R_CK05[which(data$CK05==levels(data$CK05)[8] | data$CK05==levels(data$CK05)[1] |data$CK05==levels(data$CK05)[2] | data$CK05==levels(data$CK05)[3])] <- 1
#data$R_CK05[which(data$CK05!=levels(data$CK05)[8] | data$CK05==levels(data$CK05)[1] |data$CK05==levels(data$CK05)[2] | data$CK05==levels(data$CK05)[3])] <- 0

#data$R_CK06[which(data$CK06==levels(data$CK06)[1])] <- 1
#data$R_CK06[which(data$CK06!=levels(data$CK06)[1])] <- 0

data$R_CK07[which(data$CK07==levels(data$CK07)[2])] <- 1
data$R_CK07[which(data$CK07!=levels(data$CK07)[2])] <- 0

data$R_CK08[which(data$CK08==levels(data$CK08)[3])] <- 1
data$R_CK08[which(data$CK08!=levels(data$CK08)[3])] <- 0

data$R_CK09[which(data$CK09==levels(data$CK09)[1])] <- 1
data$R_CK09[which(data$CK09!=levels(data$CK09)[1])] <- 0

data$R_CK10[which(data$CK10==levels(data$CK10)[3])] <- 1
data$R_CK10[which(data$CK10!=levels(data$CK10)[3])] <- 0

data$R_CK11[which(data$CK11==levels(data$CK11)[2])] <- 1
data$R_CK11[which(data$CK11!=levels(data$CK11)[2])] <- 0

data$R_CK12[which(data$CK12==levels(data$CK12)[3])] <- 1
data$R_CK12[which(data$CK12!=levels(data$CK12)[3])] <- 0

data$R_CK13[which(data$CK13==levels(data$CK13)[3])] <- 1
data$R_CK13[which(data$CK13!=levels(data$CK13)[3])] <- 0

data$R_CK14[which(data$CK14==levels(data$CK14)[1])] <- 1
data$R_CK14[which(data$CK14!=levels(data$CK14)[1])] <- 0

#data$R_CK15[which(data$CK15==levels(data$CK15)[3])] <- 1
#data$R_CK15[which(data$CK15!=levels(data$CK15)[3])] <- 0

data$R_CK16[which(data$CK16==levels(data$CK16)[2])] <- 1
data$R_CK16[which(data$CK16!=levels(data$CK16)[2])] <- 0

data$R_CK17[which(data$CK17==levels(data$CK17)[2])] <- 1
data$R_CK17[which(data$CK17!=levels(data$CK17)[2])] <- 0

data$R_CK18[which(data$CK18==levels(data$CK18)[1])] <- 1
data$R_CK18[which(data$CK18!=levels(data$CK18)[1])] <- 0


###### Itemanalyse (Kapitel 5) ######
data.items <- data[,187:282]

iac.exp <- alpha(data.items)
capture.output(iac.exp,file = "alpha1.txt")
trennschärfen.exp <- iac.exp$item.stats$r.drop
names(data.items)[which(trennschärfen.exp==min(trennschärfen.exp))]
t <- trennschärfen.exp[which(trennschärfen.exp==min(trennschärfen.exp))]
n <- which(trennschärfen.exp==min(trennschärfen.exp))
i=1
item.name <- NA
item.tr <- NA
## Item rausschmeißen
while(t<0.15){
  data.items <- data.items[,-n]
  
  iac.exp <- alpha(data.items)
  trennschärfen.exp <- iac.exp$item.stats$r.drop
  item.name[i] <- names(data.items)[which(trennschärfen.exp==min(trennschärfen.exp))]
  item.tr[i] <- trennschärfen.exp[which(trennschärfen.exp==min(trennschärfen.exp))]
  i=i+1
  t <- trennschärfen.exp[which(trennschärfen.exp==min(trennschärfen.exp))]
  n <- which(trennschärfen.exp==min(trennschärfen.exp))
}

###### Items nach Itemanalyse aus Datensatz entfernen
data <- data[,-which(names(data)%in%item.name)]

###### Score-Werte pU & CK  #####
# Skalierung an maximaler Punktezahl
data$Score.pU <- 100*rowSums(data[,187:246])/(2*length(names(data)[187:246]))
data$Score.pU.N <- 100*rowSums(data[,187:223])/(2*length(names(data)[187:223]))
data$Score.pU.I <- 100*rowSums(data[,224:246])/(2*length(names(data)[224:246]))
data$Score.pU.A <- 100*rowSums(data[,c(187:203,224:235)])/(2*length(names(data)[c(187:203,224:235)]))
data$Score.pU.CR <- 100*rowSums(data[,c(204:223,236:246)])/(2*length(names(data)[c(204:223,236:246)]))

data$Score.CK <- 100*rowSums(data[,247:260])/length(names(data)[247:260])
data$Score.CK.CR <- 100*rowSums(data[,247:252])/length(names(data)[247:252])
data$Score.CK.A <- 100*rowSums(data[,253:260])/length(names(data)[253:260])

###### Reliabilität (Kapitel 5) ######
data.N <- data.items[,which(names(data.items)%in%names(data.items[grep("N", names(data.items))]))]
data.I <- data.items[,which(names(data.items)%in%names(data.items[grep("I", names(data.items))]))]

alpha2 <- alpha(data.items)
capture.output(alpha2,file = "alpha2.txt")
alphaN <- alpha(data.N)
capture.output(alphaN,file = "alphaN.txt")
alphaI <- alpha(data.I)
capture.output(alphaI,file = "alphaI.txt")


###### Vergleich Bachelor-Master (Kapitel 5) #####
data$SD06_01[which(data$SD06_01%in%c(1,2,4) & data$SD03=="Nordrhein-Westfalen")] <- 20 ### NRW-ler anpassen

data$Gruppe <- NA
data$Gruppe[which(data$SD06_01<6.5)] <- 0
data$Gruppe[which(data$SD06_01>6.5)] <- 1

G1 <- data$Score.pU[which(data$Gruppe==0)]
G2 <- data$Score.pU[which(data$Gruppe==1)]

describe(G1)
describe(G2)

shapiro.test(data$Score.pU[which(data$Gruppe==0)])
shapiro.test(data$Score.pU[which(data$Gruppe==1)])
library(car)
leveneTest(data$Score.pU, data$Gruppe)

t.test(data$Score.pU~data$Gruppe, var.equal = T, alternative="less", conf.int = T)
wilcox.test(data$Score.pU~data$Gruppe, alternative = "less", exact=T, conf.level = .95, conf.int = T)

library(effectsize)
Gruppe1 <- data$Score.pU[which(data$Gruppe==0)]
Gruppe2 <- data$Score.pU[which(data$Gruppe==1)]

cohens_d(Gruppe2, Gruppe1)

library(rcompanion)
wilcoxonR(data$Score.pU, data$Gruppe)

library(ggplot2)
plot <- data.frame(
  Score = c(data$Score.pU[which(data$Gruppe==0)], data$Score.pU[which(data$Gruppe==1)]),
  Gruppe = c(rep("Bachelor", length(data$Score.pU[which(data$Gruppe==0)])), rep("Master", length(data$Score.pU[which(data$Gruppe==1)])))
)
ggplot(data = plot, aes(x=Gruppe, y=Score, fill=Gruppe))+
  geom_boxplot()+ 
  geom_jitter(shape=16, position=position_jitter(0.1), alpha = .3)+
  scale_fill_manual(values=c("steelblue", "indianred"))

#### Weitere Untersuchungen
## Fachwissen
data$Gruppe[which(data$SD06_01<6.5)] <- 0
data$Gruppe[which(data$SD06_01>6.5)] <- 1

shapiro.test(data$Score.CK[which(data$Gruppe==0)])
shapiro.test(data$Score.CK[which(data$Gruppe==1)])
library(car)
leveneTest(data$Score.CK, data$Gruppe)

t.test(data$Score.CK~data$Gruppe, var.equal = T, alternative="less", conf.int = T)


## Unterrichtserfahrung 
shapiro.test(data$SD08_01[which(data$Gruppe==0)])
shapiro.test(data$SD08_01[which(data$Gruppe==1)])
library(car)
leveneTest(data$SD08_01, data$Gruppe)

t.test(data$SD08_01~data$Gruppe, var.equal = F, alternative="less", conf.int = T)
wilcoxonR(data$SD08_01, data$Gruppe)

###### Vergleich Novizen Experten (Kapitel 5) ######
data$Gruppe[which(data$Status%in%c("LP", "SemLP", "Prom"))] <- 0
data$Gruppe[which(data$Status=="Stud")] <- 1

G1 <- data$Score.pU[which(data$Gruppe==0)]
G2 <- data$Score.pU[which(data$Gruppe==1)]

describe(G1)
describe(G2)

shapiro.test(G1)
shapiro.test(G2)
leveneTest(data$Score.pU, data$Gruppe)

t.test(data$Score.pU~data$Gruppe, var.equal = T, alternative="greater")
wilcox.test(G1,G2, alternative = "greater", exact=F, conf.level = .95, conf.int = T)

library(effectsize)
Gruppe1 <- data$Score.pU[which(data$Status%in%c("LP", "SemLP", "Prom"))]
Gruppe2 <- data$Score.pU[which(data$Status=="Stud")]

cohens_d(Gruppe1, Gruppe2)

wilcoxonR(data$Score.pU, data$Gruppe)

library(ggplot2)
library(ggsignif)
plot <- data.frame(
  Score = c(data$Score.pU[which(data$Gruppe==0)], data$Score.pU[which(data$Gruppe==1)]),
  Gruppe = c(rep("Expert:innen", length(data$Score.pU[which(data$Gruppe==0)])), rep("Studierende", length(data$Score.pU[which(data$Gruppe==1)])))
)
ggplot(data = plot, aes(x=Gruppe, y=Score, fill=Gruppe))+
  geom_boxplot()+ 
  geom_jitter(shape=16, position=position_jitter(0.1), alpha = .3)+
  scale_fill_manual(values=c("steelblue", "indianred"))+
  ylim(30,140)+
  geom_signif(
    y_position = 133, xmin = 1, xmax = 2, ymax=9.5,
    annotation = "***", tip_length = 0.02
  )
###### Unterrichtswahrnehmung und Tiefe der Kommentare (Kapitel 5) #####
### Deskriptive Statistik Ebene
describe(data$rel_Beschreiben)
describe(data$Score.pU[which(!is.na(data$rel_Beschreiben))])
describe(data$Score.Ebene)
describe(data$Score.pU)

### Vergleich pU - Ebene
shapiro.test(data$rel_Beschreiben)
shapiro.test(data$Score.pU)
shapiro.test(data$Score.Ebene)

cor.test(data$Score.pU, data$rel_Beschreiben, method = "spearman")
cor.test(data$Score.pU, data$Score.Ebene, method = "spearman")
cor.test(data$Score.pU, data$Score.Ebene.oB, method = "spearman")

summary(lm(data$Score.pU~data$rel_Beschreiben))
ggplot(data = data, aes(y=Score.pU, x=rel_Beschreiben)) + 
  geom_point(alpha=.5) + 
  geom_smooth(method=lm, se=FALSE, col="steelblue", size=1)+
  xlab("relative Häufigkeit Beschreiben")+
  ylab("Score pU")

summary(lm(data$Score.pU~data$Score.Ebene))
ggplot(data = data, aes(y=Score.pU, x=Score.Ebene)) + 
  geom_point(alpha=.5) + 
  geom_smooth(method=lm, se=FALSE, col="steelblue", size=1)+
  xlab("Score Kommentare")+
  ylab("Score pU")

#ohne Ausreißer
subset <- data[-which(data$Score.Ebene>300),]
cor.test(subset$Score.pU, subset$Score.Ebene, method = "spearman")

summary(lm(subset$Score.pU~subset$Score.Ebene))
ggplot(data = subset, aes(y=Score.pU, x=Score.Ebene)) + 
  geom_point(alpha=.5) + 
  geom_smooth(method=lm, se=FALSE, col="steelblue", size=1)+
  xlab("Score Kommentare")+
  ylab("Score pU")

###### Schwierigkeitsniveaus (Kapitel 5) #####
G1 <- data$Score.pU.N
G2 <- data$Score.pU.I
Scores <- c(G1,G2)
Gruppe <- c(rep(1, length(G1)), rep(2, length(G2)))

describe(G1)
describe(G2)

shapiro.test(G1)
shapiro.test(G2)
leveneTest(Scores, Gruppe)

t.test(Scores~Gruppe, var.equal = T, alternative="greater")
wilcox.test(G1,G2, paired = T ,alternative = "greater", exact=F, conf.level = .95, conf.int = T)

library(effectsize)
cohens_d(G1, G2)

wilcoxonR(Scores, Gruppe)


#### LMM
data$Gruppe[which(data$Status%in%c("SemLP", "Prom", "LP"))] <- 2
data$Gruppe[which(data$Status%in%c("Stud"))] <- 1

describeBy(data$Score.pU.N, data$Gruppe)
describeBy(data$Score.pU.I, data$Gruppe)

Score <- NA
Score[1] <- round(mean(data$Score.pU.N[which(data$Gruppe==1)]),2)
Score[2] <- round(mean(data$Score.pU.N[which(data$Gruppe==2)]),2)
Score[3] <- round(mean(data$Score.pU.I[which(data$Gruppe==1)]),2)
Score[4] <- round(mean(data$Score.pU.I[which(data$Gruppe==2)]),2)

Gruppe <- c("Studierende","Expert:innen","Studierende","Expert:innen")
Facette <- c("S1","S1","S2","S2")

level <- data.frame(Score,Gruppe, Facette)
library(ggplot2)
ggplot(data=level, aes(x=Facette, y=Score, group=Gruppe))+
  geom_line(aes(col=Gruppe))+
  geom_point(aes(col=Gruppe))+
  scale_x_discrete(labels = c("Noticing", "Interpretation"))+
  scale_color_manual(labels = c("Expert:innen", "Studierende"), values = c("steelblue", "indianred"))+
  labs(y = "Score pU", color = "")


require(lme4)
require(lmerTest)
require(multilevel)

data$Gruppe[which(data$Status%in%c("SemLP", "Prom", "LP"))] <- 1
data$Gruppe[which(data$Status%in%c("Stud"))] <- 2

niveau <- data.frame(
  Ergebnis = c(data$Score.pU.N, data$Score.pU.I),
  Variable = c(rep("Noticing", length(data$Score.pU.N)), rep("Interpretation", length(data$Score.pU.I))),
  Person = c(data$code, data$code),
  Gruppe = data$Gruppe
)

mod1 = lmer(niveau$Ergebnis ~ 1 + niveau$Gruppe*niveau$Variable + (1 | niveau$Person))
summary(mod1)

library(MuMIn)
r.squaredGLMM(mod1)

###### Themenspezifität (Kapitel 6) #####
# Unterschied in Themen pU
# 29 Items pU.A, 31 pU.CR
# 6 CK.CR, 8 CK.A
subset <- data[which(data$Studie=="HS"),]

shapiro.test(subset$Score.pU.A)
shapiro.test(subset$Score.pU.CR)

describe(subset$Score.pU.A)
describe(subset$Score.pU.CR)

Score.pU<- c(subset$Score.pU.A, subset$Score.pU.CR)
Thema <- c(rep("A", length(subset$Score.pU.A)), rep("C", length(subset$Score.pU.CR)))
leveneTest(Score.pU, Thema)

t.test(Score.pU~Thema, var.equal = T, sonf.int=T)
boxplot(subset$Score.pU.A, subset$Score.pU.CR)
wilcox.test(subset$Score.pU.A, subset$Score.pU.CR, paired = T, exact=T, conf.level = .95, conf.int = T)
mean(subset$Score.pU.A)
mean(subset$Score.pU.CR)

library(ggplot2)
plot <- data.frame(
  Score = c(subset$Score.pU.A, subset$Score.pU.CR),
  Thema = c(rep("Atombau", length(subset$Score.pU.A)), rep("Chemische Reaktion", length(subset$Score.pU.CR)))
)
ggplot(data = plot, aes(x=Thema, y=Score, fill=Thema))+
  geom_boxplot()+ 
  geom_jitter(shape=16, position=position_jitter(0.1), alpha = .3)+
  scale_fill_manual(values=c("steelblue", "indianred"))+
  ylab("Score pU")

library(effectsize)
Atombau <- subset$Score.pU.A
ChemischeReaktion <- subset$Score.pU.CR

cohens_d(Atombau, ChemischeReaktion)

library(rcompanion)
wilcoxonPairedR(plot$Score, plot$Thema)

#Unterschied in Themen CK
describe(subset$Score.CK.A)
describe(subset$Score.CK.CR)

Score.CK <- c(subset$Score.CK.A, subset$Score.CK.CR)
leveneTest(Score.CK, Thema)

shapiro.test(data$Score.CK.A)
shapiro.test(data$Score.CK.CR)

t.test(Score.CK~Thema, var.equal = T, conf.int=T)
wilcox.test(subset$Score.CK.A, subset$Score.CK.CR, paired = T, conf.level = .95, conf.int = T)
mean(subset$Score.CK.A)
mean(subset$Score.CK.CR)

library(ggplot2)
library(ggsignif)
plot <- data.frame(
  Score = c(subset$Score.CK.A, subset$Score.CK.CR),
  Thema = c(rep("Atombau", length(subset$Score.CK.A)), rep("Chemische Reaktion", length(subset$Score.CK.CR)))
)

ggplot(data = plot, aes(x=Thema, y=Score, fill=Thema))+
  geom_boxplot()+ 
  geom_jitter(shape=16, position=position_jitter(0.1), alpha = .3)+
  scale_fill_manual(values=c("steelblue", "indianred"))+
  ylim(-0.1,140)+
  geom_signif(
    y_position = 125, xmin = 1, xmax = 2, 
    annotation = "**", tip_length = 0.02
  )+
  ylab("Score CK")

library(effectsize)
Atombau <- subset$Score.CK.A
ChemischeReaktion <- subset$Score.CK.CR

cohens_d(Atombau, ChemischeReaktion)

library(rcompanion)
wilcoxonPairedR(plot$Score, plot$Thema)

# lineare Regression --> Varianzaufklärung durch CK
summary(lm(subset$Score.pU.A~subset$Score.CK.A))
ggplot(data = subset, aes(x=Score.CK.A, y=Score.pU.A)) + 
  geom_point(alpha=.5) + 
  geom_smooth(method=lm, se=FALSE, col="steelblue", size=1)+
  xlab("Score CK")+
  ylab("Score pU")

summary(lm(subset$Score.pU.CR~subset$Score.CK.CR))
ggplot(data = subset, aes(x=Score.CK.CR, y=Score.pU.CR)) + 
  geom_point(alpha=.5) + 
  geom_smooth(method=lm, se=FALSE, col="steelblue", size=1)+
  xlab("Score CK")+
  ylab("Score pU")

###### weitere Ergebnisse: Einflussfaktoren pU (Kapitel 6) ######
subset <- data[which(data$Studie=="HS"),]

require(lme4)
require(lmerTest)
require(multilevel)
describe(subset$Score.pU.A)
describe(subset$Score.pU.CR)
describe(subset$Score.CK.A)
describe(subset$Score.CK.CR)
describe(subset$SD07_01)

einfluss <- data.frame(
  Ergebnis = c(subset$Score.pU.A, subset$Score.pU.CR),
  CK = c(subset$Score.CK.A, subset$Score.CK.CR),
  Thema = c(rep("Atombau", length(subset$Score.pU.A)), rep("Chemische Reaktion", length(subset$Score.pU.CR))),
  Person = c(subset$code, subset$code),
  Erfahrung = c(subset$SD07_01, subset$SD07_01))
einfluss <- einfluss[-which(is.na(einfluss$Erfahrung)),]


mod1 = lmer(einfluss$Ergebnis ~ 1 + einfluss$Thema*einfluss$CK + einfluss$Erfahrung + (1 | einfluss$Person))
summary(mod1)

library(MuMIn)
r.squaredGLMM(mod1)

###### Vergleich CK - Art (Kapitel 6) ######
describe(data$rel_KF[which(!is.na(data$Score.CK))])
describe(data$rel_LU[which(!is.na(data$Score.CK))])
describe(data$rel_SA[which(!is.na(data$Score.CK))])
describe(data$rel_AB[which(!is.na(data$Score.CK))])

shapiro.test(data$Score.CK[which(!is.na(data$Score.CK))])
shapiro.test(data$rel_KF[which(!is.na(data$Score.CK))])
shapiro.test(data$rel_LU[which(!is.na(data$Score.CK))])
shapiro.test(data$rel_SA[which(!is.na(data$Score.CK))])
shapiro.test(data$rel_AB[which(!is.na(data$Score.CK))])

cor.test(data$Score.CK, data$rel_KF, method = "spearman")
cor.test(data$Score.CK, data$rel_LU, method = "spearman")
cor.test(data$Score.CK, data$rel_SA, method = "spearman")
cor.test(data$Score.CK, data$rel_AB, method = "spearman")
cor.test(data$Score.CK, data$rel_SA+data$rel_LU, method = "spearman")
cor.test(data$Score.CK, data$rel_KF+data$rel_AB, method = "spearman")


