library(stringr)
library(tidyr)
library(psych)

###### Datenaufbereitung ######
data <- read.csv2("/Users/nadineboele/Desktop/Promotion/Expertenauswertung 2/Experten2.csv")
data <- data[-1,]
data$code <- paste(data$SD01_01, data$SD01_02, data$SD01_03, sep="")
data <- data[order(data$QUESTNNR),]

#### Variablennamen anpassen
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

#### Umwandeln der Zeiten in numeric
pos.char <- grep("Z", names(data))
for(i in pos.char){
  data[,i] <- as.numeric(as.character(data[,i]))
  
}


#### Zusammenfassen der beiden Themen
num.c <- c(grep("C3.", names(data)), grep("C5.", names(data)), grep("C6.", names(data)))
dup <- which(duplicated(data$code)==T)

pos1 <- c()
pos2 <- c()
for(i in dup){
  pos1[i] <- which(data$code==data$code[i]) # Warnings sind ok ;-)
  pos2[i] <- i
}
pos1 <- pos1[!is.na(pos1)]
pos2 <- pos2[!is.na(pos2)]

data[pos1,num.c] <- data[pos2,num.c]
data <- data[-pos2,]
data <- data[pos1,]
### Auswahl der Variablen mit N und I
pos1 <- c(grep("TIME", names(data)),
          grep("START", names(data)),
          grep("LAST", names(data)),
          grep("PAGE", names(data)),
          grep("REF", names(data)),
          grep("CASE", names(data)),
          grep("MODE", names(data)),
          grep("SD01_", names(data)),
          grep("QUESTNNR", names(data)),
          grep("MAILSENT", names(data)),
          grep("FINISHED", names(data)),
          grep("MISS", names(data)),
          grep("SERIAL", names(data)),
          grep("Q_VIEWER", names(data)))

data <- data[,-pos1]

pos <- c(grep(".N", names(data)),grep(".I", names(data)))
names(data)[pos]

###### Vergleich der Expertendaten #####
### Reduzieren des Datensatzes auf Itemvariablen
data.items <- data[,pos]

### Einlesen eigene Daten
Autor <- read.csv2("/Users/nadineboele/Desktop/Promotion/Expertenauswertung 2/eigene Daten.csv", header = T)
names(Autor) <- names(data.items)
data.items <- rbind(data.items, Autor)


### Anzahl der jeweilgen Antwort 
num.data <- data.frame()

for(i in 1:ncol(data.items)){
  num.data[1,i] <- length(which(data.items[,i]==1))
  num.data[2,i] <- length(which(data.items[,i]==2))
  num.data[3,i] <- length(which(data.items[,i]==3))
  num.data[4,i] <- length(which(data.items[,i]==4))
}
names(num.data) <- names(data.items)

### Relative Häufigkeiten der jeweiligen Antworten
rel.data <- num.data/nrow(data.items)

### Zusammenfassen in Tendenz
tendenz.positiv <- rel.data[1,] + rel.data[2,]
tendenz.negativ <- rel.data[3,] + rel.data[4,]

rel.tendenz.data <- rbind(tendenz.positiv, tendenz.negativ)

### Größte prozentuale Übereinstimmung pro Variable
max.rel.tendenz <- apply(rel.tendenz.data, 2, max)


###### Festlegung der richtigen Antwort (absolute Mehrheit) #####
verwerfen <- rel.data[,which(max.rel.tendenz<0.75)]
verbleibend <- rel.data[,which(max.rel.tendenz>=0.75)]

right.answer<-c(rep(NA, 108))
perc.answer <- c(rep(NA, 108))
for (i in 1:ncol(verbleibend)) {
  if(length(which(verbleibend[,i]==max(verbleibend[,i])))==1){
    right.answer[i] <- which(verbleibend[,i]==max(verbleibend[,i]))
    perc.answer[i] <- verbleibend[which(verbleibend[,i]==max(verbleibend[,i])),i]
  }
  else{
    right.answer[i] <-paste(which(verbleibend[,i]==max(verbleibend[,i]))[1], which(verbleibend[,i]==max(verbleibend[,i]))[2], sep=", ")
    perc.answer[i] <- max(verbleibend[,i])
  }
}

right.data <- data.frame(names(verbleibend),right.answer, perc.answer)

###### Übereinstimmung Krippendorff #####
### Einzelwerte
data_exp <- read.csv2("/Users/nadineboele/Desktop/Promotion/Expertenauswertung 2/EXP Items.csv", header = T)
pos.verbl <- which(max.rel.tendenz>=0.75)
data_kripp <- data_exp[,pos.verbl]

data_kripp <- data.frame(t(data_kripp))
library(DescTools)
data_kripp <- t(as.matrix(data_kripp))
KrippAlpha(data_kripp, method = "ordinal")

###### Trennschärfe ######
iac <- alpha(data_icc)
mean <- iac$item.stats$mean
min <- iac$item.stats$sd
trennschärfe <- iac$item.stats$r.drop
