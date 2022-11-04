#Load files
data<-read.csv2("C:/Users/su/Downloads/GCMS/Oct-2022/CoTXSS-batch1.csv", sep = ",", header = F)
data2<-read.csv2("C:/Users/su/Downloads/GCMS/Oct-2022/CoTXSS-batch2.csv", sep = ",", header = F)
data<-rbind(data, data2)
rm(data2)
#Retain useful columns only
data_useful<-data[,c(2,4,7)]
dim(data_useful)

data_useful<-data_useful[!apply(data_useful == "", 1, all), ] #remove empty rows
dim(data_useful)

library(misty)
dup<-df.duplicated(data_useful, V2, V4, V7) #see duplicated rows
data_useful<-df.unique(data_useful, V2, V4, V7) #retain uniques rows
library(tidyverse)
a<-as.numeric(row.names(data_useful[str_detect(data_useful$V2, "Peak"),]))
data_useful<-data_useful[-a,]

#Add sample name to col names
for (row in 1:nrow(data_useful)){
  if(data_useful$V4[row]==""){
    data_useful$V4[row] <- data_useful$V2[row]
  }
  if(data_useful$V7[row]==""){
    data_useful$V7[row] <- data_useful$V2[row]
  }
}
data_useful<-data_useful[,-1]

#Extract sample names
sample_names<-as.data.frame(data_useful[str_detect(data_useful$V4, "EI"),][,1])
colnames(sample_names)<-"Samples"

#Remove blank
##for loop might be needed if multiple blanks are present

blank<-sample_names[str_detect(sample_names$Samples, "Blank"),]
start <- which(data_useful$V4 == blank) #which row blank is at

##the next string after blank
afterblank<-sample_names[which(sample_names == blank)+1,]
end<-which(data_useful$V4 == afterblank)

##Removal
data_useful<-data_useful[-(start:(end-1)),]
sample_names<-as.data.frame(sample_names[-which(sample_names == blank),])
colnames(sample_names)<-"Samples"
#String_split
snss1<-as.data.frame(str_split_fixed(sample_names$Samples, "-", 2)[,2])
snss2<-str_split_fixed(snss1$`str_split_fixed(sample_names$Samples, "-", 2)[, 2]`, "[.]", 2)[,1]
snss2<-as.data.frame(snss2)
snss2

#Extract friedelin
friedelin<-as.data.frame(matrix(0, ncol = 1, nrow = length(sample_names$Samples)))
colnames(friedelin)<-"friedelin"
rownames(friedelin)<-snss2$snss2

fr<-data.frame(data_useful[which(data_useful$V4<13.507&data_useful$V4>13.487),])
friedelin$friedelin<-fr$V7

#Extract psi-taraxasterol 
psi<-data.frame(data_useful[which(str_detect(data_useful$V4, "TIC") == T | data_useful$V4<13.151 & data_useful$V4>13.123),])
varpsi<-array(0)
for (i in 1:nrow(psi)){
  if (str_detect(psi$V4[i], "TIC") == T & str_detect(psi$V4[i+1], "TIC") == T | str_detect(psi$V4[i], "TIC") == T & i == nrow(psi)){
    print(psi$V4[i])
    varpsi<-append(varpsi, psi$V4[i])
  }

}
varpsi<-varpsi[-1]
library(data.table)
psi[which(psi$V4 %in% varpsi),]
psi<-psi[- which(psi$V4 %in% varpsi),]
psirow1<-psi[seq(1, nrow(psi), 2), ]
psirow2<-psi[seq(2, nrow(psi), 2), ]
psi<-cbind(psirow1,psirow2)
psi<-psi[,-c(1,3)]

name_index<-cbind(snss2,sample_names)
library(dplyr)
psi<-right_join(name_index, psi, by = c("Samples"="V7"))
psi<-psi[,-2]
colnames(psi)<-c("Samples", "Psi")

##Combine with friedelin
friedelin<- setDT(friedelin, keep.rownames = TRUE)[] 
colnames(friedelin)[1]="Samples"

table<-left_join(friedelin, psi, by = c("Samples"="Samples"))

#Extract Tara
 
tara<-data.frame(data_useful[which(str_detect(data_useful$V4, "TIC") == T | data_useful$V4<13.234 & data_useful$V4>13.190),])
vartara<-array(0)
for (i in 1:nrow(tara)){
  if (str_detect(tara$V4[i], "TIC") == T & str_detect(tara$V4[i+1], "TIC") == T | str_detect(tara$V4[i], "TIC") == T & i == nrow(tara)){
    print(tara$V4[i])
    vartara<-append(vartara, tara$V4[i])
  }
}
vartara<-vartara[-1]

tara[which(tara$V4 %in% vartara),]
tara<-tara[- which(tara$V4 %in% vartara),]
tararow1<-tara[seq(1, nrow(tara), 2), ]
tararow2<-tara[seq(2, nrow(tara), 2), ]
tara<-cbind(tararow1,tararow2)
tara<-tara[,-c(1,3)]

name_index<-cbind(snss2,sample_names)

tara<-right_join(name_index, tara, by = c("Samples"="V7"))
tara<-tara[,-2]
colnames(tara)<-c("Samples", "Tara")

##Combine with friedelin
table<-left_join(table, tara, by = c("Samples"="Samples"))

##In this specific context, CoTXSS --> CoTXSS- and Negative --> Negative- for the sake of string splitting
table[table == "CoTXSS"] <- "CoTXSS-"
table$Samples <- str_replace_all(table$Samples, "CoTXSS","CoTXSS-") 
table$Samples <- str_replace_all(table$Samples, "Negative","Negative-") 
table<-table %>% remove_rownames %>% column_to_rownames(var="Samples")
colnames(table)[1]<-"Friedelin"

write.csv(table, "C:/Users/su/Downloads/GCMS/Oct-2022/Peak-area-extraction-Oct2022-CoTXSS.csv")
