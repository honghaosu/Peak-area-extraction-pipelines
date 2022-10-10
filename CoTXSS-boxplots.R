data<-read.csv2("C:/Users/su/Downloads/GCMS/Oct-2022/CoTXSS-peak-area-auto-and-manual.csv", sep = ",")
library(tidyverse)
colnames(data)[1]<-"Samples"

temp<-data[,1]
temp<-data.frame(temp)

#Silly method as I don't know about regular expression
a<-str_split_fixed(temp$temp, "-", 3)[,1]
b<-str_split_fixed(temp$temp, "-", 3)[,2]
c<-str_split_fixed(temp$temp, "-", 3)[,3]

df<-data.frame(cbind(a,b,c))

for (i in 1:nrow(df)){
  if (df[i,3]==""){
    df[i,3]=df[i,2]
  }
}

for (i in 1:nrow(df)){
  if (str_detect(df[i,2], "M")==T){
    df[i,1]=paste(df[i,1], df[i,2], sep = "-")
  }
}
#The end of silly method
df<-df[,-2]
colnames(df)<-c("Samples", "Repeat")
data_new<-cbind(df, data)
data_new<-data_new[,-3]
#Re-arrange data_new
x<-c("Negative", "CoTXSS", "Co1", "Co2", "Co3", "Co4", "Co5", "Co6", "Co7", "Co8",
         "Co2M", "Co3-1M", "Co3-2M", "Co4M")

data_new<-data_new %>%
  mutate(Samples = factor(Samples, levels = x)) %>%
  arrange(Samples, Repeat)

#Psi
Psi<-data.frame(data_new[,c(1,2,6)])
Psi$Product<-c(rep("Psi-taraxasterol", times=nrow(Psi)))
colnames(Psi)[3]<-"Yield"

#Tara
Tara<-data.frame(data_new[,c(1,2,7)])
Tara$Product<-c(rep("Taraxasterol", times=nrow(Tara)))
colnames(Tara)[3]<-"Yield"

Table<-rbind(Psi, Tara)
Table<-Table[,-2]
Table<-as_tibble(Table)
Table$Yield<-as.numeric(Table$Yield)
library(ggplot2)
labels<-c("Negative \n Control",
          "CoTXSS",
          "CoTXSS \n G380T",
          "CoTXSS \n D385E",
          "CoTXSS \n H492Q",
          "CoTXSS \n P751A",
          "CoTXSS \n G380D",
          "CoTXSS \n D385A",
          "CoTXSS \n H492A",
          "CoTXSS \n P751D",
          "CoTXSS \n D385E \n H492Q",
          "CoTXSS \n D385E \n H492Q \n P751A",
          "CoTXSS \n G380T \n H492Q \n P751A",
          "CoTXSS \n G380T \n D385E \n H492Q \n P751A"
)

p<-ggplot(Table,mapping = aes(x=Samples, y=Yield, color=Product)) +
  geom_boxplot()+
  scale_x_discrete(labels=labels)+
  ylab("Yield (??g/mg)")+
  theme_bw()
p

