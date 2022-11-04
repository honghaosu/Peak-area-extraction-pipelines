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

p1<-ggplot(Table,mapping = aes(x=Samples, y=Yield, color=Product)) +
  geom_boxplot()+
  scale_x_discrete(labels=labels)+
  ylab("Yield (ig/mg)")+
  theme_bw()
p1

#MWU test for psi
MWUpsi<-Psi[,c(1,3)]
MWUpsi$Yield<-as.numeric(MWUpsi$Yield)
psiyield<-as.data.frame(MWUpsi$Yield)
MPsiout<-rep(0,12)
for(a in 1:12){
  psitest<-wilcox.test(psiyield[7:12,1], psiyield[(6*a+7):(6*a+12),1], correct = FALSE)
  MPsiout[a]<-psitest[[3]]
}
MPsiout

#MWU test for tara
MWUtara<-Tara[,c(1,3)]
MWUtara$Yield<-as.numeric(MWUtara$Yield)
tarayield<-as.data.frame(MWUtara$Yield)

MTaraout<-rep(0,12)
for(a in 1:12){
  p<-wilcox.test(tarayield[7:12,1], tarayield[(6*a+7):(6*a+12),1], correct = FALSE)
  MTaraout[a]<-p[[3]]
}
MTaraout

#Add significance labels
#psi
MWUpsilabel<-rep(0,12)
for(b in 1:12){
  if(MPsiout[b]<=0.01){
    MWUpsilabel[b]<-"**"
    }else if(MPsiout[b]>0.01 & MPsiout[b]<=0.05){
    MWUpsilabel[b]<-"*"
    }else{
    MWUpsilabel[b]<-"NS"
    }
}

Cosamplenames<-levels(Psi$Samples)
psinamelabel<-cbind(Cosamplenames[-c(1,2)],MWUpsilabel)
psinamelabel<-data.frame(psinamelabel)
colnames(psinamelabel)<-c("Samples", "Significance")
psinamelabel
psinamelabel<-psinamelabel%>%
  mutate(Product="Psi-taraxasterol")
#tara
MWUtaralabel<-rep(0,12)
for(b in 1:12){
  if(MTaraout[b]<=0.01){
    MWUtaralabel[b]<-"**"
  }else if(MTaraout[b]>0.01 & MTaraout[b]<=0.05){
    MWUtaralabel[b]<-"*"
  }else{
    MWUtaralabel[b]<-"NS"
  }
}

taranamelabel<-cbind(Cosamplenames[-c(1,2)],MWUtaralabel)
taranamelabel<-data.frame(taranamelabel)
colnames(taranamelabel)<-c("Samples", "Significance")
taranamelabel<-taranamelabel%>%
  mutate(Product="Taraxasterol")


#Change the height of the label
psiheight<-rep(0,12)
for(c in 1:12){
  psiheight[c]<-max(Psi$Yield[(6*c+7):(6*c+12)]) #What's wrong with Co4?
}
psiheight<-as.numeric(psiheight)
psiheight[4]<-12.72603711
psiheight<-psiheight+1
psinamelabel
psinamelabel$Height<-psiheight

taraheight<-rep(0,12)
for(c in 1:12){
  taraheight[c]<-max(Tara$Yield[(6*c+7):(6*c+12)])
}
taraheight<-as.numeric(taraheight)
taraheight<-taraheight+1
taraheight

taranamelabel$Height<-taraheight
taranamelabel
namelabel<-rbind(psinamelabel, taranamelabel)

p2<-p1+
  geom_text(data = namelabel, aes(x = Samples, y = Height, #to be changed 
                           label = Significance, fill=Product), 
            color=ifelse(namelabel$Product=="Psi-taraxasterol","#F8766D","#00BFC4"), 
            size = 4, position = position_dodge(1))

p2
##Just for the poster and paper
T2<-Table[which(str_detect(Table$Samples,"Negative")==T|str_detect(Table$Samples,"CoTXSS")==T|str_detect(Table$Samples,"Co2M")==T|str_detect(Table$Samples,"Co4M")==T),]
labels<-c("Negative \n Control",
          "CoTXSS",
          
          "CoTXSS \n D385E \n H492Q",
          
          "CoTXSS \n G380T \n D385E \n H492Q \n P751A"
)
T2
T2$Product<-str_replace_all(T2$Product, "Psi", "ø")

p1<-ggplot(T2,mapping = aes(x=Samples, y=Yield, fill=Product)) +
  geom_boxplot()+
  scale_x_discrete(labels=labels)+
  ylab("Yield (ìg/mg)")+
  scale_fill_manual(values=c('#EE7733', '#009988'))
  

nl<-namelabel[which(str_detect(namelabel$Samples,"Co2M")==T|str_detect(namelabel$Samples,"Co4M")==T),]
nl$Product<-str_replace_all(nl$Product, 'Psi', 'ø')
p2<-p1+
  geom_text(data = nl, aes(x = Samples, y = Height, #to be changed 
                                  label = Significance, fill=Product), 
            color=ifelse(nl$Product=="ø-taraxasterol",'#EE7733', '#009988'), 
            size = 5, position = position_dodge(1))+theme_bw()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        legend.key.size = unit(1, 'cm'),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16))
p2

#No need to be transparent