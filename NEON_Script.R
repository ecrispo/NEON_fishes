library(lattice)
library(ggplot2)
fish<-read.csv(file.choose(),header=T)

#Make sure data entered correctly
summary(fish)
fish$SITE<-as.factor(fish$SITE)
fish$SPECIES<-as.factor(fish$SPECIES)
fish$before_after<-as.factor(fish$before_after)
mosquito<-subset(fish,fish$SPECIES=="Gambusia_affinis")
summary(mosquito)
Xiphophorus<-subset(fish,fish$SPECIES=="Xiphophorus_hellerii")
summary(Xiphophorus)
Poecilia<-subset(fish,fish$SPECIES=="Poecilia_reticulata")
summary(Poecilia)
PoeciliaCUPE<-subset(Poecilia,Poecilia$SITE=="CUPE")
PoeciliaCUPE$DATE<-as.Date(PoeciliaCUPE$DATE)
PoeciliaGUIL<-subset(Poecilia,Poecilia$SITE=="GUIL")
PoeciliaGUIL$DATE<-as.Date(PoeciliaGUIL$DATE)
qplot(x=DATE,y=fishTotalLength, 
      data=PoeciliaGUIL)
qplot(x=DATE,y=fishTotalLength, 
      data=PoeciliaCUPE)
summary(PoeciliaCUPE)
summary(PoeciliaGUIL)
Agonostomus<- subset(fish, fish$SPECIES=="Agonostomus_monticola")
summary(Agonostomus)
#Focus on the spotted algae eating goby ONLY, not unknown species
spottedGoby<-subset(fish,fish$SPECIES=="Sicydium_punctatum")
spottedGoby$DATE<-as.Date(spottedGoby$DATE)
qplot(x=DATE,y=fishTotalLength, data=spottedGoby) 

#Environmental factors
Env<-read.csv(file.choose(),header=T)
EnvCUPE<-subset(Env, Env$SITE=="CUPE") 
EnvCUPE$DATE<-as.Date(EnvCUPE$DATE)
qplot(x=DATE,y=waterTemp, 
      data=EnvCUPE,
      main="water temp Cupe",xlab="Year",ylab="Water Temp (C)")  
qplot(x=DATE,y=pH, data=EnvCUPE,main="water pH")
TempCUPE <- ggplot(EnvCUPE,aes(x=DATE,y=waterTemp))+labs(x="Year",y="Water temp (C)")+ geom_point()
TempCUPE
dates_vline <- as.Date(c("2017-09-06", "2017-09-20"))  
TempCUPE + geom_vline(xintercept=dates_vline, linetype="dashed")
#removing the outlier
EnvCUPE.pH<-subset(Env,Env$SITE=="CUPE" & Env$pH>5)
EnvCUPE.pH$DATE<-as.Date(EnvCUPE.pH$DATE)
qplot(x=DATE,y=pH, data=EnvCUPE.pH,main="water Ph",xlab="year")
pHCUPE <- ggplot(EnvCUPE.pH,aes(x=DATE,y=pH))+labs(x="Year",y="pH")+ geom_point()
pHCUPE
dates_vline <- as.Date(c("2017-09-06", "2017-09-20"))  
pHCUPE + geom_vline(xintercept=dates_vline, linetype="dashed")


qplot(x=DATE,y=dissolvedOxygen,data=EnvCUPE,main="dissolved Oxygen",ylab="Dissolved Oxygen (mg/L)",xlab="Year")
qplot(x=DATE,y=dissolvedOxygenSaturation,data=EnvCUPE,main="dissloved Oxygen saturation")
OxyCUPE <- ggplot(EnvCUPE,aes(x=DATE,y=dissolvedOxygen))+labs(x="Year",y="Dissolved oxygen (mg/L)") + geom_point()
OxyCUPE
dates_vline <- as.Date(c("2017-09-06", "2017-09-20"))  
OxyCUPE + geom_vline(xintercept=dates_vline, linetype="dashed")


EnvGUIL<-subset(Env, Env$SITE=="GUIL")
EnvGUIL$DATE<-as.Date(EnvGUIL$DATE)
qplot(x=DATE,y=waterTemp, 
      data=EnvGUIL,
      main="water temp Cupe",xlab="Year",ylab="Water Temp (C)")  
#removing temp outlier
EnvGUIL.temp<-subset(Env,Env$SITE=="GUIL" & Env$waterTemp>15)
EnvGUIL.temp$DATE<-as.Date(EnvGUIL.temp$DATE)
qplot(x=DATE,y=waterTemp, data=EnvGUIL.temp,main="water temp",xlab="Year",ylab="Water Temp (C)")
TempGUIL <- ggplot(EnvGUIL.temp,aes(x=DATE,y=waterTemp))+labs(x="Year",y="Water temp (C)") + geom_point()
TempGUIL
dates_vline <- as.Date(c("2017-09-06", "2017-09-20"))  
TempGUIL + geom_vline(xintercept=dates_vline, linetype="dashed")

qplot(x=DATE,y=pH, data=EnvGUIL,main="water pH")
#removing pH outlier
EnvGUIL.pH<-subset(Env,Env$SITE=="GUIL" & Env$pH>5)
EnvGUIL.pH$DATE<-as.Date(EnvGUIL.pH$DATE)
qplot(x=DATE,y=pH,data=EnvGUIL.pH,main="water pH",xlab="Year",ylab="pH")
pHGUIL <- ggplot(EnvGUIL.pH,aes(x=DATE,y=pH))+labs(x="Year",y="pH") + geom_point()
pHGUIL
dates_vline <- as.Date(c("2017-09-06", "2017-09-20"))  
pHGUIL + geom_vline(xintercept=dates_vline, linetype="dashed")


qplot(x=DATE,y=dissolvedOxygen,data=EnvGUIL,main="dissolved Oxygen",xlab="Year",ylab="Dissolved Oxygen (mg/L)")

qplot(x=DATE,y=dissolvedOxygenSaturation,data=EnvGUIL,main="dissloved Oxygen saturation")
OxyGUIL <- ggplot(EnvGUIL,aes(x=DATE,y=dissolvedOxygen))+labs(x="Year",y="Dissolved oxygen (mg/L)") + geom_point()
OxyGUIL
dates_vline <- as.Date(c("2017-09-06", "2017-09-20"))  
OxyGUIL + geom_vline(xintercept=dates_vline, linetype="dashed")


Xiphophorus$DATE
Xiphophorus$DATE<-as.Date(Xiphophorus$DATE)

qplot(x=DATE,y=fishTotalLength, data=Xiphophorus) 
#Added this one:
Xiphophorus
Xiphophorus$DATE<-as.Date(Xiphophorus$DATE)
Xiphophorus2<- subset(Xiphophorus, SPECIES=="Xiphophorus_hellerii" & DATE < "2018-06-01")
Xiphophorus2

qplot(x=DATE,y=fishTotalLength, data=Xiphophorus2) 
xiph <- ggplot(Xiphophorus2,aes(x=DATE,y=fishTotalLength))+labs(x="Year",y="Fish Length (mm)")+ geom_point()
xiph
dates_vline <- as.Date(c("2017-09-06", "2017-09-20"))  
xiph + geom_vline(xintercept=dates_vline, linetype="dashed")

hist(Xiphophorus2$fishTotalLength[Xiphophorus2$before_after=="before"],breaks=16,col="red",xlab="Fish total length(mm)",ylab="Number of fish",main="Fish length distribution (before)")
hist(Xiphophorus2$fishTotalLength[Xiphophorus2$before_after=="after"],breaks=20,col="red",xlab="Fish total length(mm)",ylab="Number of fish",main="Fish length distribution (after)")
tapply(Xiphophorus2$fishTotalLength, Xiphophorus2$before_after, mean)
tapply(Xiphophorus2$fishTotalLength, Xiphophorus2$before_after, sd)
tapply(Xiphophorus2$fishTotalLength, Xiphophorus2$before_after, length)
#The Mann-Whitney-Wilcoxon rank sum test is most appropriate 
#because of the non-normal, bimodel distribution
wilcox.test(Xiphophorus2$fishTotalLength~Xiphophorus2$before_after)

#Change order of time periods so that "before" appears before "after"
Xiphophorus2$before_after<-factor(Xiphophorus2$before_after,levels=c("before","after"))
ggplot(Xiphophorus2, aes(x=before_after, y=fishTotalLength)) +
  geom_boxplot(fill="white",
               color="black", notch=TRUE)+ geom_point(position="jitter")
ggplot(Xiphophorus2, aes(before_after, y=fishTotalLength)) +
  geom_violin(fill="white") + geom_boxplot(fill="white", width=.2)+
  labs(x="Timing of data collection relative to hurricanes",y="Total Fish Length (mm)")
#It is easy to see the bimodal distribution in the violin plot. Easier than in the other two plots.

Agonostomus$DATE
Agonostomus$DATE<-as.Date(Agonostomus$DATE)
Agonostomus2<- subset(Agonostomus, SPECIES=="Agonostomus_monticola" & DATE < "2018-06-01")
Agonostomus2

qplot(x=DATE,y=fishTotalLength, 
      data=Agonostomus2)
agon <- ggplot(Agonostomus2,aes(x=DATE,y=fishTotalLength))+labs(x="Year",y="Fish Length (mm)")+ geom_point()
agon
dates_vline <- as.Date(c("2017-09-06", "2017-09-20"))  
agon + geom_vline(xintercept=dates_vline, linetype="dashed")

hist(Agonostomus2$fishTotalLength[Agonostomus2$before_after=="before"],breaks=16,col="red",xlab="Fish total length (mm)",ylab="Number of fish",main="Fish length distribution (before)")
#This one below also looks a bit bimodal:
hist(Agonostomus2$fishTotalLength[Agonostomus2$before_after=="after"],breaks=16,col="red",xlab="Fish total length (mm)",ylab="Number of fish",main="Fish length distribution (after)")
tapply(Agonostomus2$fishTotalLength, Agonostomus2$before_after, mean)     
tapply(Agonostomus2$fishTotalLength, Agonostomus2$before_after, sd) 
tapply(Agonostomus2$fishTotalLength, Agonostomus2$before_after, length) 
Agonostomus2$before_after<-factor(Agonostomus2$before_after,levels=c("before","after"))
ggplot(Agonostomus2, aes(x=before_after, y=fishTotalLength)) +
  geom_boxplot(fill="white",
               color="black", notch=TRUE)+ geom_point(position="jitter")
ggplot(Agonostomus2, aes(before_after, y=fishTotalLength)) +
  geom_violin(fill="white") + geom_boxplot(fill="white", width=.2)+
  labs(x="Timing of data collection relative to hurricanes",y="Total Fish Length (mm)")
wilcox.test(Agonostomus2$fishTotalLength~Agonostomus2$before_after)


mosquito$DATE<-as.Date(mosquito$DATE)
qplot(x=DATE,y=fishTotalLength, 
      data=mosquito)
mosq <- ggplot(mosquito,aes(x=DATE,y=fishTotalLength))+labs(x="Year",y="Fish Length (mm)")+ geom_point()
mosq
dates_vline <- as.Date(c("2017-09-06", "2017-09-20"))  
mosq + geom_vline(xintercept=dates_vline, linetype="dashed")
hist(mosquito$fishTotalLength[mosquito$before_after=="before"],breaks=16,col="red",xlab="Fish total length (mm)",ylab="Number of fish",main="Fish length distribution (before)")
hist(mosquito$fishTotalLength[mosquito$before_after=="after"],breaks=16,col="red",xlab="Fish total length (mm)",ylab="Number of fish",main="Fish length distribution (after)")
tapply(mosquito$fishTotalLength, mosquito$before_after, mean)     
tapply(mosquito$fishTotalLength, mosquito$before_after, sd) 
tapply(mosquito$fishTotalLength, mosquito$before_after, length) 
wilcox.test(mosquito$fishTotalLength~mosquito$before_after)
mosquito$before_after<-factor(mosquito$before_after,levels=c("before","after"))
ggplot(mosquito, aes(x=before_after, y=fishTotalLength)) +
  geom_boxplot(fill="white",
               color="black", notch=TRUE)+ geom_point(position="jitter")
ggplot(mosquito, aes(before_after, y=fishTotalLength)) +
  geom_violin(fill="white") + geom_boxplot(fill="white", width=.2)+
  labs(x="Timing of data collection relative to hurricanes",y="Total Fish length (mm)")

PoeciliaGUIL2<- subset(PoeciliaGUIL, DATE < "2018-06-01")
PoeciliaGUIL2
summary(PoeciliaGUIL2)
#removing 0 values from guppies
PoeciliaGUIL3<-subset(PoeciliaGUIL2, PoeciliaGUIL2$fishTotalLength>1)
PoeciliaGUIL3
summary(PoeciliaGUIL3)
hist(PoeciliaGUIL3$fishTotalLength[PoeciliaGUIL3$before_after=="before"],breaks=16,col="red",xlab="Fish total length(mm)",ylab="Number of fish",main="Fish length distribution (before)")
hist(PoeciliaGUIL3$fishTotalLength[PoeciliaGUIL3$before_after=="after"],breaks=20,col="red",xlab="Fish total length(mm)",ylab="Number of fish",main="Fish length distribution (after)")
tapply(PoeciliaGUIL3$fishTotalLength, PoeciliaGUIL3$before_after, mean)
tapply(PoeciliaGUIL3$fishTotalLength, PoeciliaGUIL3$before_after, sd)
tapply(PoeciliaGUIL3$fishTotalLength, PoeciliaGUIL3$before_after, length)
t.test(PoeciliaGUIL3$fishTotalLength~PoeciliaGUIL3$before_after,var.equal=F)
wilcox.test(PoeciliaGUIL3$fishTotalLength~PoeciliaGUIL3$before_after)
PoeciliaGUIL3$before_after<-factor(PoeciliaGUIL3$before_after,levels=c("before","after"))
ggplot(PoeciliaGUIL3, aes(before_after, y=fishTotalLength)) +
  geom_violin(fill="white") + geom_boxplot(fill="white", width=.2)+
  labs(x="Timing of data collection relative to hurricanes",y="Total Fish Length (mm)")
PoeciliaGUIL3$DATE<-as.Date(PoeciliaGUIL3$DATE)
guppyGraph2 <- ggplot(PoeciliaGUIL3,aes(x=DATE,y=fishTotalLength))+labs(x="Year",y="Fish Length (mm)")+ geom_point()
guppyGraph2
dates_vline <- as.Date(c("2017-09-06", "2017-09-20"))  
guppyGraph2 + geom_vline(xintercept=dates_vline, linetype="dashed")

library(ggplot2)
library(maps)
library(mapdata)
library(ggmap)
data<-read.csv(file.choose(),header=T)
data$river<-as.factor(data$river)
is.factor(data$river)
Location <- c(-67.3, 17.86, -66.6, 18.4)
myMap <- get_map(location=Location, source="stamen", maptype="toner", crop=FALSE) 
ggmap(myMap)
ggmap(myMap)+ geom_point(aes(x = longitude, y = latitude,color=river), data = data, alpha = .5, size = 5)+
  scale_colour_manual(values=c("Rio Guilarte" = "black", "Rio Cupeyes" = "gray36"))+xlab("longitude") + ylab("latitude")
