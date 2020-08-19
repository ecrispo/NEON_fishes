fish<-read.csv(file.choose(),header=T)

#Make sure data entered correctly
summary(fish)
fish$SITE<-as.factor(fish$SITE)
fish$SPECIES<-as.factor(fish$SPECIES)
fish$before_after<-as.factor(fish$before_after)
mosquito<-subset(fish,fish$SPECIES=="Gambusia_affinis")
summary(mosquito)
#plot the histograms
hist(mosquito$fishTotalLength[mosquito$before_after=="before"],breaks=16,col="red",xlab="Fish total lenght(mm)",ylab="Number of fish",main="Fish lenght distribution (before)")
hist(mosquito$fishTotalLength[mosquito$before_after=="after"],breaks=16,col="red",xlab="Fish total lenght(mm)",ylab="Number of fish",main="Fish lenght distribution (after)")
#standard deviation
sd(mosquito$fishTotalLength[mosquito$before_after=="before"])
sd(mosquito$fishTotalLength[mosquito$before_after=="after"])
#Welsh t-test
t.test(mosquito$fishTotalLength~mosquito$before_after, data=mosquito, var.equal=F)

#calculate means and s for each of the two groups (before and after)
aggregate(mosquito$fishTotalLength,by=list(mosquito$before_after),FUN=mean)
aggregate(mosquito$fishTotalLength,by=list(mosquito$before_after),FUN=sd)
aggregate(mosquito$fishTotalLength,by=list(mosquito$before_after),FUN=length)

tapply(mosquito$fishTotalLength, mosquito$before_after, sd)
tapply(mosquito$fishTotalLength, mosquito$before_after, var)
#Final Graph
install.packages("ggplot2")
library(ggplot2)

mosquito$before_after<-factor(mosquito$before_after,levels=c("before","after"))
ggplot(mosquito, aes(x=before_after, y=fishTotalLength)) +
  geom_boxplot(fill="cornflowerblue",
               color="black", notch=TRUE)+ geom_point(position="jitter", color="blue", alpha=.5)+ geom_rug(side="l", color="black")

ggplot(mosquito, aes(x=before_after, y=fishTotalLength)) +
  geom_boxplot(fill="cornflowerblue",
               color="black", notch=TRUE)+ geom_point(position="jitter", color="blue")

ggplot(mosquito, aes(x=before_after, y=fishTotalLength)) +
  geom_boxplot(fill="white",
               color="black", notch=TRUE)+ geom_point(position="jitter")

ggplot(mosquito, aes(before_after, y=fishTotalLength)) +
  geom_violin(fill="lightblue") + geom_boxplot(fill="lightgreen", width=.2)

ggplot(mosquito, aes(before_after, y=fishTotalLength)) +
   geom_violin(fill="white") + geom_boxplot(fill="white", width=.2)+
   labs(x="Timing of data collection reletive to hurricanes",y="Total Fish Length (mm)")
 
#Checking
print("fish")
#Next species
Xiphophorus<-subset(fish,fish$SPECIES=="Xiphophorus_hellerii")
summary(Xiphophorus)
#analysis of length
hist(Xiphophorus$fishTotalLength[Xiphophorus$before_after=="before"],breaks=16,col="red",xlab="Fish total lenght(mm)",ylab="Number of fish",main="Fish lenght distribution (before)")
hist(Xiphophorus$fishTotalLength[Xiphophorus$before_after=="after"],breaks=20,col="red",xlab="Fish total lenght(mm)",ylab="Number of fish",main="Fish lenght distribution (after)")
tapply(Xiphophorus$fishTotalLength, Xiphophorus$before_after, var)
tapply(Xiphophorus$fishTotalLength, Xiphophorus$before_after, sd)
#Welsh t-test for length (sample sizes are not equal, although vars differ less than 3 fold)
t.test(Xiphophorus$fishTotalLength~Xiphophorus$before_after, data=Xiphophorus, var.equal=F)
t.test(Xiphophorus$fishTotalLength~Xiphophorus$before_after, data=Xiphophorus, var.equal=T)
#analysis of weight
hist(Xiphophorus$fishWeight[Xiphophorus$before_after=="before"],breaks=10,col="red",xlab="Fish weight (g)",ylab="Number of fish",main="Fish weight distribution (before)")
hist(Xiphophorus$fishWeight[Xiphophorus$before_after=="after"],breaks=10,col="red",xlab="Fish weight (g)",ylab="Number of fish",main="Fish weight distribution (after)")
#Log transforming the weight data
logweight<-log(Xiphophorus$fishWeight)
hist(logweight[Xiphophorus$before_after=="before"],breaks=10,col="red",xlab="log fish weight (g)",ylab="Number of fish",main="Log fish weight distribution (before)")
hist(logweight[Xiphophorus$before_after=="after"],breaks=10,col="red",xlab="log fish weight (g)",ylab="Number of fish",main="Log fish weight distribution (after)")




#Data still not normal
#Rank sum test
wilcox.test(Xiphophorus$fishWeight~Xiphophorus$before_after)
tapply(Xiphophorus$fishTotalLength, Xiphophorus$before_after, mean)
tapply(Xiphophorus$fishWeight, Xiphophorus$before_after, mean)
#final graph
Xiphophorus$before_after<-factor(Xiphophorus$before_after,levels=c("before","after"))

ggplot(Xiphophorus, aes(x=before_after, y=fishTotalLength)) +
  geom_boxplot(fill="white",
               color="black", notch=TRUE)+ geom_point(position="jitter")
ggplot(Xiphophorus, aes(before_after, y=fishTotalLength)) +
  geom_violin(fill="white") + geom_boxplot(fill="white", width=.2)+
  labs(x="Timing of data collection reletive to hurricanes",y="Total Fish Length (mm)")
ggplot(Xiphophorus, aes(before_after, y=fishWeight)) +
  geom_violin(fill="white") + geom_boxplot(fill="white", width=.2)+
  labs(x="Timing of data collection reletive to hurricanes",y="Total Fish Length (mm)")

#next species
Poecilia<-subset(fish,fish$SPECIES=="Poecilia_reticulata")
summary(Poecilia)
#Poecilia length analysis
#The attach function code was removed
#Must include Peocilia$ before each variable name in all code below
library(lattice)
histogram(~fishTotalLength | before_after*SITE, data=Poecilia, main="Fish length distribution",xlab="Fish length(mm)")
table(Poecilia$SITE, Poecilia$before_after)
aggregate(Poecilia$fishTotalLength, by=list(before_after,SITE), FUN=mean)
aggregate(Poecilia$fishTotalLength, by=list(before_after,SITE), FUN=sd)
fit <- aov(Poecilia$fishTotalLength ~ before_after*SITE)
summary(fit)
interaction.plot(SITE, before_after, Poecilia$fishTotalLength, type="b", col=c("red","blue"), pch=c(16, 18), main="Interaction between hurricane and site", ylab="Poecilia fish mean lenght")
#Poecilia weight analysis
library(lattice)
histogram(~fishWeight | before_after*SITE, data=Poecilia, main="Fish weight distribution", xlab="Fish weight (g)")
aggregate(Poecilia$fishWeight, by=list(before_after,SITE), FUN=mean)
aggregate(Poecilia$fishWeight, by=list(before_after,SITE), FUN=sd)
fit2 <- aov(Poecilia$fishWeight ~ before_after*SITE)
summary(fit2)
interaction.plot(SITE, before_after, Poecilia$fishWeight, type="b", col=c("red","blue"), pch=c(16, 18), main="Interaction between hurricane and site", ylab= "Poecilia fish mean weight")

PoeciliaCUPE<-subset(Poecilia,Poecilia$SITE=="CUPE")
PoeciliaCUPE$DATE<-as.Date(PoeciliaCUPE$DATE)
qplot(x=DATE,y=fishTotalLength, 
      data=PoeciliaCUPE)
PoeciliaGUIL<-subset(Poecilia,Poecilia$SITE=="GUIL")
PoeciliaGUIL$DATE<-as.Date(PoeciliaGUIL$DATE)
qplot(x=DATE,y=fishTotalLength, 
      data=PoeciliaGUIL)
summary(PoeciliaCUPE)
summary(PoeciliaGUIL)
length(PoeciliaCUPE)
length(PoeciliaGUIL)

Poecilia$before_after<-factor(Poecilia$before_after,levels=c("before","after"))
ggplot(PoeciliaCUPE, aes(before_after, y=fishTotalLength)) +
  geom_violin(fill="white") + geom_boxplot(fill="white", width=.2)+
  labs(x="Timing of data collection reletive to hurricanes",y="Total Fish Length (mm)")

ggplot(PoeciliaGUIL, aes(before_after, y=fishTotalLength)) +
  geom_violin(fill="white") + geom_boxplot(fill="white", width=.2)+
  labs(x="Timing of data collection reletive to hurricanes",y="Total Fish Length (mm)")


#Next species
#Agonostomus length analysis
Agonostomus<- subset(fish, fish$SPECIES=="Agonostomus_monticola")
summary(Agonostomus)
hist(Agonostomus$fishTotalLength[Agonostomus$before_after=="before"],breaks=16,col="red",xlab="Fish total length(mm)",ylab="Number of fish",main="Fish lenght distribution (before)")
hist(Agonostomus$fishTotalLength[Agonostomus$before_after=="after"],breaks=16,col="red",xlab="Fish total length(mm)",ylab="Number of fish",main="Fish lenght distribution (after)")
tapply(Agonostomus$fishTotalLength, Agonostomus$before_after, mean)     
tapply(Agonostomus$fishTotalLength, Agonostomus$before_after, sd) 
tapply(Agonostomus$fishTotalLength, Agonostomus$before_after, length) 
t.test(Agonostomus$fishTotalLength~Agonostomus$before_after, data=Agonostomus, var.equal=F)
#Agonostomus weight analysis
hist(Agonostomus$fishWeight[Agonostomus$before_after=="before"],breaks=16,col="red",xlab="Fish weight(g)",ylab="Number of fish",main="Fish weight distribution (before)")
hist(Agonostomus$fishWeight[Agonostomus$before_after=="after"],breaks=16,col="red",xlab="Fish weight (g)",ylab="Number of fish",main="Fish lenght distribution (after)")
tapply(Agonostomus$fishWeight, Agonostomus$before_after, mean)
tapply(Agonostomus$fishWeight, Agonostomus$before_after, sd)
tapply(Agonostomus$fishWeight, Agonostomus$before_after, length)
t.test(Agonostomus$fishWeight~Agonostomus$before_after, data=Agonostomus, var.equal=F)
#FINAL GRAPH
Agonostomus$before_after<-factor(Agonostomus$before_after,levels=c("before","after"))
ggplot(Agonostomus, aes(before_after, y=fishTotalLength)) +
  geom_violin(fill="white") + geom_boxplot(fill="white", width=.2)+
  labs(x="Timing of data collection reletive to hurricanes",y="Total Fish Length (mm)")
ggplot(Agonostomus, aes(before_after, y=fishWeight)) +
  geom_violin(fill="white") + geom_boxplot(fill="white", width=.2)+
  labs(x="Timing of data collection reletive to hurricanes",y="Total Fish Length (mm)")

#Should exclude UNKNOWN goby species
gobies<-subset(fish,fish$SPECIES=="Sicydium_punctatum"|fish$SPECIES=="Sicydium_spp")

gobies
length(gobies$fishTotalLength)
summary(fish)
hist(gobies$fishTotalLength[gobies$before_after=="before"],breaks=16,col="red",xlab="Fish length (mm)",ylab="Number of fish",main="Fish length distribution (before)")
hist(gobies$fishTotalLength[gobies$before_after=="after"],breaks=16,col="red",xlab="Fish length (mm)",ylab="Number of fish",main="Fish length distribution (after)")
tapply(gobies$fishTotalLength, gobies$before_after, mean)
tapply(gobies$fishTotalLength, gobies$before_after, sd)
tapply(gobies$fishTotalLength, gobies$before_after, length)
t.test(gobies$fishTotalLength~gobies$before_after, data=gobies, var.equal=F)
wilcox.test(gobies$fishTotalLength~gobies$before_after)
#weight 
hist(gobies$fishWeight[gobies$before_after=="before"],breaks=16,col="red",xlab="Fish weight (g)",ylab="Number of fish",main="Fish weight distribution (before)")
hist(gobies$fishWeight[gobies$before_after=="after"],breaks=16,col="red",xlab="Fish weight (g)",ylab="Number of fish",main="Fish weight distribution (before)")
logweightgobies<-log(gobies$fishWeight)
hist(logweightgobies[gobies$before_after=="before"],breaks=10,col="red",xlab="log fish weight (g)",ylab="Number of fish",main="Log fish weight distribution (before)")
hist(logweightgobies[gobies$before_after=="after"],breaks=10,col="red",xlab="log fish weight (g)",ylab="Number of fish",main="Log fish weight distribution (after)")
tapply(logweightgobies, gobies$before_after, mean)
tapply(logweightgobies, gobies$before_after, sd)
t.test(logweightgobies~gobies$before_after, data=gobies, var.equal=F)

#Focus on the spotted algae eating goby ONLY
spottedGoby<-subset(fish,fish$SPECIES=="Sicydium_punctatum")
spottedGoby$DATE<-as.Date(spottedGoby$DATE)
qplot(x=DATE,y=fishTotalLength, data=spottedGoby) 
summary(spottedGoby)
length(spottedGoby$fishWeight)
hist(spottedGoby$fishTotalLength[spottedGoby$before_after=="before"],breaks=16,col="red",xlab="Fish length (mm)",ylab="Number of fish",main="Fish length distribution (before)")
hist(spottedGoby$fishTotalLength[spottedGoby$before_after=="after"],breaks=16,col="red",xlab="Fish length (mm)",ylab="Number of fish",main="Fish length distribution (after)")
tapply(spottedGoby$fishTotalLength, spottedGoby$before_after, mean)
tapply(spottedGoby$fishTotalLength, spottedGoby$before_after, sd)
tapply(spottedGoby$fishTotalLength, spottedGoby$before_after, length)
wilcox.test(spottedGoby$fishTotalLength~spottedGoby$before_after)
#Since the data are bimodal, especially in the AFTER group, cannot use t-test
#Use Mann-Whitney Wilcoxon U test instead
t.test(spottedGoby$fishTotalLength~spottedGoby$before_after, data=spottedGoby, var.equal=F)
#weight 
hist(spottedGoby$fishWeight[spottedGoby$before_after=="before"],breaks=16,col="red",xlab="Fish weight (g)",ylab="Number of fish",main="Fish weight distribution (before)")
hist(spottedGoby$fishWeight[spottedGoby$before_after=="after"],breaks=16,col="red",xlab="Fish weight (g)",ylab="Number of fish",main="Fish weight distribution (after)")
logweightgobies2<-log(spottedGoby$fishWeight)
hist(logweightgobies2[spottedGoby$before_after=="before"],breaks=10,col="red",xlab="log fish weight (g)",ylab="Number of fish",main="Log fish weight distribution (before)")
hist(logweightgobies2[spottedGoby$before_after=="after"],breaks=10,col="red",xlab="log fish weight (g)",ylab="Number of fish",main="Log fish weight distribution (after)")
tapply(logweightgobies2, spottedGoby$before_after, mean)
tapply(logweightgobies2, spottedGoby$before_after, sd)
t.test(logweightgobies2~spottedGoby$before_after, data=spottedGoby, var.equal=F)
wilcox.test(spottedGoby$fishWeight~spottedGoby$before_after)
#final graph
spottedGoby$before_after<-factor(spottedGoby$before_after,levels=c("before","after"))
ggplot(spottedGoby, aes(before_after, y=fishTotalLength)) +
  geom_violin(fill="white") + geom_boxplot(fill="white", width=.2)+
  labs(x="Timing of data collection reletive to hurricanes",y="Total Fish Length (mm)")
ggplot(spottedGoby, aes(before_after, y=fishWeight)) +
  geom_violin(fill="white") + geom_boxplot(fill="white", width=.2)+
  labs(x="Timing of data collection reletive to hurricanes",y="Total Fish Length (mm)")

#Environmental factors
Env<-read.csv(file.choose(),header=T)
sd(Env$waterTemp,na.rm=T) #EXAMPLE
sd(Env$waterTemp) #EXAMPLE


EnvCUPE<-subset(Env, Env$SITE=="CUPE") 

EnvCUPE$DATE<-as.Date(EnvCUPE$DATE)

library(ggplot2)

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

#New analysis


Xiphophorus$DATE
Xiphophorus$DATE<-as.Date(Xiphophorus$DATE)

qplot(x=DATE,y=fishTotalLength, data=Xiphophorus) 
#Added this one:
Xiphophorus2$DATE<-as.Date(Xiphophorus2$DATE)
qplot(x=DATE,y=fishTotalLength, data=Xiphophorus2) 
xiph <- ggplot(Xiphophorus2,aes(x=DATE,y=fishTotalLength))+labs(x="Year",y="Fish length (mm)")+ geom_point()
xiph
dates_vline <- as.Date(c("2017-09-06", "2017-09-20"))  
xiph + geom_vline(xintercept=dates_vline, linetype="dashed")
#I was worried that the data were bimodal because you have data from two different months in "after"
#But even when you look only at Feb, the "after" data are biomodal. 
hist(Xiphophorus2$fishTotalLength[Xiphophorus2$DATE > "2017-12-07"])

Xiphophorus2<- subset(fish, fish$SPECIES=="Xiphophorus_hellerii" & DATE < "2018-06-01")
Xiphophorus2
hist(Xiphophorus2$fishTotalLength[Xiphophorus2$before_after=="before"],breaks=16,col="red",xlab="Fish total lenght(mm)",ylab="Number of fish",main="Fish lenght distribution (before)")
hist(Xiphophorus2$fishTotalLength[Xiphophorus2$before_after=="after"],breaks=20,col="red",xlab="Fish total lenght(mm)",ylab="Number of fish",main="Fish lenght distribution (after)")
tapply(Xiphophorus2$fishTotalLength, Xiphophorus2$before_after, mean)
tapply(Xiphophorus2$fishTotalLength, Xiphophorus2$before_after, sd)
#The Mann-Whitney-Wilcoxon rank sum test is most appropriate 
#because of the non-normal, bimodel distribution
wilcox.test(Xiphophorus2$fishTotalLength~Xiphophorus2$before_after)
t.test(Xiphophorus$fishTotalLength~Xiphophorus$before_after, data=Xiphophorus, var.equal=F)
t.test(Xiphophorus$fishTotalLength~Xiphophorus$before_after, data=Xiphophorus, var.equal=T)

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
Agonostomus2<- subset(fish, fish$SPECIES=="Agonostomus_monticola" & DATE < "2018-06-01")
Agonostomus2
Agonostomus2$DATE<-as.Date(Agonostomus2$DATE)
qplot(x=DATE,y=fishTotalLength, 
      data=Agonostomus2)
agon <- ggplot(Agonostomus2,aes(x=DATE,y=fishTotalLength))+labs(x="Year",y="Fish length (mm)")+ geom_point()
agon
dates_vline <- as.Date(c("2017-09-06", "2017-09-20"))  
agon + geom_vline(xintercept=dates_vline, linetype="dashed")

hist(Agonostomus2$fishTotalLength[Agonostomus2$before_after=="before"],breaks=16,col="red",xlab="Fish total length(mm)",ylab="Number of fish",main="Fish lenght distribution (before)")
#This one below also looks a bit bimodal:
hist(Agonostomus2$fishTotalLength[Agonostomus2$before_after=="after"],breaks=16,col="red",xlab="Fish total length(mm)",ylab="Number of fish",main="Fish lenght distribution (after)")
tapply(Agonostomus2$fishTotalLength, Agonostomus2$before_after, mean)     
tapply(Agonostomus2$fishTotalLength, Agonostomus2$before_after, sd) 
tapply(Agonostomus2$fishTotalLength, Agonostomus2$before_after, length) 
t.test(Agonostomus2$fishTotalLength~Agonostomus2$before_after, data=Agonostomus2, var.equal=F)
Agonostomus2$before_after<-factor(Agonostomus2$before_after,levels=c("before","after"))
ggplot(Agonostomus2, aes(x=before_after, y=fishTotalLength)) +
  geom_boxplot(fill="white",
               color="black", notch=TRUE)+ geom_point(position="jitter")
ggplot(Agonostomus2, aes(before_after, y=fishTotalLength)) +
  geom_violin(fill="white") + geom_boxplot(fill="white", width=.2)+
  labs(x="Timing of data collection relative to hurricanes",y="Total Fish Length (mm)")
#I am doing this out of curiosity:
wilcox.test(Agonostomus2$fishTotalLength~Agonostomus2$before_after)
#P values are typically lower for the t-test than for the rank-based test
#But in this case the P value is even lower for the rank test
#This suggests to me that the t-test is not valid due to the bimodal distribution



mosquito$DATE<-as.Date(mosquito$DATE)
qplot(x=DATE,y=fishTotalLength, 
      data=mosquito)
mosq <- ggplot(mosquito,aes(x=DATE,y=fishTotalLength))+labs(x="Year",y="Fish Length (mm)")+ geom_point()
mosq
dates_vline <- as.Date(c("2017-09-06", "2017-09-20"))  
mosq + geom_vline(xintercept=dates_vline, linetype="dashed")
hist(mosquito$fishTotalLength[mosquito$before_after=="before"],breaks=16,col="red",xlab="Fish total lenght(mm)",ylab="Number of fish",main="Fish lenght distribution (before)")
hist(mosquito$fishTotalLength[mosquito$before_after=="after"],breaks=16,col="red",xlab="Fish total lenght(mm)",ylab="Number of fish",main="Fish lenght distribution (after)")
tapply(mosquito$fishTotalLength, mosquito$before_after, mean)     
tapply(mosquito$fishTotalLength, mosquito$before_after, sd) 
tapply(mosquito$fishTotalLength, mosquito$before_after, length) 
t.test(mosquito$fishTotalLength~mosquito$before_after, data=mosquito, var.equal=F)
wilcox.test(mosquito$fishTotalLength~mosquito$before_after)
mosquito$before_after<-factor(mosquito$before_after,levels=c("before","after"))
ggplot(mosquito, aes(x=before_after, y=fishTotalLength)) +
  geom_boxplot(fill="white",
               color="black", notch=TRUE)+ geom_point(position="jitter")
ggplot(mosquito, aes(before_after, y=fishTotalLength)) +
  geom_violin(fill="white") + geom_boxplot(fill="white", width=.2)+
  labs(x="Timing of data collection relative to hurricanes",y="Total Fish Length (mm)")
#If you want to strive for consistency, it would be valid to use Mann-Whitney-Wilcoxon test for all three analyses
#It would not change any of your conclusions

#I like the violin plots because you can see the distributions like in the histograms
#I also think the graphs with the date on the X axis are valuablen because they show visually when the samples were collected

#The mosquitofish did not evolve in the presence of hurricanes
#The other two (mountain mullet and swordtail) got smaller
#It is typical that animals will evolve smaller size in the face of stress
#This is called r selection (in contrast to K selection)
#Look up some articles on r versus K selection
#There should be plenty of old (i.e. 'classic') articles on this topic


#The code below puts on lines at the hurricane time periods:
pHplot <- ggplot(EnvGUIL,aes(x=DATE,y=pH),xlab="Year",ylab="pH") + geom_point()
pHplot
dates_vline <- as.Date(c("2017-09-06", "2017-09-20"))  
pHplot + geom_vline(xintercept=dates_vline, linetype="dashed")
