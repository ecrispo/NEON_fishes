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

#Next species
#Agonostomus length analysis
Agonostomus<- subset(fish, fish$SPECIES=="Agonostomus_monticola")
summary(Agonostomus)
hist(Agonostomus$fishTotalLength[Agonostomus$before_after=="before"],breaks=16,col="red",xlab="Fish total lenght(mm)",ylab="Number of fish",main="Fish lenght distribution (before)")
hist(Agonostomus$fishTotalLength[Agonostomus$before_after=="after"],breaks=16,col="red",xlab="Fish total lenght(mm)",ylab="Number of fish",main="Fish lenght distribution (after)")
tapply(Agonostomus$fishTotalLength, Agonostomus$before_after, mean)     
tapply(Agonostomus$fishTotalLength, Agonostomus$before_after, sd)     
t.test(Agonostomus$fishTotalLength~Agonostomus$before_after, data=Agonostomus, var.equal=F)
#Agonostomus weight analysis
hist(Agonostomus$fishWeight[Agonostomus$before_after=="before"],breaks=16,col="red",xlab="Fish weight(g)",ylab="Number of fish",main="Fish weight distribution (before)")
hist(Agonostomus$fishWeight[Agonostomus$before_after=="after"],breaks=16,col="red",xlab="Fish total lenght(mm)",ylab="Number of fish",main="Fish lenght distribution (after)")
tapply(Agonostomus$fishWeight, Agonostomus$before_after, mean)
tapply(Agonostomus$fishWeight, Agonostomus$before_after, sd)
t.test(Agonostomus$fishWeight~Agonostomus$before_after, data=Agonostomus, var.equal=F)
