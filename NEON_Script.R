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
#Welsh t-test for lenght (sample sizes are not equal, although vars differ less than 3 fold)
t.test(Xiphophorus$fishTotalLength~Xiphophorus$before_after, data=Xiphophorus, var.equal=F)
t.test(Xiphophorus$fishTotalLength~Xiphophorus$before_after, data=Xiphophorus, var.equal=T)
#analysis of weight
hist(Xiphophorus$fishWeight[Xiphophorus$before_after=="before"],breaks=10,col="red",xlab="Fish weight (g)",ylab="Number of fish",main="Fish weight distribution (before)")
hist(Xiphophorus$fishWeight[Xiphophorus$before_after=="after"],breaks=10,col="red",xlab="Fish weight (g)",ylab="Number of fish",main="Fish weight distribution (after)")
#Log transforming the weight data
logweight<-log10(Xiphophorus$fishWeight)
hist(logweight[Xiphophorus$before_after=="before"],breaks=10,col="red",xlab="log fish weight (g)",ylab="Number of fish",main="Log fish weight distribution (before)")
hist(logweight[Xiphophorus$before_after=="after"],breaks=10,col="red",xlab="log fish weight (g)",ylab="Number of fish",main="Log fish weight distribution (after)")

