fish<-read.csv(file.choose(),header=T)

#Make sure data entered correctly
summary(fish)
fish$SITE<-as.factor(fish$SITE)
fish$SPECIES<-as.factor(fish$SPECIES)
fish$before_after<-as.factor(fish$before_after)
mosquito<-subset(fish,fish$SPECIES=="Gambusia_affinis")
summary(mosquito)

#A few missing steps first
t.test(mosquito$fishTotalLength~mosquito$before_after)

#calculate means and s for each of the two groups (before and after)
aggregate(mosquito$fishTotalLength,by=list(mosquito$before_after),FUN=mean)
aggregate(mosquito$fishTotalLength,by=list(mosquito$before_after),FUN=sd)
aggregate(mosquito$fishTotalLength,by=list(mosquito$before_after),FUN=length)


tapply(mosquito$fishTotalLength, mosquito$before_after, sd)
tapply(mosquito$fishTotalLength, mosquito$before_after, var)
