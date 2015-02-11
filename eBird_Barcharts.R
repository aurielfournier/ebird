#trying to replicate ebird bar graphs

library(ggplot2)
library(reshape)
library(gridExtra)

dat <- data.frame(matrix(ncol=3, nrow=12))
dat$X1 <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
dat$X1 <- factor(dat$X1, levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
dat$X2 <- c(0,3,5,6,7,4,2,2,0,0,1,2)
dat$X3 <- -dat$X2

mdat <- melt(dat, id=c("X1"))

colnames(mdat) <- c("month","variable","value")

ggplot(data=mdat)+
  geom_bar(aes(x=month,y=value),position=position_dodge(), stat="identity", fill="black",colour="black")


setwd("~/GitHub/data")
dat12 <- read.csv('Ebird_2012.csv',header=T)
dat13 <- read.csv('Ebird_2013.csv',header=T)
dat14 <- read.csv('Ebird_2014.csv',header=T)


dat12 <- dat12[dat12$species=="Sora"|dat12$species=="Virginia Rail"|dat12$species=="Yellow Rail",]
dat13 <- dat13[dat13$species=="Sora"|dat13$species=="Virginia Rail"|dat13$species=="Yellow Rail",]
dat14 <- dat14[dat14$species=="Sora"|dat14$species=="Virginia Rail"|dat14$species=="Yellow Rail",]

df12 <- dat12[,c(2,10:ncol(dat12))]
df13 <- dat13[,c(2,10:ncol(dat13))]
df14 <- dat14[,c(2,10:ncol(dat14))]

mdat12 <- melt(df12)
cdat12 <- cast(variable ~ species,data=mdat12)
cdat12 <- cdat12[29:41,]
mdat13 <- melt(df13)
cdat13 <- cast(variable ~ species,data=mdat13)
cdat13 <- cdat13[29:41,]
mdat14 <- melt(df14)
cdat14 <- cast(variable ~ species,data=mdat14)
cdat14 <- cdat14[29:41,]
graph112 <- ggplot(data=mdat12[mdat12$species=="Sora",])+
  geom_bar(aes(x=variable,y=value),position=position_dodge(), stat="identity", fill="black",colour="black")

graph212 <- ggplot(data=mdat12[mdat12$species=="Virginia Rail",])+
  geom_bar(aes(x=variable,y=value),position=position_dodge(), stat="identity", fill="black",colour="black")

graph312 <- ggplot(data=mdat12[mdat12$species=="Yellow Rail",])+
  geom_bar(aes(x=variable,y=value),position=position_dodge(), stat="identity", fill="black",colour="black")

grid.arrange(graph112, graph212, graph312)

graph113 <- ggplot(data=mdat13[mdat13$species=="Sora",])+
  geom_bar(aes(x=variable,y=value),position=position_dodge(), stat="identity", fill="black",colour="black")

graph213 <- ggplot(data=mdat13[mdat13$species=="Virginia Rail",])+
  geom_bar(aes(x=variable,y=value),position=position_dodge(), stat="identity", fill="black",colour="black")

graph313 <- ggplot(data=mdat13[mdat13$species=="Yellow Rail",])+
  geom_bar(aes(x=variable,y=value),position=position_dodge(), stat="identity", fill="black",colour="black")

grid.arrange(graph113, graph213, graph313)

graph114 <- ggplot(data=mdat14[mdat14$species=="Sora",])+
  geom_bar(aes(x=variable,y=value),position=position_dodge(), stat="identity", fill="black",colour="black")

graph214 <- ggplot(data=mdat14[mdat14$species=="Virginia Rail",])+
  geom_bar(aes(x=variable,y=value),position=position_dodge(), stat="identity", fill="black",colour="black")

graph314 <- ggplot(data=mdat14[mdat14$species=="Yellow Rail",])+
  geom_bar(aes(x=variable,y=value),position=position_dodge(), stat="identity", fill="black",colour="black")

grid.arrange(graph114, graph214, graph314)

sora12 <- smooth.spline(cdat12$variable, cdat12$Sora)
vira12 <- smooth.spline(cdat12$variable, cdat12$"Virginia Rail")
yera12 <- smooth.spline(cdat12$variable, cdat12$"Yellow Rail")

sora13 <- smooth.spline(cdat13$variable, cdat13$Sora)
vira13 <- smooth.spline(cdat13$variable, cdat13$"Virginia Rail")
yera13 <- smooth.spline(cdat13$variable, cdat13$"Yellow Rail")

sora14 <- smooth.spline(cdat14$variable, cdat14$Sora)
vira14 <- smooth.spline(cdat14$variable, cdat14$"Virginia Rail")
yera14 <- smooth.spline(cdat14$variable, cdat14$"Yellow Rail")

ks.test(cdat12$Sora,cdat14$Sora)
ks.test(cdat13$Sora,cdat14$Sora)
ks.test(cdat13$Sora,cdat12$Sora)

ks.test(cdat12$"Virginia Rail",cdat14$"Virginia Rail")
ks.test(cdat13$"Virginia Rail",cdat14$"Virginia Rail")
ks.test(cdat13$"Virginia Rail",cdat12$"Virginia Rail")

ks.test(cdat12$"Yellow Rail",cdat14$"Yellow Rail")
ks.test(cdat13$"Yellow Rail",cdat14$"Yellow Rail")
ks.test(cdat13$"Yellow Rail",cdat12$"Yellow Rail")
