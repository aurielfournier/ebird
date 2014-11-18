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


setwd("C:/Users/Auriel Fournier/Downloads")
dat <- read.delim('BarChart',skip=12,header=T)
dat <- dat[2:4,]

mdat <- melt(dat)

mdat2 <- data.frame(mdat$X, mdat$variable, -mdat$value)
colnames(mdat2) <- colnames(mdat)
mdata <- rbind(mdat, mdat2)

mdata1 <- mdata[mdata$X=="Black-bellied Whistling-Duck",]
mdata2 <- mdata[mdata$X=="Fulvous Whistling-Duck",]
mdata3 <- mdata[mdata$X=="Greater White-fronted Goose",]

graph1 <- ggplot(data=mdata1)+
  geom_bar(aes(x=variable,y=value),position=position_dodge(), stat="identity", fill="black",colour="black")+
  ylim(1,-1)

graph2 <- ggplot(data=mdata2)+
  geom_bar(aes(x=variable,y=value),position=position_dodge(), stat="identity", fill="black",colour="black")+
  ylim(1,-1)

graph3 <- ggplot(data=mdata3)+
  geom_bar(aes(x=variable,y=value),position=position_dodge(), stat="identity", fill="black",colour="black")+
  ylim(1,-1)

grid.arrange(graph1, graph2, graph3)