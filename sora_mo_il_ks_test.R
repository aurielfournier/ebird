dat <- read.csv("ebd_sora_relNov-2014.csv", header=T)

ddat <- dat[,c("OBSERVATION.COUNT","STATE_PROVINCE","OBSERVATION.DATE")]

dat$OBSERVATION.DATE <- as.character(dat$OBSERVATION.DATE)

dates <- colsplit(dat$OBSERVATION.DATE, "/", names=c("month","day","year"))
dates$month <- as.numeric(dates$month)
dat <- cbind(dates,dat)

dat <- na.omit(dat)

dates <- dat[,c("month","day","year")]
dates<-apply(dates,2, as.numeric) ##this is to make sure they came in as a numeric not as a character or factor
varmonth<-cbind(c(1:12),c(0,31,59,90,120,151,181,212,243,273,304,334))
juldates<-data.frame(date=as.numeric())
for(i in 1:nrow(dates)) {juldates[i,1]<-varmonth[which(dates[i,1]==varmonth[,1]),2]+dates[i,2]
                         
}

dat$jdate <- juldates$date


df <- dat[dat$STATE_PROVINCE=="Missouri"|dat$STATE_PROVINCE=="Illinois",]

df$OBSERVATION.COUNT <- ifelse(df$OBSERVATION.COUNT=="X",1,df$OBSERVATION.COUNT)

df12 <- df[df$year==2012,]
df13 <- df[df$year==2013,]
df14 <- df[df$year==2014,]

df12 <- df12[df12$month<=10&df12$month>=8,]
df13 <- df13[df13$month<=10&df13$month>=8,]
#df14 <- df14[df14$month<=10&df14$month>=8,]



spline12 = smooth.spline(df12$jdate, df12$OBSERVATION.COUNT, spar=.7)
smoothdf12 = data.frame(x=spline12$x, y=spline12$y, year=2012)

spline13 = smooth.spline(df13$jdate, df13$OBSERVATION.COUNT, spar=.7)
smoothdf13 = data.frame(x=spline13$x, y=spline13$y, year=2013)

#spline14 = smooth.spline(df14$jdate, df14$OBSERVATION.COUNT, spar=.7)
#smoothdf14 = data.frame(x=spline14$x, y=spline14$y, year=2014)


smoothdf <- rbind(smoothdf12, smoothdf13)

smoothdf$year <- as.factor(smoothdf$year)
xaxis <- data.frame(jdate=c(min(smoothdf$x):max(smoothdf$x)), value=rep(0))

ggplot() + 
  geom_bar(data=xaxis, aes(x=jdate, y=value), position=position_dodge(), stat="identity", colour="white",size=.5) +
  geom_line(data=m, aes(x=x, y=value, group=year, colour=year), size=2)


mebird <- melt(smoothdf, id=c("year", "x"))
mebird$value <- scale(mebird$value)
mebird$type <- "ebird"

mm <- rbind(mebird, msora)
mm$cat <- paste(mm$year,mm$type,sep="_")

ct <- cast(data=mm, x ~ year + type)

ggplot() + 
  geom_bar(data=xaxis, aes(x=jdate, y=value), position=position_dodge(), stat="identity", colour="white",size=.5) +
  geom_line(data=mm, aes(x=x, y=value, group=cat, colour=cat), size=2)


ctebird <- cast(x ~ year, data=mebird)
ctebird <- ctebird[ctebird$x<=301&ctebird$x>=226,]

ks.test(ct$"2012_count",ct$"2012_ebird")
ks.test(ct$"2013_count",ct$"2013_ebird")
ks.test(ct$"2012_count",ct$"2013_count")
ks.test(ct$"2012_count",ct$"2013_ebird")
ks.test(ct$"2013_count",ct$"2012_ebird")
