
setwd('/Users/AurielFournier/Dropbox/R/ebird')
################################################
ababase<-read.csv('/Users/AurielFournier/Dropbox/R/ebird/ababase.csv',header=T) ### this the base aba file created to filter/group birds

filter<-c(' sp.','hybrid','Domestic','/')   ###what things should be filtered by (exact cases)

url1<-'http://ebird.org/ebird/BarChart?cmd=getChart&displayType=download&getLocations=states&states=US-AL&bYear=1900&eYear=2014&bMonth=1&eMonth=12&reportType=location&parentState=US-AL'
worklist<-c(2014) ###this is a list of years you want to download, if you do any range of time make sure its in the form 'yearstart-yearend', the '-' is important
states<-c('MO') ###a vector of the states you want to download ebird data from and aggregate over, maxed at 3
qs2<-length(states)
###dont need this for now
#extract1<-c('US-NC-013,US-NC-031,US-NC-049,US-NC-061,US-NC-079,US-NC-095,US-NC-103,US-NC-107,US-NC-133,US-NC-137,US-NC-147'),split=','))
#extract2<-c('US-NC-015,US-NC-029,US-NC-041,US-NC-053,US-NC-073,US-NC-083,US-NC-131,US-NC-139,US-NC-143')
#extract3<-c('US-MD-003,US-MD-005,US-MD-009,US-MD-013,US-MD-017,US-MD-021,US-MD-031,US-MD-033,US-MD-043')
######

################################################
#########start download#########################
for(p in states){
  for(i in worklist){
    url1s<-sub('US-AL',paste0('US-',states),url1)
    url1s<-sub('1900',substr(i,1,4),url1s)
    url1s<-sub('2014',substr(i,nchar(i)-4,nchar(i)),url1s)
    download.file(url1s,paste0('Ebird_',states,'_',i))
    
  }
}
#####lets read in the data as a text file#####

for(i in worklist){
  #i<-2013 ##for testing purposes only
  nsample<-t(read.delim(paste0('Ebird_',states[1],'_',i),skip=13,nrow=1,header=F,row.names=1)[,1:48])
  frame1<-read.delim(paste0('Ebird_',states[1],'_',i),skip=14,header=F,row.names=1)[,1:48]
  colnames(frame1)<-paste0(rep(1:12,1,each=4),'.',1:4)
  
  ####
  if(qs2>1){nsample2<-t(read.delim(paste0('Ebird_',states[2],'_',i),skip=13,nrow=1,header=F,row.names=1)[,1:48])
            frame3<-read.delim(paste0('Ebird_',states[2],'_',i),skip=14,header=F,row.names=1)[,1:48]
            colnames(frame3)<-paste0(rep(1:12,1,each=4),'.',1:4)}
  if(qs2==3){nsample3<-t(read.delim(paste0('Ebird_',states[3],'_',i),skip=13,nrow=1,header=F,row.names=1)[,1:48])
             frame6<-read.delim(paste0('Ebird_',states[3],'_',i),skip=14,header=F,row.names=1)[,1:48]
             colnames(frame6)<-paste0(rep(1:12,1,each=4),'.',1:4)}
  
  ##############################################
  ##Merges multiple states together
  if(qs2>1){
    test3<-merge(frame1*nsample,frame3*nsample2,by='row.names')
    frame2<-(test3[,2:49]+test3[,50:97])/(nsample+nsample2)
    row.names(frame2)<-test3$Row.names
    colnames(frame2)<-paste0(rep(1:12,1,each=4),'.',1:4)
  }
  if(qs2==3){
    test3<-merge(frame2*(nsample+nsample2),frame6*nsample3,by='row.names')
    
    frame2<-(test3[,2:49]+test3[,50:97])/(nsample+nsample2+nsample3)
    row.names(frame2)<-test3$Row.names
    colnames(frame2)<-paste0(rep(1:12,1,each=4),'.',1:4)
  } 
  if(qs2==1){frame2<-frame1}
  ##############################################
  ###Remove sp's,hybrids,domestics,unknowns#####
  if(sum(filter%in%row.names(frame2))!=0){
    frame23<-frame2[-unique(unlist(sapply(filter,grep,row.names(frame2),ignore.case=F))),]}
  frame2$id<-1:nrow(frame2)
  ##############################################merge with template
  frame3<-merge(frame2,ababase,by.x='row.names',by.y='species')
  frame3<-frame3[order(frame3$id),]
  colnames(frame3)[1]<-'species'
  frame3<-frame3[,c(50,1,53,51,52,54,55,56,57,2:49)]
  ##############################################
  write.csv(frame3,paste0('Ebird_',i,'.csv'),row.names=F)
}
