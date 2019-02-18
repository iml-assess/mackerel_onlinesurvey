#################################################################################################################
#*** Analyse results from mackerel informal online survey
#*** survey was done on 2016 and 2016
#################################################################################################################


#################################################################################################################
########### READ IN DATA ########################################################################################
#################################################################################################################

########### LOAD ########################################################################################
wb <- loadWorkbook("data/online_survey_results.xlsx")
answers <- readWorksheet(wb, 'answers_clean')

########### CLEAN ########################################################################################
#### TRANSLATION
answers$bait = tolower(as.character(answers$bait))
answers[answers$bait=='na','bait']=NA
answers$gear = tolower(as.character(answers$gear))
answers[answers$gear=='na','gear']=NA
answers = translate.df(answers) 

#### FORMATTING
answers$bait = as.character(answers$bait)
answers$gear = as.character(answers$gear)
answers$particip_before = as.character(answers$particip_before)

answers$Year = as.numeric(as.character(answers$Year))
answers$declared.perc = as.numeric(as.character(answers$declared.perc))
answers$bait_use.perc = as.numeric(as.character(answers$bait_use.perc))
answers$experience = as.numeric(as.character(answers$experience))
answers$X.b = as.numeric(as.character(answers$X.b))
answers$X.c = as.numeric(as.character(answers$X.c))
answers$X.r = as.numeric(as.character(answers$X.r))
answers$discards2 = as.numeric(as.character(answers$discards2))*0.453592 #kg
answers$Catch2018 = as.numeric(as.character(answers$Catch2018))*0.453592 #kg
answers$Catch2016 = as.numeric(as.character(answers$Catch2016))*0.453592 #kg
answers$lat = as.numeric(as.character(answers$lat))
answers$lon = as.numeric(as.character(answers$lon))

answers$Time = as.POSIXct(answers$Time)

#### Delete empty answers and raw info
answers[,c('activities_detail','region','port','declared','bait_use','declared_change','Catch2015','catch','discards')] <- list(NULL)
answers = answers[!is.na(answers$activity),]

#### fuse catch into 1 column
answers$catch = rowSums(answers[,paste0('Catch',unique(answers$Year))],na.rm = TRUE)
answers[,paste0('Catch',unique(answers$Year))] = list(NULL)

#### code to help match port coordinates (not necesarry anymore once done)
# load(file="Rdata/ports.Rdata")
# ports=rbind(ports,data.frame(category=NA,lat=c(38.898556),lon=c(-77.037852),port="IM",prov="QC"))
# 
# library(stringdist)
# 
# 
# answers$lat=NA;answers$lon=NA;answers$port_match=NA;answers$prov_match=NA
# 
# for(i in 1:nrow(answers)){
#   if(is.na(answers$port[i])){next}
#   search=closest.match(answers$port[i],ports$port) #search=agrep(answers$port[i],ports$port) #ports[agrep("pleasant",ports$port),]
#   if(length(search)==0){next}else{
#   answers$port_match[i]=search
#   answers$prov_match[i]=ports[ports$port==search,'prov']
#   answers$lat[i]=ports[ports$port==search,"lat"]
#   answers$lon[i]=ports[ports$port==search,"lon"]}
# }
# answers[answers$Year==2018,c('port','port_match','lat','lon','prov_match')][20:30,]
# ports[agrep("lorn",ports$port),] #look up port

#################################################################################################################
########### ANALYSES ############################################################################################
#################################################################################################################

Year = c('2016','2018')
ans = answers[answers$Year %in% Year,]
active = ans[ans$catch!=0,] # only work with the ones 100% sure they fished (not all were excluded in first resdoc)
active$comments = NULL # for conveniences because super big

########### BASIC STATISTICS ########################################################################################

##### number of particpants
table(answers$Year) # number of unique non-empty forms submitted
table(active$Year) # number of participants who declared catch

#### licensed?
table(active[!is.na(active$activity) & active$activity %in% c('Bait','Commercial','A combination of these'),]$Year)

#### recreational?
table(active[!is.na(active$activity) & active$activity %in% c('Recreational'),]$Year)

#### by form
get.stats(active,'form')
get.stats(active[active$form!='EN.FB',],'form')

### by province
get.stats(active,'prov')
get.stats(active[active$activity %in% c('Bait','Commercial','A combination of these'),],'prov') # number of active licensed by province

### by activity
get.stats(active,'activity')

subact = active[active$activity=="A combination of these" &!is.na(active$activity),]
ddply(subact,c('Year'),summarise,bait=mean(X.b,na.rm=T),commercial=mean(X.c,na.rm=T),rec=mean(X.r,na.rm=T)) #mean of each activity

active[active$activity=='Recreational' & active$gear != 'rod and line' &!is.na(active$gear),c('Year','prov')]

#### tonnes caught
ddply(active,'Year',summarise,catch=round(sum(catch)/1000))

dcast(ddply(active,c('Year','activity'),summarise,catch=round(sum(catch)/1000),2),Year~activity,value.var = 'catch')
dcast(ddply(active,c('Year','prov'),summarise,catch=round(sum(catch)/1000),2),Year~prov,value.var = 'catch')

#### comments
comm = ddply(active,c('Year'),function(x){
    y = unlist(strsplit(as.character(x$comments_stand),", "))
    y = data.frame(table(y))
    y = y[!is.na(y$y) & !y$y=='NA',]
    y[order(-y$Freq),]
})
comm

########### NON-SPATIAL PLOTS ###################################################################################
#### TIMESTAMPS ANSWERS --------------------------------------------------------------------------------
active$date = as.Date(paste0(format(active$Time, format="%m-%d"),"-00"), format="%m-%d-%y")
N = ddply(active,'Year',summarise,N=length(Year))
N$date = max (active$date)
p.time <- ggplot(active[active$date >= "2000-10-01",],aes(x=date))+
    facet_grid(Year~.)+
    geom_histogram(bins=50,aes(fill=form))+
    scale_y_continuous(expand=c(0,0))+
    scale_fill_viridis_d()+
    geom_text(data=N,aes(y=50,label=paste("N =",N)),hjust=1)+
    labs(fill='',x='Date',y='Number')


savepng(p.time,"./img/",'time',c(18,10))

#### ACTIVITY --------------------------------------------------------------------------------
act = ddply(active,c('Year','activity'),summarise,nr=length(form))
cols = c("yellowgreen","violetred","orange",'grey',"slateblue")

for(y in Year){
    x = active[active$Year==y,]
    if(length(unique(x$activity))==4) mycol=cols[-4] else mycol=cols #grey if there is 'educational'
    # overall
    savepng(pieplot(x,id='activity',width=c(0.6,0.4),col=mycol),
            paste0("./img/",y,"/"),'activity',c(12,8))
    
    # by province
    
    savepng(pieplot(x,id='activity',facet='prov',total=TRUE,ncol=3,col=mycol,repel = TRUE),
            paste0("./img/",y,"/"),'activity.prov',c(26,20),res=500)
    
    x <- translate.df(x,to='fr')

    savepng(pieplot(x,id='activity',facet='prov',total=TRUE,ncol=3,col=mycol,repel = TRUE),
            paste0("./img/",y,"/FR/"),'activity.prov',c(26,20),res=500) 
}

#### GEAR --------------------------------------------------------------------------------
active$ngear = do.call('c',lapply(strsplit(as.character(active$gear),", "),function(x)length(x[!is.na(x)])))  
get.stats(active[active$ngear>0,],'ngear')

gear.act = ddply(active,c('Year','activity'),function(x){
    y = unlist(strsplit(as.character(x$gear),", "))
    y[grep('sei',y)] = 'seiner' # all seiners under one name (tuck, <20m and >20m)
    y = data.frame(table(y))
    y[order(-y$Freq),]
})
gear.act

p.nbait=ggplot(active[active$ngear>0,],aes(x=ngear))+geom_histogram(binwidth=1)+scale_y_continuous(expand=c(0,0))+facet_grid(Year~.)+labs(y='Number',x='Number of bait species')
savepng(p.nbait,"./img/",'gearnumber',c(12,10))


gear = ddply(active,c('Year'),function(x){
    y = unlist(strsplit(as.character(x$gear),", "))
    y[grep('sei',y)] = 'seiner' # all seiners under one name (tuck, <20m and >20m)
    y = data.frame(table(y))
    y[order(-y$Freq),]
})
gear

p.gear = ggplot(gear,aes(x=reorder(y,-Freq),y=Freq))+geom_bar(stat="identity")+
      facet_grid(Year~.)+
      scale_y_continuous(expand=c(0,0),limits=c(0,max(gear$Freq)*1.2))+
      geom_text(aes(y=Freq, label=Freq), vjust=-1)+
      theme(axis.text.x = element_text(angle=45,hjust=1))+
      ylab('Number')+xlab("Gear")

savepng(p.gear,"./img/",'gear',c(18,10))

for(y in Year){
    d = gear[gear$Year==y,]
    p.gear.y = ggplot(d,aes(x=reorder(y,-Freq),y=Freq))+geom_bar(stat="identity")+
        scale_y_continuous(expand=c(0,0),limits=c(0,max(d$Freq)*1.2))+
        geom_text(aes(y=Freq, label=Freq), vjust=-1)+
        theme(axis.text.x = element_text(angle=45,hjust=1))+
        ylab('Number')+xlab("Gear")
    
    savepng(p.gear.y,paste0("./img/",y,"/"),'gear',c(10,6))
}
# by province
gear.prov = ddply(active,c('Year','prov'),function(x){
    y = unlist(strsplit(as.character(x$gear),", "))
    y[grep('sei',y)] = 'seiner' # all seiners under one name (tuck, <20m and >20m)
    y = data.frame(table(y))
    y[order(-y$Freq),]
})
gear.prov

p.gear.prov = ggplot(gear.prov,aes(x=reorder(y,-Freq),y=Freq,fill=prov))+geom_bar(stat="identity")+
    facet_grid(Year~.)+
    scale_y_continuous(expand=c(0,0),limits=c(0,max(gear$Freq)*1.2))+
    theme(axis.text.x = element_text(angle=45,hjust=1))+
    ylab('Number')+xlab("Gear")+
    scale_fill_viridis_d()

savepng(p.gear.prov,"./img/",'gear.prov',c(18,10))

for(y in Year){
    d = gear.prov[gear.prov$Year==y,]
    d = d[!d$prov=='NA',]
    tot = ddply(d,c('Year','y'),summarise,Freq=sum(Freq))
    tot$prov = 'TOT'
    d = rbind(d,tot)
    p.gear.prov.y = ggplot(d,aes(x=reorder(y,-Freq),y=Freq))+geom_bar(stat="identity")+
        facet_wrap(~prov,ncol=1)+
        scale_y_continuous(expand=c(0,0),limits=c(0,max(d$Freq)*1.2))+
        geom_text(aes(y=Freq, label=Freq), vjust=-0.3,size=2.8,col='grey10')+
        theme(axis.text.x = element_text(angle=45,hjust=1))+
        ylab('Number')+xlab("Gear")
    
    savepng(p.gear.prov.y,paste0("./img/",y,"/"),'gear.prov',c(7,18))
    
    d <- translate.df(d,to='fr')
    
    p.gear.prov.y = ggplot(d,aes(x=reorder(y,-Freq),y=Freq))+geom_bar(stat="identity")+
        facet_wrap(~prov,ncol=1)+
        scale_y_continuous(expand=c(0,0),limits=c(0,max(d$Freq)*1.2))+
        geom_text(aes(y=Freq, label=Freq), vjust=-0.3,size=2.8,col='grey10')+
        theme(axis.text.x = element_text(angle=45,hjust=1))+
        ylab('Nombre')+xlab("Engin")
    
    savepng(p.gear.prov.y,paste0("./img/",y,"/FR/"),'gear.prov',c(7,18))
}

#### BAIT --------------------------------------------------------------------------------
active$nbait = do.call('c',lapply(strsplit(as.character(active$bait),", "),function(x)length(x[!is.na(x)])))  
get.stats(active[active$nbait>0,],'nbait')

p.nbait=ggplot(active[active$nbait>0,],aes(x=nbait))+geom_histogram(binwidth=1)+scale_y_continuous(expand=c(0,0))+facet_grid(Year~.)+labs(y='Number',x='Number of bait species')
savepng(p.nbait,"./img/",'baitnumber',c(12,10))

bait = ddply(active,c('Year'),function(x){
    y = unlist(strsplit(as.character(x$bait),", "))
    y = data.frame(table(y))
    y[order(-y$Freq),]
})
bait

p.bait = ggplot(bait,aes(x=reorder(y,-Freq),y=Freq))+geom_bar(stat="identity")+
    facet_grid(Year~.)+
    scale_y_continuous(expand=c(0,0),limits=c(0,max(bait$Freq)*1.2))+
    geom_text(aes(y=Freq, label=Freq), vjust=-1)+
    theme(axis.text.x = element_text(angle=45,hjust=1))+
    ylab('Number')+xlab("Bait")

savepng(p.bait,"./img/",'bait',c(18,10))

for(y in Year){
    d = bait[bait$Year==y,]
    p.bait.y = ggplot(d,aes(x=reorder(y,-Freq),y=Freq))+geom_bar(stat="identity")+
        scale_y_continuous(expand=c(0,0),limits=c(0,max(d$Freq)*1.2))+
        geom_text(aes(y=Freq, label=Freq), vjust=-1)+
        theme(axis.text.x = element_text(angle=45,hjust=1))+
        ylab('Number')+xlab("Bait")
    x <- translate.word(x,'TOTAL','TOTALE')
    savepng(p.bait.y,paste0("./img/",y,"/"),'bait',c(10,6))
}

# by province
bait.prov = ddply(active,c('Year','prov'),function(x){
    y = unlist(strsplit(as.character(x$bait),", "))
    y = data.frame(table(y))
    y = y[order(-y$Freq),]
    if(is.null(nrow(y))){y=data.frame(y=NULL,Freq=NULL)}
    return(y)
})
bait.prov

p.bait.prov = ggplot(bait.prov,aes(x=reorder(y,-Freq),y=Freq,fill=prov))+geom_bar(stat="identity")+
    facet_grid(Year~.)+
    scale_y_continuous(expand=c(0,0),limits=c(0,max(bait$Freq)*1.1))+
    theme(axis.text.x = element_text(angle=45,hjust=1))+
    ylab('Number')+xlab("Bait")+
    scale_fill_viridis_d()

savepng(p.bait.prov,"./img/",'bait.prov',c(18,10))

for(y in Year){
    d = bait.prov[bait.prov$Year==y,]
    d = d[!d$prov=='NA',]
    tot = ddply(d,c('Year','y'),summarise,Freq=sum(Freq))
    tot$prov = 'TOT'
    d = rbind(d,tot)
    p.bait.prov.y <- ggplot(d,aes(x=reorder(y,-Freq),y=Freq))+geom_bar(stat="identity")+
        facet_wrap(~prov,ncol=1)+
        scale_y_continuous(expand=c(0,0),limits=c(0,max(d$Freq)*1.2))+
        geom_text(aes(y=Freq, label=Freq), vjust=-0.3,size=2.8,col='grey10')+
        theme(axis.text.x = element_text(angle=45,hjust=1))+
        ylab('Number')+xlab("Bait")
    savepng(p.bait.prov.y,paste0("./img/",y,"/"),'bait.prov',c(7,18))
    
   d <- translate.df(d,to='fr')
    
    p.bait.prov.y <- ggplot(d,aes(x=reorder(y,-Freq),y=Freq))+geom_bar(stat="identity")+
        facet_wrap(~prov,ncol=1)+
        scale_y_continuous(expand=c(0,0),limits=c(0,max(d$Freq)*1.2))+
        geom_text(aes(y=Freq, label=Freq), vjust=-0.3,size=2.8,col='grey10')+
        theme(axis.text.x = element_text(angle=45,hjust=1))+
        ylab('Nombre')+xlab("Appât")
    savepng(p.bait.prov.y,paste0("./img/",y,"/FR/"),'bait.prov',c(7,18))
}

#### CATCH: BY ACTIVITY --------------------------------------------------------------------------------
ddply(active,c('Year','activity'),summarise,mean=mean(catch),sd=sd(catch),max=max(catch),min=min(catch))

p.catch.act = ggplot(active,aes(x=catch/1000,fill=activity))+
    geom_histogram(binwidth=10)+
    facet_grid(Year~.)+
    labs(x='Catch (t)',y='Number')+
    scale_fill_viridis_d()

savepng(p.catch.act,"./img/",'catch.activity',c(18,10))

catch.act = ddply(active,c('Year','activity'),summarise,catch=round(sum(catch)/1000,2))
cols = c("yellowgreen","violetred","orange",'grey',"slateblue")

for(y in Year){
    x = active[active$Year==y,]
    x$catch = x$catch/1000
    if(length(unique(x$activity))==4) mycol=cols[-4] else mycol=cols #grey if there is 'educational'
    # overall
    savepng(pieplot(x,value='catch',id='activity',width=c(0.6,0.4),col=mycol,repel=TRUE),
            paste0("./img/",y,"/"),'catch.act',c(12,8))
    
    # by province
    savepng(pieplot(x,value='catch',id='activity',facet='prov',total=TRUE,ncol=3,col=mycol,repel=TRUE),
            paste0("./img/",y,"/"),'catch.act.prov',c(26,20),res=500)
    
    # by province FR
    x <- translate.df(x,to='fr')
    savepng(pieplot(x,value='catch',id='activity',facet='prov',total=TRUE,ncol=3,col=mycol,repel=TRUE),
            paste0("./img/",y,"/FR/"),'catch.act.prov',c(26,20),res=500)
}

#### CATCH: BY DECLARED VS UNDECLARED --------------------------------------------------------------------------------
decl = active[,c('Year','prov','catch','declared.perc','activity')]
decl$Declared = decl$catch*(decl$declared.perc/100)
decl$Undeclared = decl$catch*(1-decl$declared.perc/100)
decl$Unknown = ifelse(is.na(decl$declared.perc),decl$catch,0)
decl[is.na(decl$declared.perc),c('Declared','Undeclared')] = 0

tab1 = ddply(decl,c('Year','prov'),summarise,decl=round(sum(Declared)/1000),undecl=round(sum(Undeclared)/1000))
tab1[tab1$Year==2018,]
tab2 = ddply(decl,c('Year'),summarise,decl=round(sum(Declared)/1000),undecl=round(sum(Undeclared)/1000),n=length(Declared))


declm = melt(decl[,c('Year','prov','Declared','Undeclared','Unknown')],c('Year','prov'),variable.name = 'declared',value.name = 'catch')
myN = ddply(decl,c('Year','prov'),summarise,n=length(prov))


p.catch.decl.prov = ggplot(decl,aes(x=declared.perc,fill=prov))+
    geom_histogram(binwidth=10)+
    facet_grid(Year~.)+
    labs(x='% declared',y='Number')+
    scale_fill_viridis_d()
p.catch.decl.act = ggplot(decl,aes(x=declared.perc,fill=activity))+
    geom_histogram(binwidth=10)+
    facet_grid(Year~.)+
    labs(x='% declared',y='Number')+
    scale_fill_viridis_d()

savepng(p.catch.decl.prov,"./img/",'catch.declared.prov',c(18,10))
savepng(p.catch.decl.act,"./img/",'catch.declared.act',c(18,10))

cols = c("darkolivegreen4","coral3",'darkgrey')

for(y in Year){
    x = declm[declm$Year==y,]
    x$catch = x$catch/1000
    Nlabs = myN[myN$Year==y,]
    Nlabs = c(Nlabs[!Nlabs$prov=='NA','n'],sum(Nlabs$n))
    # overall
    savepng(pieplot(x,value='catch',id='declared',width=c(0.6,0.4),col=cols,repel = TRUE),
            paste0("./img/",y,"/"),'catch.decl',c(12,8))
    
    # by province
    savepng(pieplot(x,value='catch',id='declared',facet='prov',total=TRUE,ncol=3,col=cols,N=Nlabs,repel=TRUE),
            paste0("./img/",y,"/"),'catch.decl.prov',c(26,20),res=500)
    
    # by province FR
    x <- translate.df(x,to='fr')
    savepng(pieplot(x,value='catch',id='declared',facet='prov',total=TRUE,ncol=3,col=cols,N=Nlabs,repel=TRUE),
            paste0("./img/",y,"/FR/"),'catch.decl.prov',c(26,20),res=500)
}

#### CATCH: BY BAIT/NOT BAIT --------------------------------------------------------------------------------
bait = active[,c('Year','prov','catch','bait_use.perc','activity')]
bait$Bait = bait$catch*(bait$bait_use.perc/100)
bait$Other = bait$catch*(1-bait$bait_use.perc/100)
bait$Unknown = ifelse(is.na(bait$bait_use.perc),bait$catch,0)
bait[is.na(bait$bait_use.perc),c('Bait','Other')] = 0

baitm = melt(bait[,c('Year','prov','Bait','Other','Unknown')],c('Year','prov'),variable.name = 'bait',value.name = 'catch')
myN = ddply(bait,c('Year','prov'),summarise,n=length(prov))


p.catch.bait.prov = ggplot(bait,aes(x=bait_use.perc,fill=prov))+
    geom_histogram(binwidth=10)+
    facet_grid(Year~.)+
    labs(x='% bait use',y='Number')+
    scale_fill_viridis_d()
p.catch.bait.act = ggplot(bait,aes(x=bait_use.perc,fill=activity))+
    geom_histogram(binwidth=10)+
    facet_grid(Year~.)+
    labs(x='% bait use',y='Number')+
    scale_fill_viridis_d()

savepng(p.catch.bait.prov,"./img/",'catch.bait.prov',c(18,10))
savepng(p.catch.bait.act,"./img/",'catch.bait.act',c(18,10))

cols = c("darkolivegreen4","darkolivegreen1",'darkgrey')

for(y in Year){
    x = baitm[baitm$Year==y,]
    x$catch = x$catch/1000
    Nlabs = myN[myN$Year==y,]
    Nlabs = c(Nlabs[!Nlabs$prov=='NA','n'],sum(Nlabs$n))
    # overall
    savepng(pieplot(x,value='catch',id='bait',width=c(0.6,0.4),col=cols,repel=TRUE),
            paste0("./img/",y,"/"),'catch.bait',c(12,8))
    
    # by province
    savepng(pieplot(x,value='catch',id='bait',facet='prov',total=TRUE,ncol=3,col=cols,N=Nlabs,repel=TRUE),
            paste0("./img/",y,"/"),'catch.bait.prov',c(26,20),res=500)
    
    # by province FR
    x <- translate.df(x,to='fr')
    savepng(pieplot(x,value='catch',id='bait',facet='prov',total=TRUE,ncol=3,col=cols,N=Nlabs,repel=TRUE),
            paste0("./img/",y,"/FR/"),'catch.bait.prov',c(26,20),res=500)
}


#### CATCH: BY DISCARDED/NOT DISCARDED --------------------------------------------------------------------------------
dis = active[,c('Year','prov','catch','discards2','activity')]

dis.summary = data.frame(metric=c('% of fishers who indicated catch but no discards',
                    'Total discards',
                    'Total catch',
                    '% discards of total catch'),value=NA)

dis.summary = ddply(dis,c('Year'),function(x){
    dis.summary[1,2] = round(nrow(x[!is.na(x$discards2),])/nrow(x)*100)
    dis.summary[2,2] = round(sum(x$discards2,na.rm=T)/1000,digits=2)
    dis.summary[3,2] = round(sum(x$catch,na.rm=TRUE)/1000,digits=2)
    dis.summary[4,2] = round(sum(x$discards2,na.rm=T)/sum(x$catch,na.rm=TRUE)*100,digits=2) 
    return(dis.summary)
})
dis.summary

dis=dis[dis$discards2!=0 & !is.na(dis$discards2),]   # this is important on figures!!! if zero admitted reallytall first bar
topdiscards = dlply(dis,c('Year'),summarise,maxN(discards2,3)/1000)
active[active$discards2 %in% maxN(dis$discards2,3) & !is.na(active$discards2),]

p.dis.act = ggplot(dis,aes(x=discards2/1000))+geom_histogram(bins=30,aes(fill=activity))+xlab('discards')+
    facet_grid(Year~.)+
    scale_fill_viridis_d()+
    ggtitle("Discards >0")+
    labs(y='Number',x='Discarded (t)')+
    geom_text(data=dis.summary[dis.summary$metric=='Total discards',],aes(x=0,y=90,label=paste0(value,'t')))

p.dis.prov = ggplot(dis,aes(x=discards2/1000))+geom_histogram(bins=30,aes(fill=prov))+xlab('discards')+
    facet_grid(Year~.)+
    scale_fill_viridis_d()+
    ggtitle("Discards >0")+
    labs(y='Number',x='Discarded (t)')+
    geom_text(data=dis.summary[dis.summary$metric=='Total discards',],aes(x=0,y=90,label=paste0(value,'t')))

savepng(p.dis.act,"./img/",'dis.act',c(18,10))
savepng(p.dis.prov,"./img/",'dis.prov',c(18,10))

#### EXPERIENCE --------------------------------------------------------------------------------
p.exp = ggplot(active,aes(x=experience,fill=activity))+
    facet_grid(Year~.)+
    geom_histogram(binwidth = 5)+
    labs(x='Years of experience',y='Number')+
    scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0),limits=c(0,100))+
    scale_fill_viridis_d()

savepng(p.exp,"./img/",'exp.prov',c(18,10))

#### PRIOR PARTICIPATION --------------------------------------------------------------------------------
col.before = c('darkgrey',"coral3","darkolivegreen4")

for(y in 2018){
    x = active[active$Year==y,]
    
    # overall
    savepng(pieplot(x,id='particip_before',width=c(0.6,0.4),col=col.before,repel=TRUE),
            paste0("./img/",y,"/"),'prior',c(12,8))
    
    # by province
    savepng(pieplot(x,id='particip_before',facet='prov',total=TRUE,ncol=3,col=col.before,repel=TRUE),
            paste0("./img/",y,"/"),'prior.prov',c(26,20),res=500)
    
    # by province FR
    x <- translate.df(x,to='fr')
    savepng(pieplot(x,id='particip_before',facet='prov',total=TRUE,ncol=3,col=col.before,repel=TRUE),
            paste0("./img/",y,"/FR/"),'prior.prov',c(26,20),res=500)
}


########### SPATIAL PLOTS ###################################################################################
### INFO:
### LCCs are from Hugo Bourdage (idem mackerel egg survey)
### coastline is from GSHHG

spat=active[!(is.na(active$lat)|is.na(active$lon)),]

#### BASE MAP --------------------------------------------------------------------------------
#** projections
LCC <- paste("+proj=lcc",
             " +lat_1=",48, " +lat_2=",50,
             " +lat_0=",46.5," +lon_0=", -70,
             " +x_0=", 0, " +y_0=",0,
             " +ellps=", "clrk66", " +units=", "km",
             " +no_defs +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat", sep="")
LL <- "+proj=longlat +datum=NAD27 +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat"


#** shape file (coast line)
       # world <- shapefile("C:/Users/vanbe/Desktop/PhD/part2/DATA/shapes/best/GSHHS_f_L1.shp")
       # eCAN <- crop(world, extent(-70, -50, 38, 54))
       # plot(eCAN)
       # writeOGR(eCAN, dsn = '.', layer = 'eCAN', driver = "ESRI Shapefile")
eCAN = shapefile(paste0(getwd(),"/shape/eCAN.shp"))

eCAN = spTransform(eCAN, CRS(LL))
eCAN = gSimplify(eCAN, tol = 0.01) # I recropped the shape for plotting because with the new projection it is skewed
eCAN = crop(eCAN, extent(-69, -52, 40, 52.5))
eCANgg = fortify(eCAN) # dataframe for ggplot

#** spatial data 
coordinates(spat)=~lon+lat  #sp
proj4string(spat) <- CRS("+proj=longlat +grilleum=NAD27 +ellps=clrk66 +nadgrilles=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.grille")
spat = spTransform(spat, CRS(LL))
spat@coords = round(spat@coords, digits=5)
colnames(spat@coords)<-c("x","y")
spatgg = data.frame(spat)

#** Background layer
# create the breaks- and label vectors
ewbrks <- seq(-68,-56,4)
nsbrks <- seq(44,52,2)
ewlbls <- unlist(lapply(ewbrks, function(x) ifelse(x < 0, paste0(-x, "°E"), ifelse(x > 0, paste0(x, "°W"),x))))
nslbls <- unlist(lapply(nsbrks, function(x) ifelse(x < 0, paste0(-x, "°S"), ifelse(x > 0, paste0(x, "°N"),x))))

#** create the base map
p.CANmap=ggplot(data=eCANgg, aes(x=long, y=lat)) +geom_polygon(aes(group=group),fill='lightgrey',col='grey')+
  coord_equal()+ylab('')+xlab('')+
  scale_x_continuous(breaks = ewbrks, labels = ewlbls, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, labels = nslbls, expand = c(0, 0)) +
  theme(axis.text = element_text(size=12))

#### PROVINCE CHECK --------------------------------------------------------------------------------
p.CANmap+geom_point(data=spatgg, aes(x=x, y=y,col=prov),size=2,alpha=0.5)

#### NUMBER --------------------------------------------------------------------------------
nr=ddply(spatgg,c('Year','port_stand','x','y','prov'),summarise,nr=length(form))

p.map.nr = p.CANmap+
    geom_point(data=nr, aes(x=x, y=y,size=nr,col=as.factor(Year)),alpha=0.5)+
    theme(legend.position=c(0.8,0.2),legend.justification="center")+
    scale_color_viridis_d()+
    labs(size='',col='')

savepng(p.map.nr,"./img/",'map.nr',c(20,12))

for(y in Year){
    x = nr[nr$Year==y,]
    breaks = seq(min(x$nr),max(x$nr), length.out = 3)
    p.map.nr.y = p.CANmap+
        geom_point(data=x, aes(x=x, y=y,size=nr),alpha=0.5,col='slateblue')+
        theme(legend.position=c(0.8,0.2),legend.justification="center")+
        scale_color_viridis_d()+
        scale_size_continuous(breaks=breaks)+
        labs(size='',col='')
    
    savepng(p.map.nr.y,paste0("./img/",y,"/"),'map.nr',c(20,12))
}


#### SIZE --------------------------------------------------------------------------------
cols=c("red","green","orange")
names(cols)=c("No","Yes","I am unsure")
small=ddply(spatgg,c("port_stand","small_mackerel","x","y"),summarise,n=length(small_mackerel))
small=small[!is.na(small$small_mackerel),]
small=small[small$small_mackerel %in% names(cols),]
bar.plot_list=ls()
bar_annotation_list=ls()

bar.plot_list <-
  lapply(unique(small$port_stand), function(i) {
    d=small[small$port_stand == i,]
    col=cols[d$small_mackerel]
    gt_plot <- ggplotGrob(
      ggplot(d)+geom_bar(aes("a",n,group=small_mackerel, fill = small_mackerel),stat='identity') +
        labs(x = NULL, y = NULL) +
        scale_y_continuous(limits=c(0,max(nr$nr)),expand = c(0,0))+
        theme(legend.position = "none", rect = element_blank(),
              line = element_blank(), text = element_blank())+
        scale_fill_manual(values = col)
    )
    panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
    gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
  })

bar_annotation_list <- lapply(1:length(unique(small$port_stand)), function(i){
  annotation_custom(bar.plot_list[[i]],
                    xmin = nr[i,"x"] - 0.1,
                    xmax = nr[i,"x"] + 0.1,
                    ymin = nr[i,"y"],
                    ymax = nr[i,"y"] + 0.4) })

leg.plot=ggplot(small,aes(x=x,y=y,col=small_mackerel))+geom_point()+scale_color_manual(values = cols)+labs(col='')
leg <- gtable_filter(ggplot_gtable(ggplot_build(leg.plot)), "guide-box")

small_plot <- Reduce(`+`, bar_annotation_list, p.CANmap)
small_plot <- small_plot + annotation_custom(leg, xmin=-55, xmax=-58, ymin=44, ymax=46)

png(filename=paste0("./img/2016_old/spat_small_old.png"),units="cm",res=300,width=20,height=12)
small_plot
dev.off()

#### GEAR --------------------------------------------------------------------------------
gear = spatgg[!is.na(spatgg$gear),c("Year","gear","port_stand","x","y")]
gear = ddply(gear,c('Year','port_stand','x','y'),summarise,gillnet=count.char("illn",gear),jigger=count.char("igger",gear),rod_and_line=count.char("od ",gear),trap=count.char("rap",gear),seiner=count.char("eine",gear),handline=count.char("and",gear))
gear = melt(gear,id=c('Year','port_stand','x','y'),variable.name='gear',value.name='n')

cols = rainbow(length(unique(gear$gear)))
names(cols) = unique(gear$gear)

for(y in Year){
    x=gear[gear$Year==y,]
    bar.plot_list =
      lapply(unique(x$port_stand), function(i) {
        d=x[x$port_stand == i,]
        col=cols[d$gear]
        gt_plot = ggplotGrob(
          ggplot(d)+geom_bar(aes("a",n,group=gear, fill = gear),stat='identity') +
            labs(x = NULL, y = NULL) +
            scale_y_continuous(limits=c(0,max(nr$nr)),expand = c(0,0))+
            theme(legend.position = "none", rect = element_blank(),
                  line = element_blank(), text = element_blank())+
            scale_fill_manual(values = col)
        )
        panel_coords = gt_plot$layout[gt_plot$layout$name == "panel",]
        gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
      })
    
    nx = nr[nr$Year==y,]
    bar_annotation_list = lapply(1:length(unique(x$port_stand)), function(i){
      annotation_custom(bar.plot_list[[i]],
                        xmin = nx[i,"x"] - 0.1,
                        xmax = nx[i,"x"] + 0.1,
                        ymin = nx[i,"y"],
                        ymax = nx[i,"y"] + 0.6) })
    
    leg.plot = ggplot(x,aes(x=x,y=y,col=gear))+geom_point()+scale_color_manual(values = cols)+theme(legend.background = element_rect(fill = "transparent", colour = NA))
    leg = gtable_filter(ggplot_gtable(ggplot_build(leg.plot)), "guide-box")
    
    p.map.gear = Reduce(`+`, bar_annotation_list, p.CANmap)
    p.map.gear = p.map.gear + annotation_custom(leg, xmin=-55, xmax=-58, ymin=45, ymax=46)
    
    savepng(p.map.gear,paste0("./img/",y,"/"),'map.gear',c(20,16))
}

#### GEAR --------------------------------------------------------------------------------
bait = spatgg[!is.na(spatgg$bait),c("Year","bait","port_stand","x","y")]
bait = ddply(bait,c('Year','port_stand','x','y'),summarise,
           tuna=count.char("una",bait),
           lobster=count.char("obst",bait),
           snow_crab=count.char("now",bait),
           halibut=count.char("but",bait),
           rock_crab=count.char("ock",bait),
           striped_bass=count.char("iped",bait),
           groundfish=count.char("ound",bait))
bait = melt(bait,id=c('Year','port_stand','x','y'),variable.name='bait',value.name='n')


cols = rainbow(length(unique(bait$bait)))
names(cols) = unique(bait$bait)

for(y in Year){
    x=bait[bait$Year==y,]
    bar.plot_list =
      lapply(unique(x$port_stand), function(i) {
        d=x[x$port_stand == i,]
        col=cols[d$bait]
        gt_plot <- ggplotGrob(
          ggplot(d)+geom_bar(aes("a",n,group=bait, fill = bait),stat='identity') +
            labs(x = NULL, y = NULL) +
            scale_y_continuous(limits=c(0,max(nr$nr)),expand = c(0,0))+
            theme(legend.position = "none", rect = element_blank(),
                  line = element_blank(), text = element_blank())+
            scale_fill_manual(values = col)
        )
        panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
        gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
      })
    
    bar_annotation_list = lapply(1:length(unique(x$port_stand)), function(i){
      annotation_custom(bar.plot_list[[i]],
                        xmin = nr[i,"x"] - 0.1,
                        xmax = nr[i,"x"] + 0.1,
                        ymin = nr[i,"y"],
                        ymax = nr[i,"y"] + 0.6) })
    
    leg.plot = ggplot(x,aes(x=x,y=y,col=bait))+geom_point()+scale_color_manual(values = cols)+theme(legend.background = element_rect(fill = "transparent", colour = NA))
    leg = gtable_filter(ggplot_gtable(ggplot_build(leg.plot)), "guide-box")
    
    p.map.bait <- Reduce(`+`, bar_annotation_list, p.CANmap)
    p.map.bait <- p.map.bait + annotation_custom(leg, xmin=-55, xmax=-57, ymin=45.5, ymax=46)
    
    savepng(p.map.bait,paste0("./img/",y,"/"),'map.bait',c(20,16))
}
