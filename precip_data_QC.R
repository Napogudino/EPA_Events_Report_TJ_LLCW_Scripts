#  Quality check precipitation data

setwd("G:/mydocuments/SDSU/research/tijuana_watershed/writeups/EPA_events_report/EPA_Events_Report_TJ_LLCW_Scripts")
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

format.precip <- function(indir,fname){
  x = read.csv(paste0(indir,fname))
  x$Date = as.Date(strptime(x$DATE,format="%Y%m%d"))
  x$Date = as.Date(strptime(x$DATE,format="%Y%m%d"))
  x$month = as.numeric(format(x$Date,"%m"))
  x$year = as.numeric(format(x$Date,"%Y"))
  x$wy = x$year
  x$wy[x$month>=10] = x$wy[x$month>=10]+1
  xout = x[!is.na(x$PRCP.0.1MM),]
  return(xout)
}

summarize.precip <- function(x){
  # Calculate total precip
  x.ann.P.wy = aggregate(x$PRCP.0.1MM,by=list(x$wy),FUN="sum")
  x.ann.count.wy = aggregate(x$PRCP.0.1MM,by=list(x$wy),FUN="length")
  xout = data.frame(Year=as.numeric(as.character(x.ann.P.wy$Group.1)),P.mm=x.ann.P.wy$x/10,P.count=x.ann.count.wy$x)
  return(xout)
}

P.COMB = read.csv("climate_daily_IB_Napo.csv")
    #  Climate_daily_IB_Napo is combination of IB.NOR and Hormiguitas
P.COMB$Date = as.Date(strptime(paste0(P.COMB$Month, "/", P.COMB$Day, "/", P.COMB$Year),format="%m/%d/%Y"))
P.COMB$month = as.numeric(format(P.COMB$Date,"%m"))
P.COMB$year = as.numeric(format(P.COMB$Date, "%Y"))
P.COMB$wy = P.COMB$Year
P.COMB$wy[P.COMB$month>=10] = P.COMB$wy[P.COMB$month>=10]+1

#sum total precip by calendar and wy
# IB, from Napo (Imperial Beach Naval Outlying Field)
COMB.annual.precip.mm.year = aggregate(as.numeric(as.character(P.COMB$Precip)), by= list(as.character(P.COMB$year)), FUN=sum) #sum by water year
COMB.annual.count.by.year = aggregate(as.numeric(as.character(P.COMB$Precip)), by= list(as.character(P.COMB$year)), FUN=length) #sum by water year
COMB.annual.precip.mm.year$P.cm = IB.annual.precip.mm.year$x/10
COMB.annual.precip.mm.wy = aggregate(as.numeric(as.character(P.COMB$Precip)), by= list(as.character(P.COMB$wy)), FUN=sum)
COMB.annual.count.by.wy = aggregate(as.numeric(as.character(P.COMB$Precip)), by= list(as.character(P.COMB$wy)), FUN=length) #sum by water year

P.COMB.sum = data.frame(Year=as.numeric(as.character(COMB.annual.precip.mm.wy$Group.1)),P.mm=COMB.annual.precip.mm.wy$x/10,P.count=COMB.annual.count.by.wy$x)

# read in Lindbergh Field 
indir.Lind = "G:/mydocuments/SDSU/research/tijuana_watershed/precipitation/NOAA_lindbergh_field/"
fname.Lind = "125207_lindbergh_field_to_2017.csv"
P.Lind = format.precip(indir.Lind,fname.Lind)
P.Lind.sum = summarize.precip(x=P.Lind)


#  Summarize IB, Ream Field
indir.IB.ream = "G:/mydocuments/SDSU/research/tijuana_watershed/precipitation/NOAA_imperial_beach_ream_field_NAS/"
fname.IB.ream = "imperial_beach_ream_field_daily_1945_2012.csv"
P.IBream = format.precip(indir.IB.ream,fname.IB.ream)
P.IBream.sum = summarize.precip(x=P.IBream)

#  Summarize Chula Vista
indir.Chula.Vista = "G:/mydocuments/SDSU/research/tijuana_watershed/precipitation/NOAA_chula_vista/"
fname.Chula.Vista = "chula_vista_1918_2012.csv"
P.Chula.Vista = format.precip(indir.Chula.Vista,fname.Chula.Vista)
P.Chula.Vista.sum = summarize.precip(x=P.Chula.Vista)

#  Summarize Jamul
indir.Jamul = "G:/mydocuments/SDSU/research/tijuana_watershed/precipitation/NOAA_jamul/"
fname.Jamul = "san_diego_jamul_daily_precip_1998_2012.csv"
P.Jamul = format.precip(indir.Jamul,fname.Jamul)
P.Jamul.sum = summarize.precip(x=P.Jamul)

#  Summarize IB33
indir.IB33 = "G:/mydocuments/SDSU/research/tijuana_watershed/precipitation/NOAA_imperial_beach_33/"
fname.IB33 = "US1CASD0003_IB33_2008_2017.csv"
P.IB33 = format.precip(indir.IB33,fname.IB33)
P.IB33.sum = summarize.precip(x=P.IB33)

#  Summarize SD Brownfield
indir.SDBF = "G:/mydocuments/SDSU/research/tijuana_watershed/precipitation/NOAA_san_diego_brown_field/"
fname.SDBF = "san_diego_brown_field_1998_2017.csv"
P.SDBF = format.precip(indir.SDBF,fname.SDBF)
P.SDBF.sum = summarize.precip(x=P.SDBF)

# Compile into output data frame, table
yrange = range(range(P.COMB.sum$Year),range(P.IB33.sum$Year),range(P.Jamul.sum$Year),range(P.Chula.Vista.sum$Year),range(P.Lind.sum$Year))

P.all.count = data.frame(Year=seq(yrange[1],yrange[2]))
st.list = c("COMB","Lind","IBream","Jamul","IB33","Chula.Vista","SDBF")
st.list = c("Lind","IBream","Jamul","IB33","Chula.Vista","SDBF")
N.stations = length(st.list)

for (i in 1:N.stations){
  eval(parse(text=paste0("df.tmp  = P.", st.list[i],".sum")))
  match.tmp = match(df.tmp$Year,P.all.count$Year)
  P.all.count[match.tmp,i+1] = df.tmp$P.count/365
}
names(P.all.count) = c("Year",st.list)

P.all.count.1990.2017 = P.all.count[P.all.count$Year %in% seq(2005,2016),]

P.all.sum = data.frame(Year=seq(yrange[1],yrange[2]))
for (i in 1:N.stations){
  eval(parse(text=paste0("df.tmp  = P.", st.list[i],".sum")))
  match.tmp = match(df.tmp$Year,P.all.sum$Year)
  P.all.sum[match.tmp,i+1] = df.tmp$P.mm
}
names(P.all.sum) = c("Year",st.list)
P.all.sub = P.all.sum[P.all.sum$Year %in% seq(1980,2012),]


Pinvent.t = t(P.all.count.1990.2017[,2:(N.stations+1)])  # "3" skips the "COMB" station
Pinvent.t[is.na(Pinvent.t)] = 0
Pinvent.t[Pinvent.t>1]=1

colnames(Pinvent.t) = P.all.count.1990.2017$Year
grid = expand.grid(X=as.numeric(colnames(Pinvent.t)),Y=seq(1:N.stations))
grid$Z = as.vector(t(Pinvent.t))

library(latticeExtra)
col.l <- colorRampPalette(c('red', 'orange', 'yellow', 'green', 'cyan', 'blue'))

levelplot(Z ~ X*Y, data = grid, xlab="",ylab="",scales=list(y=list(at=seq(1:N.stations), 
                                                                   labels=st.list),x=list(at=colnames(Pinvent.t),labels=colnames(Pinvent.t),rot=90)),
          #colorkey=list(at=seq(0, 1, 0.2), 
          #labels=list(at=c(0, 0.3, 0.6, 0.9), 
          #            labels=c("none", "a bit", "a bit more", "a lot"))),
          col.regions=col.l) +
  layer(panel.text(X, Y, round(Z, 2),cex=0.7), data = grid)

# Compare 

dev.new()
plot(P.all.sub$P.Lind,P.all.sub$P.SDBF,xlab="P.Lind, mm",ylab="P.SDBF or P.COMB",pch=20,xlim=c(0,600),ylim=c(0,600))
points(P.all.sub$P.Lind,P.all.sub$P.COMB*10,col="red",pch=20)
abline(0,1)

IB.annual.precip.mm.wy$Lind.P.wy.COMB.wyrs = Lind.annual.precip.mm.wy[match(IB.annual.precip.mm.wy$Group.1,Lind.annual.precip.mm.wy$Group.1),"x"]
IB.noNA = IB.annual.precip.mm.wy[!is.na(IB.annual.precip.mm.wy$Lind.P.wy.COMB.wyrs),]
plot(IB.annual.precip.mm.wy$x,IB.annual.precip.mm.wy$Lind.P.wy.COMB.wyrs/10)
abline(0,1)

#  double mass charts
P.all.complete.only = P.all.sum
P.all.complete.only[P.all.count<1.0]=NA

#  COMB and Lind
dev.new()
par(mfrow=c(2,1),mar=c(2,0,0,0),oma=c(4,5,2,2))
P.COMB.and.Lind = P.all.complete.only[(!is.na(P.all.complete.only$COMB)) & (!is.na(P.all.complete.only$Lind)),]
P.COMB.and.Lind$COMB.cum = cumsum(P.COMB.and.Lind$COMB)
P.COMB.and.Lind$Lind.cum = cumsum(P.COMB.and.Lind$Lind)/10
plot(P.COMB.and.Lind$Lind.cum,P.COMB.and.Lind$COMB.cum,pch=20,cex=1.5,xlab="Cumulative precip, Lindbergh (cm)",ylab="Cumulative precip, COMB (cm)")
abline(lm(P.COMB.and.Lind$COMB.cum~(P.COMB.and.Lind$Lind.cum)))
mtext(side=2,"Cumulative precip, COMB (cm)",line=3)

#  SDBF and Lind
P.SDBF.and.Lind = P.all.complete.only[(!is.na(P.all.complete.only$SDBF)) & (!is.na(P.all.complete.only$Lind)),]
P.SDBF.and.Lind$SDBF.cum = cumsum(P.SDBF.and.Lind$SDBF)/10
P.SDBF.and.Lind$Lind.cum = cumsum(P.SDBF.and.Lind$Lind)/10
plot(P.SDBF.and.Lind$Lind.cum,P.SDBF.and.Lind$SDBF.cum,pch=20,cex=1.5,xlab="Cumulative precip, Lindbergh (cm)",ylab="Cumulative precip, SDBF (cm)")
abline(lm(P.SDBF.and.Lind$SDBF.cum~(P.SDBF.and.Lind$Lind.cum)))
mtext(side=2,"Cumulative precip, SDBF (cm)",line=3)
mtext(side=1,"Cumulative precip, Lindbergh (cm)",line=3)

# IB.33 and Lind
P.IB33.and.Lind = P.all.complete.only[(!is.na(P.all.complete.only$IB33)) & (!is.na(P.all.complete.only$Lind)),]
P.IB33.and.Lind$IB33.cum = cumsum(P.IB33.and.Lind$IB33)/10
P.IB33.and.Lind$Lind.cum = cumsum(P.IB33.and.Lind$Lind)/10
plot(P.IB33.and.Lind$Lind.cum,P.IB33.and.Lind$IB33.cum,pch=20,cex=1.5,xlab="Cumulative precip, Lindbergh (cm)",ylab="Cumulative precip, IB33 (cm)")
abline(lm(P.IB33.and.Lind$IB33.cum~(P.IB33.and.Lind$Lind.cum)))
mtext(side=2,"Cumulative precip, IB33 (cm)",line=3)
mtext(side=1,"Cumulative precip, Lindbergh (cm)",line=3)
