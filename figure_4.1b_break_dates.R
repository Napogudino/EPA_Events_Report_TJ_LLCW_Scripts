#  Read trap and P data, break and summarize P data by actual removal date.


#Set working directory to the data folder, script directory will be used if sourcing functions
setwd("G:/mydocuments/SDSU/research/tijuana_watershed/writeups/EPA_events_report/EPA_Events_Report_TJ_LLCW_Scripts")
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder




table.trap.efficiency = read.csv("sediment_loads_2006_2012_corrected_uncorrected_05182017.csv") #table with date and total excavation from TJE traps
tje.corr.2006.2012.totals = table.trap.efficiency[as.character(table.trap.efficiency$Sed_size) == "Total",] #only use annual totals

# Load precip
  # IB
P.IB = read.csv("climate_daily_IB_Napo.csv") 
P.IB$Date = as.Date(strptime(paste0(precip$Month, "/", precip$Day, "/", precip$Year),format="%m/%d/%Y"))
P.IB$month = as.numeric(format(P.IB$Date,"%m"))
P.IB$year = as.numeric(format(P.IB$Date, "%Y"))
P.IB$wy = P.IB$Year
P.IB$wy[P.IB$month>=10] = P.IB$wy[P.IB$month>=10]+1

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

  # Lindbergh Field 
# read in Lindbergh Field 
indir.Lind = "G:/mydocuments/SDSU/research/tijuana_watershed/precipitation/NOAA_lindbergh_field/"
fname.Lind = "125207_lindbergh_field.csv"
P.lind = format.precip(indir.Lind,fname.Lind)
P.lind.sum = summarize.precip(x=P.lind)

#  Summarize SD Brownfield
indir.SDBF = "G:/mydocuments/SDSU/research/tijuana_watershed/precipitation/NOAA_san_diego_brown_field/"
fname.SDBF = "san_diego_brown_field_1998_2017.csv"
P.SDBF = format.precip(indir.SDBF,fname.SDBF)
P.SDBF.sum = summarize.precip(x=P.SDBF)

#  Cut p data by removal dates
cut.dates = as.Date(tje.corr.2006.2012.totals$Removal.date)
P.IB$cut.labels = cut(P.IB$Date, cut.dates, labels=tje.corr.2006.2012.totals$Removal_WY_Date[2:length(tje.corr.2006.2012.totals$Removal_WY_Date)])

P.IB.by.cut.date = aggregate(P.IB$Precip,by=list(P.IB$cut.labels),FUN="sum")
tje.corr.2006.2012.totals$P.by.cut.date.mm = c(NA,P.IB.by.cut.date$x)

 # Cut lind P data
P.lind$cut.labels = cut(P.lind$Date, cut.dates, labels=tje.corr.2006.2012.totals$Removal_WY_Date[2:length(tje.corr.2006.2012.totals$Removal_WY_Date)])
P.lind.by.cut.date = aggregate(P.lind$PRCP.0.1MM,by=list(P.lind$cut.labels),FUN="sum")
tje.corr.2006.2012.totals$P.lind.by.cut.date.mm = c(NA,P.lind.by.cut.date$x/10)

# Cut SDBF P data
P.SDBF$cut.labels = cut(P.SDBF$Date, cut.dates, labels=tje.corr.2006.2012.totals$Removal_WY_Date[2:length(tje.corr.2006.2012.totals$Removal_WY_Date)])
P.SDBF.by.cut.date = aggregate(P.SDBF$PRCP.0.1MM,by=list(P.SDBF$cut.labels),FUN="sum")
tje.corr.2006.2012.totals$P.SDBF.by.cut.date.mm = c(NA,P.SDBF.by.cut.date$x/10)

dev.new()
par(mfrow=c(3,1),mar=c(2,0,0,0),oma=c(4,5,2,2))
plot(tje.corr.2006.2012.totals$P.by.cut.date.mm,tje.corr.2006.2012.totals$Tons_removed_uncorrected,xlab="Precipitation between cleanings, mm",ylab="Tons removed",pch=20,cex=1.5,ylim=c(0,90000),xlim=c(0,600))
points(tje.corr.2006.2012.totals$P.by.cut.date.mm,tje.corr.2006.2012.totals$Corrected_load_tons,pch=20,cex=1.5,col="grey")
text()

plot(tje.corr.2006.2012.totals$P.lind.by.cut.date.mm,tje.corr.2006.2012.totals$Tons_removed_uncorrected,xlab="Precipitation between cleanings, mm",ylab="Tons removed",pch=20,cex=1.5,ylim=c(0,90000),xlim=c(0,600))
points(tje.corr.2006.2012.totals$P.lind.by.cut.date.mm,tje.corr.2006.2012.totals$Corrected_load_tons,pch=20,cex=1.5,col="grey")

plot(tje.corr.2006.2012.totals$P.SDBF.by.cut.date.mm,tje.corr.2006.2012.totals$Tons_removed_uncorrected,xlab="Precipitation between cleanings, mm",ylab="Tons removed",pch=20,cex=1.5,ylim=c(0,90000),xlim=c(0,600))
points(tje.corr.2006.2012.totals$P.SDBF.by.cut.date.mm,tje.corr.2006.2012.totals$Corrected_load_tons,pch=20,cex=1.5,col="grey")

# Remove 2005
tje.no2005 = tje.corr.2006.2012.totals[tje.corr.2006.2012.totals$Removal_WY_Date>=2006,]
dev.new()
par(mfrow=c(2,1),mar=c(2,0,0,0),oma=c(4,5,2,2))
plot(tje.no2005$P.by.cut.date.mm,tje.no2005$Tons_removed_uncorrected,xlab="Precipitation between cleanings, mm",ylab="Tons removed",pch=20,cex=1.5,ylim=c(0,90000))
points(tje.no2005$P.by.cut.date.mm,tje.no2005$Corrected_load_tons,pch=20,cex=1.5,col="grey")
mtext(side=2,"Sediment load, tons",line=3)
plot(tje.no2005$P.lind.by.cut.date.mm,tje.no2005$Tons_removed_uncorrected,xlab="Precipitation between cleanings, mm",ylab="Tons removed",pch=20,cex=1.5,ylim=c(0,90000))
points(tje.no2005$P.lind.by.cut.date.mm,tje.no2005$Corrected_load_tons,col="grey",pch=20,cex=1.5)
mtext(side=2,"Sediment load, tons",line=3)
mtext(side=1,"Precipitation, Lindbergh, mm",line=3)

# Plot P time series and removal dates, bot IB and Lindbergh
#barplot(P.IB$Precip,P.IB$Date,space=c(0,1),width=0.5,border="black")
par(mfrow=c(2,1),mar=c(1,0,0,0),oma=c(4,5,2,2))
plot(P.IB$Date,P.IB$Precip,type="l",xlim=as.Date(c("2004-10-10","2016-12-31")))
abline(v=cut.dates,lty=2,lwd=2,col="grey")
mtext(side=2,"Daily Precipitation, mm",line=3)
plot(P.lind$Date,P.lind$PRCP/10,type="l",xlim=as.Date(c("2004-10-10","2016-12-31")))
abline(v=cut.dates,lty=2,lwd=2,col="grey")

par(mfrow=c(1,1))
plot(tje.corr.2006.2012.totals$P.by.cut.date.mm,tje.corr.2006.2012.totals$P.lind.by.cut.date.mm,xlab="P, mm, IB",ylab="P, mm, Lind")
abline(0,1)
