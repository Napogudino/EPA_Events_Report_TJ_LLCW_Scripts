# Calculate precipitation during events for raingages in San Diego, to compare with LLCW data from 2014-2017.


setwd("G:/mydocuments/SDSU/research/tijuana_watershed/writeups/EPA_events_report/EPA_Events_Report_TJ_LLCW_Scripts")
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

fname = "storm_start_end_dates.csv"

storm.dates = read.csv(fname)
storm.start = as.Date(storm.dates$Start)
storm.end = as.Date(storm.dates$End)

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

#  Function to summarize storms for a given raingage
storm.sum <- function(P) {
  psum = rep(NA,length(storm.start))
  for (i in 1:length(storm.start)){
    psum[i]=sum(P[(P$Date<=storm.end[i])&(P$Date>=storm.start[i]),"PRCP.0.1MM"])/10
  }
  return(psum)
}

# Load P data
# IB.NOF requires custom:
P.IB.NOF = read.csv("climate_daily_IB_Napo.csv") 
P.IB.NOF$Date = as.Date(strptime(paste0(P.IB.NOF$Month, "/", P.IB.NOF$Day, "/", P.IB.NOF$Year),format="%m/%d/%Y"))
P.IB.NOF$month = as.numeric(format(P.IB.NOF$Date,"%m"))
P.IB.NOF$year = as.numeric(format(P.IB.NOF$Date, "%Y"))
P.IB.NOF$wy = P.IB.NOF$Year
P.IB.NOF$wy[P.IB.NOF$month>=10] = P.IB.NOF$wy[P.IB.NOF$month>=10]+1
P.IB.NOF$PRCP.0.1MM = P.IB.NOF$Precip*10
P.IB.storms = storm.sum(P.IB.NOF)

indir.Lind = "G:/mydocuments/SDSU/research/tijuana_watershed/precipitation/NOAA_lindbergh_field/"
fname.Lind = "125207_lindbergh_field_to_2017.csv"
P.Lind = format.precip(indir.Lind,fname.Lind)
P.Lind.storms = storm.sum(P.Lind)

indir.SDBF = "G:/mydocuments/SDSU/research/tijuana_watershed/precipitation/NOAA_san_diego_brown_field/"
fname.SDBF = "san_diego_brown_field_1998_2017.csv"
P.SDBF = format.precip(indir.SDBF,fname.SDBF)
P.SDBF.storms = storm.sum(P.SDBF)

indir.IB33 = "G:/mydocuments/SDSU/research/tijuana_watershed/precipitation/NOAA_imperial_beach_33/"
fname.IB33 = "US1CASD0003_IB33_2008_2017.csv"
P.IB33 = format.precip(indir.IB33,fname.IB33)
P.IB33.storms = storm.sum(P.IB33)

storm.sum = data.frame(storm.start,storm.end,P.IB.NOF=P.IB.storms,P.Lind=P.Lind.storms,P.SDBF=P.SDBF.storms,P.IB33=P.IB33.storms,RG.HM=storm.dates$RG.HM.mm)
#dev.new()
plot(storm.sum$RG.HM,storm.sum$P.SDBF,pch=20,cex=1.5,xlim=c(0,100),ylim=c(0,100),xlab="Rainfall, Hormiguitas (mm)",ylab="Rainfall other (mm)")
points(storm.sum$RG.HM,storm.sum$P.Lind,pch=20,cex=1.5,col="grey")
points(storm.sum$RG.HM,storm.sum$P.IB33,pch=22,cex=1.5)
abline(0,1,lty=2)
legend("topleft",c("SDBF","Lind","IB3.3"),pch=c(20,20,22),col=c("black","grey","black"),bty="n",pt.cex=c(1.5,1.5,1))

# Calculate total bias

