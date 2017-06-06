#Figure 3.2 in EPA Events Report
#SSC data from storm 1: 3/1/2014
#Summary data for Table 3.1 and 3.3
#Napoleon Gudino (CICESE) wrote original SSC calculations part of script, Kris Taniguchi (SDSU) reformatted and updated script

#Set working directory to the data folder, script directory will be used if sourcing functions
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

###############################################################################################################

#Run the events report script for storm 1 (figure_2.02_PT_2014_03_01_KTedits04172017.R) to get the appropriate Q, stage, etc.
source("../EPA_Events_Report_TJ_LLCW_Scripts/figure_2.02_storm1_PT_2014_03_01_KTedits04172017.R")

#Dataframe q.data.all is the usable observed data (timeseries of all the good data)
names(q.data.all)
date.time = as.POSIXct(q.data.all$date.time) #date.time from obs data
q.cms = as.numeric(as.character(q.data.all[,2])) #q.data.all[,2] saved as factor, change to numeric

#SSC Data --> by Napo
# Load SSC data
fname.sed = "ssc_2014-16.csv"
x.ssc = read.csv(fname.sed)
dates.ssc = strptime(x.ssc$Date,format="%m/%d/%Y, %I:%M:%S %p")

#plot Q timeseries with SSC
par(oma=c(2,2,2,2),mar = c(4, 4, 0, 2))
xlimits = c(as.POSIXct(as.POSIXct("2014-02-28 12:00:00 PST")),as.POSIXct("2014-03-01 10:00:00 PST"))
xtics = seq(xlimits[1],xlimits[2],by=3600)
xlabels = format(xtics,"%m-%d %H:%M")
plot(date.time, q.cms, type ="l", xlab = "", ylab = "Discharge (cms)", xaxt = "n",xlim=xlimits)
axis(side = 1, date.time,at=xtics,labels=xlabels,las=2)
par(new=T)
plot(dates.ssc,x.ssc$g.l,yaxt="n",xlim=xlimits,xlab="",ylab="",pch=20,cex=2)
axis(side=4) 
mtext("SSC g/l",side = 4, line = 3)

###############################################################################################################
#find discharge at time that SSC was collected

ftime = date.time
#target.time = as.POSIXct("2017-02-27 14:00:00",format="%Y-%m-%d %H:%M:%S")  # date and time # of the SSC sample
target.index = which(as.Date(format(dates.ssc,"%Y-%m-%d")) %in% as.Date(date.time))
target.times = dates.ssc[target.index]

match.data = rep(NA,times=length(target.times))
for (e in 1:length(target.times)){
  difftime = ftime-target.times[e]
  index = which(abs(difftime)==min(abs(difftime))) #absolute min time diff is when sample was taken (0 is at same time)
  match.data[e] = q.cms[index] #discharge at that time
  #q.cms[(index-10):(index+10)]
}
match.data[match.data<0.07]=0.07

###############################################################################################################

#Summary data for Table 3.1:
match.data #the discharge at time of SSC collection
ssc.date.time = c(target.times) #the date.time of SSC collecction
ssc = x.ssc$g.l[target.index]
storm = "Storm 1"
Event = c("E2", "E3")

table.3.1.export = data.frame(cbind(as.character(ssc.date.time), ssc, match.data, Event))
names(table.3.1.export) <- c("Date", "SSC (g/L)", "Q (cms)", "Event")
###############################################################################################################

#Table 3.3 Caculations
startE2 = as.POSIXct("2014-02-28 15:50",format="%Y-%m-%d %H:%M")
endE2 = as.POSIXct("2014-03-01 00:00",format="%Y-%m-%d %H:%M")

q.df.all = data.frame(date.time=date.time,q.cms=q.cms)
q.df = q.df.all[(q.df.all$date.time<endE2) & (q.df.all$date.time>startE2),]
SSC.df = data.frame(date.time=dates.ssc,SSC=x.ssc$g.l)

#total.q.m3 = total.q.obs.mm/1000*10230000 #convert to m, multiply by 10.23 km2 wtshd area or 10230000 m2
#load.g = VWM*total.q.m3*1000 #1000L = 1m3
#load.ton = load.g * 1e-6 #1 gram = 1e-6 ton

#setwd('../EPA_Events_Report_TJ_LLCW_Scripts')
#source("regression_models_SSC_vs_Q.R")

out.data = SSL.calc(Q=q.df,SSC=SSC.df)

#for table 3.3:
date = obs.summary[,1] 
event = "E2" # Event number e.g. "E1"
date.event = paste(date[1], event, sep=" ")
table.3.3.export = data.frame(event.date= date.event, NSSC=out.data$NSSC,total.q.mm=out.data$Qmm, total.q.m3=out.data$Qm3, VWM=out.data$VWM, SSL.Event.VWM=out.data$SSL.VWM.event,SSL.All.VWM=out.data$SSL.VWM.all,SSL.Rating.no.bcf=out.data$SSL.rating.wo.bcf,SSL.Rating.bcf=out.data$SSL.rating.w.bcf)

