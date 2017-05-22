#Figure 2.5
#Total event discharge vs. total precip.
#Using data from EPA Events Report Table 2.3
#Script written by Kris Taniguchi, SDSU (kristaniguchi@gmail.com).  Updated by TBiggs, 2017-05-22.

#Set working directory to the data folder, script directory will be used if sourcing functions
# getwd() #the directory where the script is saved
# setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

###############################################################################################################

#Read in table 2.3 summary table of observed events
# data = read.csv("summary_table_obs_allevents.csv")
# Github code here:
data = read.csv("https://raw.githubusercontent.com/kristaniguchi/EPA_Events_Report_TJ_LLCW_Data/master/summary_table_obs_allevents.csv")

###############################################################################################################

#  Add SCS CNs
pvec.mm = seq(0,80,by=1)
pvec.in = pvec.mm/25.4

S.CN85 = 1000/85 - 10
Q.CN85 = ((pvec.in-0.2*S.CN85)^2)/(pvec.in + 0.8*S.CN85)*25.4
Q.CN85[pvec.in<0.2*S.CN85] = 0

S.CN80 = 1000/80 - 10
Q.CN80 = ((pvec.in-0.2*S.CN80)^2)/(pvec.in + 0.8*S.CN80)*25.4
Q.CN80[pvec.in<0.2*S.CN80] = 0

S.CN90 = 1000/90 - 10
Q.CN90 = ((pvec.in-0.2*S.CN90)^2)/(pvec.in + 0.8*S.CN90)*25.4
Q.CN90[pvec.in<0.2*S.CN90] = 0

#Figure 2.5 - rainfall-runoff relationship
#plot in log-log space
par(mfrow=c(2,1),mar = c(1.5, 1, 1, 1),oma=c(3,3,0,0))
plot(data$total.precip.mm, data$total.q.obs.mm, xlab = "", pch=16, cex = 1.2,las=1,xaxs="i",yaxs="i",xlim=c(0,90),ylim=c(0,50))
lines(pvec.mm,Q.CN85,col="grey")
lines(pvec.mm,Q.CN80,col="grey")
lines(pvec.mm,Q.CN90,col="grey")
text(x=60,y=39,labels="CN=90",cex=0.8)
text(x=60,y=23,labels="80",cex=0.8)
text(x=60,y=30,labels="85",cex=0.8)

mtext(side=2, "Event Total Q (mm)",line=3)
plot(data$total.precip.mm, data$total.q.obs.mm, log="xy",ylab = "Event Total Q (mm)", xlab = "Event Total Precip. (mm)", pch=16, cex = 1.2,las=1,xaxs="i",yaxs="i",xlim=c(5,90),ylim=c(0.1,50),xaxt="n",yaxt="n")
xats = c(5,10,20,50,90)
yats = c(0.1,0.5,1,2,5,10,20,50)
yats.txt = c("0.1","0.5","1","2","5","10","20","50")
axis(side=2,at=yats,labels=yats.txt,las=1)
axis(side=1,at=xats,labels=xats)

mtext(side=2,"Event Total Q (mm)",line=3)
mtext(side=1,"Event Rainfall (mm)",line=3)
lines(pvec.mm,Q.CN85,col="grey")
lines(pvec.mm,Q.CN80,col="grey")
lines(pvec.mm,Q.CN90,col="grey")
text(x=18,y=6,labels="CN=90",cex=0.8)
text(x=18,y=2,labels="85",cex=0.8)
text(x=18,y=0.7,labels="80",cex=0.8)
