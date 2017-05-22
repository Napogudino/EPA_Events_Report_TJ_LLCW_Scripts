#Figure 2.5
#Total event discharge vs. total precip.
#Using data from EPA Events Report Table 2.3
#Script written by Kris Taniguchi, SDSU (kristaniguchi@gmail.com)

#Set working directory to the data folder, script directory will be used if sourcing functions
# getwd() #the directory where the script is saved
# setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

###############################################################################################################

#Read in table 2.3 summary table of observed events
# data = read.csv("summary_table_obs_allevents.csv")
data = read.csv("https://raw.githubusercontent.com/kristaniguchi/EPA_Events_Report_TJ_LLCW_Data/master/summary_table_obs_allevents.csv")

###############################################################################################################

#Figure 2.5 - rainfall-runoff relationship
#plot in log-log space
par(mar = c(4, 4.1, 0, 0.1))
plot(data$total.precip.mm, data$total.q.obs.mm, log = "xy", ylab = "Event Total Q (mm)", xlab = "Event Total Precip. (mm)", pch=16, cex = 1.2)

pvec.mm = seq(0,80,by=1)
pvec.in = pvec.mm/25.4

#  Add SCS CNs
S.CN60 = 1000/60 - 10
Q.CN60 = ((pvec.in-0.2*S.CN60)^2)/(pvec.in + 0.8*S.CN60)
Q.CN60[Q.CN60<=0] = NA

