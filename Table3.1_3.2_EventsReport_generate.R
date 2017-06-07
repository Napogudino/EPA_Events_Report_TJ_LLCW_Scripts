#To generate Table 3.1 and 3.2 in EPA Events Report
#Script written by Kris Taniguchi, SDSU (kristaniguchi@gmail.com)
#table generated from each SSC script, pull summary data from each script and put into table

#Set working directory to the data folder, script directory will be used if sourcing functions
setwd("G:/mydocuments/SDSU/research/tijuana_watershed/writeups/EPA_events_report/EPA_Events_Report_TJ_LLCW_Scripts")
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder
script.dir= '../EPA_Events_Report_TJ_LLCW_Scripts/' #directory where scripts are saved

###############################################################################################################

#list all of the SSC scripts
flist2 = c("Figure_3.2_EventsReport_SSC_03012014.R",
          "Figure_3.3_EventsReport_SSC_03012015.R",
          "Figure_3.4_EventsReport_SSC_05152015.R",
          "Figure_3.5_EventsReport_SSC_03062016.R",
          "Figure_3.6_EventsReport_SSC_02172017.R",
          "Figure_3.7_EventsReport_SSC_02272017.R")
###############################################################################################################

#Loop to run each R events script and pull out the appropriate tables: obs.summary.PT.IBWC and obs.summary

table.3.1.out = data.frame(matrix(nrow=1, ncol=4)) #empty dataframe to put the values in from obs.summary.PT.IBWC
names(table.3.1.out) <- c("Date", "SSC (g/L)", "Q (cms)", "Event")
table.3.2.out = data.frame(event.date= NA, NSSC=NA, total.q.mm=NA, total.q.m3=NA, VWM=NA, SSL.Event.VWM=NA,SSL.All.VWM=NA,SSL.Rating.no.bcf=NA,SSL.Rating.bcf=NA)
Storm.out = NA

for (i in 1:length(flist2)) {
  source(paste(script.dir,flist2[i], sep = ""))
  #append the obs.summary.PT.IBWC to the summary.out dataframe from each storm
  table.3.1.out = rbind(table.3.1.out, table.3.1.export)
  table.3.2.out = rbind(table.3.2.out, table.3.2.export) #only the usable data (A rating)
  
  Storm.out = c(Storm.out, storm)
}

###############################################################################################################

#New summary tables
table.3.1.out$`Q (cms)`[table.3.1.out$`Q (cms)`<0.07] = 0.07
table.3.1 = table.3.1.out[2:length(table.3.1.out$Date),]
names(table.3.1) <- c("Date", "SSC (g/L)", "Q (cms)", "Event")

table.3.2 = table.3.2.out[2:length(table.3.2.out$event.date),]
Storm.out = Storm.out[2:length(Storm.out)]

###############################################################################################################

#Export new tables into .csv
fname.3.1 = "summary_table.3.1.csv"
write.csv(table.3.1, file=fname.3.1, row.names=F)

fname.3.2 = "summary_table.3.2.csv"
write.csv(table.3.2 , file=fname.3.2, row.names=F)

#see table.3.1_cal_format.R for html formatting of table in report

