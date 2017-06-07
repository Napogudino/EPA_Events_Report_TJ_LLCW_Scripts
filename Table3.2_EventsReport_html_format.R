#To format the html table 3.1 in EPA Events Report
#Script written by Kris Taniguchi, SDSU (kristaniguchi@gmail.com)
#Table with SSC data summary

#Set working directory to the data folder, script directory will be used if sourcing functions
setwd("G:/mydocuments/SDSU/research/tijuana_watershed/writeups/EPA_events_report/EPA_Events_Report_TJ_LLCW_Scripts")
source("print.htmlTable.R")
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

###############################################################################################################
#Read in the tables generated from Table2.1_cal_generate.R and all of the events scripts
table.3.2.ssc = read.csv("summary_table.3.2.csv") #table with both IBWC and PT data

###############################################################################################################
#HTML Tables
library(htmlTable)



#Format html table for PT & IBWC data all
#Create dataframes, make sure all classes of numeric are set for rounding values
table.3.2.ssc.df = data.frame(table.3.2.ssc)
#Round the values 
table.3.2.ssc.df[,c("total.q.mm","VWM")] = round(table.3.2.ssc.df[,c("total.q.mm","VWM")],1) #round the numeric columns
table.3.2.ssc.df[,c("total.q.m3","SSL.Event.VWM","SSL.All.VWM","SSL.Rating.no.bcf","SSL.Rating.bcf")] = round(table.3.2.ssc.df[,c("total.q.m3","SSL.Event.VWM","SSL.All.VWM","SSL.Rating.no.bcf","SSL.Rating.bcf")],0)

#load.round = signif(table.3.2.ssc.df[,4], 3)
names(table.3.2.ssc.df) <- c("", "", "mm", "m3", "g/L", "Event VWM<sup>b</sup>","All VWM<sup>c</sup>","Rating, no bcf<sup>d</sup>","Rating, bcf<sup>e</sup>")

table.3.2.SSC.tableout = htmlTable(table.3.2.ssc.df, 
                                   rnames = rep("", times=length(table.3.2.ssc.df[,1])), #no row names
                                   cgroup = c("Event", "N SSC","Total Q","SSC<sup>a</sup>", "SSL (tons)"), 
                                   n.cgroup = c(1, 1, 2, 1,4),
                                   caption="Table 3.2.  Total event suspended sediment concentration (SSC) and load (SSL) at the PT location for the events with SSC data.",
                                  tfoot =paste("<sup>a</sup> Volume-weighted mean suspended sediment concentration.<BR>",
                                    "<sup>b</sup> = Q x VWM (volume weighted mean SSC concentration) for samples collected during the event.<BR>",
                                    "<sup>c</sup> = Q x VWM (volume weighted mean SSC concentration) for all samples, all events.<BR>",
                                     "<sup>d</sup> Calculated from the Q-SSL rating curve, with no bias correction factor.<BR>",
                                    "<sup>e</sup> Calculated from the Q-SSL rating curve, with bias correction factor.<BR>")
)

print.htmlTable(table.3.2.SSC.tableout)

###############################################################################################################


