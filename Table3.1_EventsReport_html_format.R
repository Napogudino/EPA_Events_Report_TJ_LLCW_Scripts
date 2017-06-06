#To format the html table 3.1 in EPA Events Report
#Script written by Kris Taniguchi, SDSU (kristaniguchi@gmail.com)
#Table with SSC data summary

#Set working directory to the data folder, script directory will be used if sourcing functions
setwd("G:/mydocuments/SDSU/research/tijuana_watershed/writeups/EPA_events_report/EPA_Events_Report_TJ_LLCW_Scripts")
getwd() #the directory where the script is saved
source("print.htmlTable.R")
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

###############################################################################################################
#Read in the tables generated from Table2.1_cal_generate.R and all of the events scripts
table.3.1.ssc = read.csv("summary_table.3.1.csv") #table with both IBWC and PT data

###############################################################################################################
#HTML Tables
library(htmlTable)

#Format html table for PT & IBWC data all
#Create dataframes, make sure all classes of numeric are set for rounding values
table.3.1.ssc.df = data.frame(table.3.1.ssc)
#Round the values 
round1 = txtRound(table.3.1.ssc.df[,2:3],2) #round the numeric columns

table.3.1.final = data.frame(cbind(as.character(table.3.1.ssc.df[,1]), round1[,1], round1[,2]))
names(table.3.1.final) <- c("Date and Time", "SSC (g/L)", "Q (cms)")
#set the text columns as as.character
table.3.1.final[,1] <- as.character(table.3.1.ssc.df[,1])
table.3.1.final[,2] <- as.character(table.3.1.final[,2]) 
table.3.1.final[,3] <- as.character(table.3.1.final[,3]) 
#table.3.1.final[,4] <- as.character(table.3.1.ssc.df[,4]) 

table.3.1.SSC.tableout = htmlTable(table.3.1.final, 
                                       rnames = rep("", times=length(table.3.1.ssc.df[,1])), #no row names
                                       header = c("Date", "SSC (g/L)", "Q (cms)"),
                                       rgroup= c("Storm 1",  "Storm 2",  "Storm 3",  "Storm 6",  "Storm 9",  "Storm 10"), 
                                       n.rgroup=c(2,4,1,6,3,1),
                                       caption="Table 3.1.  Suspended sediment concentration (SSC) for all collected samples.")
print.htmlTable(table.3.1.SSC.tableout)

###############################################################################################################


