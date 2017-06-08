#  Loads tau crit and erodibility data
# Compares with fucntions in AGNPS

setwd("G:/mydocuments/SDSU/research/tijuana_watershed/writeups/EPA_events_report/EPA_Events_Report_TJ_LLCW_Scripts")
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

fname = "taucrit_erodibility.csv"

x = read.csv(fname)

plot(x$taucrit.Pa,x$Erodibility.cm3.N.s,pch=20,cex=1.5,xlab="tau crit, Pa",ylab="Erodibility, cm3/N-s",log="xy",xlim=c(0.001,5),ylim=c(1,900))

# AnnAGNPS_Technical_Documentation_2015_03.pdf, Equation 7-5
tau.pred = seq(0.001,5,by=0.05)  #  1 Pa =  1 N/m2 
erod.pred.AGNPS.eq.7.5 = 29.1E-6 * exp(-0.224*tau.pred) * (100^3/1.67)  # in Mg/n-s, convert to cm3/N-s:  1Mg x 1m3/1.67Mg x 100^3 cm3/1m3    
lines(tau.pred,erod.pred.AGNPS.eq.7.5)

# AnnAGNPS_Technical_Documentation_2015_03.pdf, Equation 7-6
erod.pred.AGNPS.7.6 = (0.0000002/sqrt(tau.pred))*100^3  #  in m3/s-N, convert to cm3/N-s  100^3
lines(tau.pred,erod.pred.AGNPS.7.6,lty=2)

legend("bottomright",c("Observed","AGNPS Eq 7-5","AGNPS Eq 7-6"),pch=c(20,NA,NA),lty=c(NA,1,2),bty="n")
