#  Loads tau crit and erodibility data
# Compares with fucntions in AGNPS

setwd("G:/mydocuments/SDSU/research/tijuana_watershed/writeups/EPA_events_report/EPA_Events_Report_TJ_LLCW_Scripts")
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

fname = "taucrit_erodibility.csv"

x = read.csv(fname)

ylab.text = expression("Erodibility, "*cm^3 * N^-1*s^-1*"")
xlab.text = expression(tau[c]*", Pa")

par(mar=c(5,5,2,2))
plot(x$taucrit.Pa,x$Erodibility.cm3.N.s,pch=20,cex=1.5,xlab=xlab.text,ylab=ylab.text,log="xy",xlim=c(0.001,5),ylim=c(1,1000))

# AnnAGNPS_Technical_Documentation_2015_03.pdf, Equation 7-5 (Gordon)
tau.pred = seq(0.001,5,by=0.05)  #  1 Pa =  1 N/m2 
erod.pred.AGNPS.eq.7.5 = 29.1E-6 * exp(-0.224*tau.pred) * (100^3/1.67)  # in Mg/n-s, convert to cm3/N-s:  1Mg x 1m3/1.67Mg x 100^3 cm3/1m3    
lines(tau.pred,erod.pred.AGNPS.eq.7.5,lty=2)

# AnnAGNPS_Technical_Documentation_2015_03.pdf, Equation 7-6 (Hanson and Simon)
erod.pred.AGNPS.7.6 = (0.0000002/sqrt(tau.pred))*100^3  #  in m3/s-N, convert to cm3/N-s  100^3
lines(tau.pred,erod.pred.AGNPS.7.6,lty=3)

#  Simon et al 2010, Figure 7 right
erod.pred.simon = 257*(tau.pred^-1.56)  # Already in units of cm3/Ns.
erod.pred.simon.upper = 257*10*(tau.pred^-1.56)
erod.pred.simon.lower= 257*0.10*(tau.pred^-1.56)
lines(tau.pred,erod.pred.simon,lty=1,lwd=2)
lines(tau.pred,erod.pred.simon.upper,lty=1)
lines(tau.pred,erod.pred.simon.lower,lty=1)

legend("topleft",c("Observed","Simon et al 2010","Gordon et al 2007","Hanson and Simon 2001"),pch=c(20,NA,NA,NA),lty=c(NA,1,2,3),pt.cex=1.5,lwd=c(NA,2,1,1))

