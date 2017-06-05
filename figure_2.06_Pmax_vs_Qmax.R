#  Load event data for EPA events report
# Plot Pmax 15min, 1hr, 6hr vs Qpk, Qmm

indir = "G:/mydocuments/SDSU/research/tijuana_watershed/los_laureles_canyon/discharge/"
fname = "LLCW_rainfall_Qpk_Qmm_2014-2017.csv"

x = read.csv(paste0(indir,fname),stringsAsFactors = FALSE)
x = x[!is.na(x[,2]),]

# Calculate Qpk for rational method
#  Range of I (mm/hr) is 0-6, for 0-36 mm for 6 hour event
Inew = seq(0,6,by=0.5)
Qpk.CIA.0.2 = 0.278*0.2*Inew*11.0  # 11.0 is drainage are at PT in km2
Qpk.CIA.0.5 = 0.278*0.5*Inew*11.0  # 11.0 is drainage are at PT in km2
Qpk.CIA.0.8 = 0.278*0.8*Inew*11.0  # 11.0 is drainage are at PT in km2


dev.new()

par(mfrow=c(2,1),mar=c(2.5,1,0,1),oma=c(3,4,1,1))
plot(x$Rainfall,x$QpkFinal.cms,pch=20,cex=1.4)
mtext(side=2,"Qpk, m3/s",line=3)
text(x=12,y=18,"A. Event total")
plot(x$P6hr,x$QpkFinal.cms,xlab="Rainfall, mm",pch=20,cex=1.4)

mtext(side=2,"  Qpk, m3/s",line=3)
mtext(side=1,"Rainfall, mm",line=3)
text(x=4.5,y=18,"B. 6-hr maximum")

plot(x$P1hr,x$QpkFinal.cms,pch=20,cex=1.4)
lines(Inew,Qpk.CIA.0.2,lty=2)
lines(Inew,Qpk.CIA.0.5,lty=2)
lines(Inew,Qpk.CIA.0.8,lty=2)

cor(x[,c(2:5,8,11)])

cor.test(x[,"Rainfall"],x[,"QpkFinal.cms"])
cor.test(x[,"P6hr"],x[,"QpkFinal.cms"])
cor.test(x[,"P1hr"],x[,"QpkFinal.cms"])
cor.test(x[,"P15min"],x[,"QpkFinal.cms"])

