#  LLCW.  Read SSC, Q data.  Calculate VWM for all, lm 

setwd("G:/mydocuments/SDSU/research/tijuana_watershed/writeups/EPA_events_report/EPA_Events_Report_TJ_LLCW_Scripts")
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

all.samples = read.csv("summary_table.3.1.csv")
VWM.all = sum(all.samples$SSC..g.L.*all.samples$Q..cms.)/sum(all.samples$Q..cms.)
x.df = data.frame(logQ=log(all.samples$Q..cms.),logSSC=log(all.samples$SSC..g.L.))
lm.ssc = lm(logSSC~logQ,data=x.df)

predict.ssc = data.frame(x=exp(x.df$logQ),y=exp(lm.ssc$fitted.values))
predict.ssc.order = predict.ssc[order(predict.ssc$x),]

# Calculate instantaneous load, kg/sec
all.samples$SSL.kg.s = all.samples$SSC..g.L.*all.samples$Q..cms.  # units cancel to give kg/s

x.df = data.frame(logQ=log(all.samples$Q..cms.),logSSC=log(all.samples$SSC..g.L.),logSSL=log(all.samples$SSL.kg.s))
lm.ssl = lm(logSSL~logQ,data=x.df)
predict.ssl = data.frame(x=exp(x.df$logQ),y=exp(lm.ssl$fitted.values))
predict.ssl.order = predict.ssl[order(predict.ssl$x),]
#  Bias correction, after Duan, as described in Crawford 1991, Eq 6
bcf = 1/(length(predict.ssl$x))*sum(exp(lm.ssl$residuals))  # Crawford 1991
bcf2 = exp(mean((lm.ssl$residuals^2)))  # Asselman 2000, eq 4

#  Double panel figure
par(mfrow=c(2,1),mar=c(1,0,0,0),oma=c(4,5,2,2))
plot(all.samples$Q..cms.,all.samples$SSC..g.L.,log="xy",xlab="",xaxt="n",las=1,yaxt="n",pch=20,cex=1.5)
ats.y = c(0.2,0.5,1,5,5,10,20)
axis(side=1,labels=FALSE)
axis(side=2,at=ats.y,labels=as.character(ats.y),las=1)
lines(predict.ssc.order$x,predict.ssc.order$y,lty=2)
mtext(side=2,"SSC, mg/L",line=3)

plot(all.samples$Q..cms.,all.samples$SSL.kg.s,log="xy",las=1,yaxt="n",pch=20,cex=1.5)
ats = c(0.01,0.1,1,10,100)
axis(side=2,at=ats,labels=as.character(ats),las=1)
lines(predict.ssl.order$x,bcf*predict.ssl.order$y,lty=3)
lines(predict.ssl.order$x,predict.ssl.order$y,lty=2)
mtext(side=1,"Q, m3/s",line=3)
mtext(side=2,"SSL, kg/s",line=3)

SSL.a.w.bcf = exp(lm.ssl$coefficients[1])*bcf
SSL.a.wo.bcf = exp(lm.ssl$coefficients[1])
SSL.b = lm.ssl$coefficients[2]
