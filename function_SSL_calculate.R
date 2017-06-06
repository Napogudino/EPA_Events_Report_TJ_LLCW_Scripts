#  Calculates SSL for three methods

SSL.calc = function(Q,SSC) { # Q = discharge time series for the event.  SSC = SSC samples for the event

Q.dates = as.Date(format(Q$date.time,"%Y-%m-%d"))
ftime = Q$date.time
target.index = which(as.Date(format(SSC$date.time,"%Y-%m-%d")) %in% Q.dates)  
target.times = SSC$date.time[target.index]
SSCsub = SSC[target.index,]

match.data = rep(NA,times=length(target.times))
for (e in 1:length(target.times)){
  difftime = ftime-target.times[e]
  index = which(abs(difftime)==min(abs(difftime))) #absolute min time diff is when sample was taken (0 is at same time)
  match.data[e] = Q$q.cms[index] #discharge at that time
  #q.cms[(index-10):(index+10)]
}

#  Interpolate Q to 5 minute intervals
q.time.5min = seq.POSIXt(from=min(Q$date.time),to=max(Q$date.time),by=5*60)
q.5min = approx(Q$date.time,Q$q.cms,xout=q.time.5min)
total.q.m3 = sum(q.5min$y*5*60)

# 1.VWM for the event
VWM = sum(SSCsub$SSC*match.data)/sum(match.data)
#total.q.m3 = (total.q.obs.mm/1000)*10230000 #convert to m, multiply by 10.23 km2 wtshd area or 10230000 m2
load.ton.event.VWM = VWM*total.q.m3*1000/1E6 #1000L = 1m3, 1E6 g = 1 ton

# 2.EMC for all samples
all.samples = read.csv("summary_table.3.1.csv")
VWM.all = sum(all.samples$SSC..g.L.*all.samples$Q..cms.)/sum(all.samples$Q..cms.)
load.ton.all.VWM = VWM.all*total.q.m3*1000/1E6

# 3. Q-SSC rating curve, all samples
ssl.predict.wbcf.tons = 5*60*sum(SSL.a.w.bcf*q.5min$y^SSL.b)/1000  # Predicted SSL for all Q, kg/s.  Obtained from running "regression_models_SSC_vs_Q.r"
ssl.predict.wobcf.tons = 5*60*sum(SSL.a.wo.bcf*q.5min$y^SSL.b)/1000 

out.df = data.frame(NSSC=length(SSCsub$SSC),Qm3=total.q.m3,Qmm=1000*total.q.m3/(10.23*1E6),VWM=VWM, SSL.VWM.event=load.ton.event.VWM,SSL.VWM.all=load.ton.all.VWM,SSL.rating.wo.bcf=ssl.predict.wobcf.tons,SSL.rating.w.bcf=ssl.predict.wbcf.tons)
return(out.df)
}