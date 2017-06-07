#  Ternary plot of soil texture in soils, sediments in traps

library(vcd)

indir.survey = "G:/mydocuments/SDSU/research/tijuana_watershed/los_laureles_canyon/soil_texture/"
fname.survey = "napoleon_texture_LLCW_mapping_2015_in_report.csv"

indir.AMEC = "G:/mydocuments/SDSU/research/tijuana_watershed/los_laureles_canyon/sed_traps/from_TNERR_2010_2015/texture/"
fname.AMEC = "soil_texture_sed_traps_AMEC.csv"

#indir.trap.survey.2017 = "G:/mydocuments/SDSU/research/tijuana_watershed/los_laureles_canyon/sed_traps/texture_survey_2017/"
#fname.trap.survey.2017 = "texture_LLCW_sediment_traps_2017_03.csv"

x.surv = read.csv(paste0(indir.survey,fname.survey),stringsAsFactors = FALSE)
x.AMEC = read.csv(paste0(indir.AMEC,fname.AMEC),stringsAsFactors = FALSE)
#x.2017 = read.csv(paste0(indir.trap.survey.2017,fname.trap.survey.2017),stringsAsFactors = FALSE,row.names=1)
#x.2017.t = data.frame(t(x.2017))
#x.2017.t.totals = rowSums(x.2017.t)
#x.2017.t$SiltClay = round(x.2017.t$X1/x.2017.t.totals,3)


x.all = rbind(x.surv,x.AMEC)
index.hillslopes = grep("HILL",x.all$Geology)
index.toplot = grep("HILL|MXSB|USTRAP",x.all$Geology) # includes MX SBasin samples
x.all$Total = rowSums(x.all[,3:7])
x.all$Tfines = rowSums(x.all[,5:7]) 
x.all$Sand.norm = x.all$Sand/x.all$Tfines
x.all$Silt.norm = x.all$Silt/x.all$Tfines
x.all$Clay.norm = x.all$Clay/x.all$Tfines
x.all.H = x.all[index.toplot,]
x.all.H.simple = data.frame(Geology=x.all.H$Geology,Sand=x.all.H$Sand.norm,Silt=x.all.H$Silt.norm,Clay=x.all.H$Clay.norm)

# Set geology label for plotting
SC.index = grep("SC",x.all.H.simple$Geology)
CG.index = grep("CG",x.all.H.simple$Geology)
MXSB.index = grep("MXSB",x.all.H.simple$Geology)
USTRAP.index = grep("USTRAP",x.all.H.simple$Geology)
pvec = c(19,15,0,2)
pchvec.all = rep(pvec[1],times=length(x.all.H$Geology))  # set plotting symbol type
pchvec.all[CG.index] = pvec[2]
pchvec.all[MXSB.index] = pvec[3]
pchvec.all[USTRAP.index] = pvec[4]

#ternaryplot(data.frame(x.surv.H$Sand.norm,x.surv.H$Silt.norm,x.surv.H$Clay.norm),pch=pchvec.surv,col="black",main="",dimnames=c("Sand","Silt","Clay"))
ternaryplot(data.frame(x.all.H.simple$Sand,x.all.H.simple$Silt,x.all.H.simple$Clay),pch=pchvec.all,col="black",main="",dimnames=c("Sand","Silt","Clay"))
grid_legend(0.7,0.7,pch=pvec,col="black",labels=c("SC","CG","MXSB","USTRAP, AMEC"))




