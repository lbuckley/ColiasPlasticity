
sun=TRUE #toggle for sun or shade
#fdir= "C:\\Users\\lbuckley\\Google Drive\\ColiasEvolution\\ColiasPlasticity\\"
fdir= "C:\\Users\\Buckley\\Google Drive\\ColiasEvolution\\ColiasPlasticity\\"
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#MAKE PLOTS

setwd(paste(fdir, "OUT\\", sep="") )
if(sun)dat=read.csv("DevTimeTemp_20Nov2015_long_sun.csv")
if(!sun)dat=read.csv("DevTimeTemp_20Nov2015_long_shade.csv")

#get rid of C1 3rd generation
dat[dat$stat==1 & dat$gen==3, 4:9]=NA

setwd(paste(fdir, "Figures\\", sep="") )

labs=c("3.0km","2.4km","1.8km")

#Time 
if(sun)pdf("Fig2_phenology.pdf", height=10,width=10)
#if(!sun)pdf("DevTime_12Oct2015_addCOOPsites_shade.pdf", height=10,width=10)

par(mfrow=c(3,3))
par(cex.lab=2,cex.axis=1.7, lwd=1.5)
par(mar = c(2,4.5,1,0.5), oma=c(2,0,0,0)) #This changes the margin of space around each plot, the default is 4. 1=bottom, 2=left, 3=top, 4=right

#par(mfrow=c(1,2), lwd=2, mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,1), cex=1.2, cex.lab=0.8,cex.main=0.8, oma=c(0,0,0,0), pch="*")

for(site.k in 1:3){
  if(site.k==1) dat.k= dat[dat$stat==1,]
  if(site.k==2) dat.k= dat[dat$stat==51713,]
  if(site.k==3) dat.k= dat[dat$stat==55722,]
  
  dat1gen1= dat.k[dat.k$gen==1 &dat.k$fixed==0,]
  dat1gen2= dat.k[dat.k$gen==2 &dat.k$fixed==0,]
  dat1gen3= dat.k[dat.k$gen==3 &dat.k$fixed==0,]
  
  dat1gen1.f= dat.k[dat.k$gen==1 &dat.k$fixed==1,]
  dat1gen2.f= dat.k[dat.k$gen==2 &dat.k$fixed==1,]
  dat1gen3.f= dat.k[dat.k$gen==3 &dat.k$fixed==1,]
  
  if(site.k==2)plot(dat1gen1$yr, dat1gen1$Jlarv, type="l", ylim= range(80,225), col="orange", ylab="Julian date",xlab="Year")
  ##No y-labels
  if(site.k %in% c(1,3))plot(dat1gen1$yr, dat1gen1$Jlarv, type="l", ylim= range(80,225), col="orange", ylab="",xlab="Year")
  
  #pupal polygons
  x= dat1gen1$yr
  y1= dat1gen1$Jpup
  y2= dat1gen1$Jadult
  polygon(c(x,rev(x)),c(y2,rev(y1)),col="gray", border=NA)
  y1= dat1gen2$Jpup
  y2= dat1gen2$Jadult
  polygon(c(x,rev(x)),c(y2,rev(y1)),col="gray", border=NA)
  y1= dat1gen3$Jpup
  y2= dat1gen3$Jadult
  polygon(c(x,rev(x)),c(y2,rev(y1)),col="gray", border=NA)
  
  #lines
  points(dat1gen1$yr, dat1gen1$Jpup, type="l", col="green")
  points(dat1gen1$yr, dat1gen1$Jadult, type="l", col="purple")
  points(dat1gen2$yr, dat1gen2$Jlarv, type="l", col="orange")
  points(dat1gen2$yr, dat1gen2$Jpup, type="l", col="green")
  points(dat1gen2$yr, dat1gen2$Jadult, type="l", col="purple")
  points(dat1gen3$yr, dat1gen3$Jlarv, type="l", col="orange")
  points(dat1gen3$yr, dat1gen3$Jpup, type="l", col="green")
  points(dat1gen3$yr, dat1gen3$Jadult, type="l", col="purple")  
  
  #plot fixed
  ys= as.numeric(as.vector(c(dat1gen1.f[1,4:6], dat1gen2.f[1,4:6],dat1gen3.f[1,4:6])))
  cols= c("orange","green","purple")
  fcols= rep_len(cols,length.out=length(ys))
  
  for(y.k in 1:length(ys)){
    arrows(x0=1960, y0=ys[y.k], x1=1970, y1=ys[y.k], length=0, col=fcols[y.k], lty="dashed", lwd=2) 
  }

  text(1965,225,labs[site.k], cex=2)
    
  if(site.k==1) legend("bottomleft", legend=c("larvae", "pupae","adults"), lty=c("solid","solid","solid"), col=c("orange","green","purple"), cex=1.5, bty="n")

#----------------------------------------------
#TEMPS
  
  if(site.k %in% c(2)) plot(dat1gen1$yr, dat1gen1$Tpup, type="l", ylim= range(10,35), col="blue", ylab="Tpupal (°C)",xlab="Year")
  #NO LABELS
  if(site.k %in% c(1,3)) plot(dat1gen1$yr, dat1gen1$Tpup, type="l", ylim= range(10,35), col="blue", ylab="",xlab="Year")

  points(dat1gen2$yr, dat1gen2$Tpup, type="l", col="orange")
points(dat1gen3$yr, dat1gen3$Tpup, type="l", col="red")

points(dat1gen1.f$yr, dat1gen1.f$Tpup, type="l", col="blue", lty="dashed")  
points(dat1gen2.f$yr, dat1gen2.f$Tpup, type="l", col="orange", lty="dashed")
points(dat1gen3.f$yr, dat1gen3.f$Tpup, type="l", col="red", lty="dashed")  

if(site.k==1) legend("topleft", legend=c("gen 1", "gen 2", "gen 3"), lty=c("solid","solid","solid"), col=c("blue","orange","red"), cex=1.5, bty="n")

if(site.k==1) legend("topright", legend=c("fixed", "variable"), title="phenology", lty=c("dashed","solid"), col=c("black"), cex=1.5, bty="n")
#--------------------------------------------------------------------------------------
#TEMP DIFF
#plot difference in Tpupal as a function of fixed temperature 
#normalize by Tpupal_fixed each generation

Tdif1.1= dat.k[dat.k$gen==1 &dat.k$fixed==0,"Tpup"]-dat.k[dat.k$gen==1 &dat.k$fixed==1,"Tpup"]
Tdif1.2= dat.k[dat.k$gen==2 &dat.k$fixed==0,"Tpup"]-dat.k[dat.k$gen==2 &dat.k$fixed==1,"Tpup"]
Tdif1.3= dat.k[dat.k$gen==3 &dat.k$fixed==0,"Tpup"]-dat.k[dat.k$gen==3 &dat.k$fixed==1,"Tpup"]

Tpup1.1=  dat.k[dat.k$gen==1 &dat.k$fixed==1,"Tpup"]
Tpup1.2=  dat.k[dat.k$gen==2 &dat.k$fixed==1,"Tpup"]
Tpup1.3=  dat.k[dat.k$gen==3 &dat.k$fixed==1,"Tpup"]

#substract off mean
Tpup1.1= Tpup1.1 -mean(Tpup1.1)
Tpup1.2= Tpup1.2 -mean(Tpup1.2)
Tpup1.3= Tpup1.3 -mean(Tpup1.3)

xlims= range(na.omit(Tpup1.1,Tpup1.2,Tpup1.3), na.omit(Tdif1.1,Tdif1.2,Tdif1.3))
ylims= xlims #range(na.omit(Tdif1.1,Tdif1.2,Tdif1.3))

if(site.k %in% c(2)) plot(Tpup1.1, Tdif1.1, col="blue", xlim=xlims, ylim=ylims, xlab="Tpupal anomaly (fixed phenology, °C)",ylab="Tpupal shift by plasticity (°C)")
# NO X-LABEL
if(site.k %in% c(1,3)) plot(Tpup1.1, Tdif1.1, col="blue", xlim=xlims, ylim=ylims, xlab="Tpupal anomaly (fixed phenology, °C)",ylab="")

points(Tpup1.2, Tdif1.2, col="orange")
points(Tpup1.3, Tdif1.3, col="red")

abline(h=0, col="grey", lwd=1); abline(v=0, col="grey", lwd=1)

#legend("bottomleft", legend=c("gen 1", "gen 2", "gen 3"),pch=c("*","*",'*'), col=c("blue","orange","red"))

} #end loop sites

#add y-axis labels
mtext(c("Year", "Year", "Tpupal anomaly (fixed phenology, °C)"), side=1, line=1, outer=TRUE, cex=1.3,at=c(0.2,0.5,0.8))

dev.off()

####################################################
####################################################
#Stats

mod1=lm(Tdif1.2~ Tpup1.2) #0 +
summary(mod1)

#--------------------------------------
#Trends over time
#1, 51713, 55722

dat1= subset(dat, dat$stat==1 & dat$gen==1 & fixed==1)
#mod1= lm(dat1$Jadult ~ dat1$yr)
mod1= lm(dat1$Tpup ~ dat1$yr)
summary(mod1)

#----------------------------------------
## ADULT TEMP DIFFERENCE

if(site.k==1) dat.k= dat[dat$stat==1,]
if(site.k==2) dat.k= dat[dat$stat==51713,]
if(site.k==3) dat.k= dat[dat$stat==55722,]

dat1gen1= dat.k[dat.k$gen==1 &dat.k$fixed==0,]
dat1gen2= dat.k[dat.k$gen==2 &dat.k$fixed==0,]
dat1gen3= dat.k[dat.k$gen==3 &dat.k$fixed==0,]

dat1gen1.f= dat.k[dat.k$gen==1 &dat.k$fixed==1,]
dat1gen2.f= dat.k[dat.k$gen==2 &dat.k$fixed==1,]
dat1gen3.f= dat.k[dat.k$gen==3 &dat.k$fixed==1,]

Tdif1.1= dat.k[dat.k$gen==1 &dat.k$fixed==0,"Tpup"]-dat.k[dat.k$gen==1 &dat.k$fixed==1,"Tpup"]
Tdif1.2= dat.k[dat.k$gen==2 &dat.k$fixed==0,"Tpup"]-dat.k[dat.k$gen==2 &dat.k$fixed==1,"Tpup"]
Tdif1.3= dat.k[dat.k$gen==3 &dat.k$fixed==0,"Tpup"]-dat.k[dat.k$gen==3 &dat.k$fixed==1,"Tpup"]

Tad1.1=  dat.k[dat.k$gen==1 &dat.k$fixed==1,"Tad"]
Tad1.2=  dat.k[dat.k$gen==2 &dat.k$fixed==1,"Tad"]
Tad1.3=  dat.k[dat.k$gen==3 &dat.k$fixed==1,"Tad"]

#substract off mean
Tad1.1= Tad1.1 -mean(Tad1.1)
Tad1.2= Tad1.2 -mean(Tad1.2)
Tad1.3= Tad1.3 -mean(Tad1.3)

xlims= range(na.omit(Tad1.1,Tad1.2,Tad1.3), na.omit(Tdif1.1,Tdif1.2,Tdif1.3))
ylims= xlims #range(na.omit(Tdif1.1,Tdif1.2,Tdif1.3))
plot(Tad1.1, Tdif1.1, col="blue", xlim=xlims, ylim=ylims, xlab="Tpupal anomaly (fixed phenology, °C)",ylab="Shift in Tad due to plasticity (°C)")
points(Tad1.2, Tdif1.2, col="orange")
points(Tad1.3, Tdif1.3, col="red")

abline(h=0, col="grey", lwd=1); abline(v=0, col="grey", lwd=1)

#Stats
mod1=lm(Tdif1.2~ Tad1.2) #0 +
summary(mod1)

