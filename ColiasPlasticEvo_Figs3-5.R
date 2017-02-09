fdir= "C:\\Users\\Buckley\\Google Drive\\ColiasEvolution\\ColiasPlasticity\\"

#############################################################################################

##part C:  Figures
#Plots and analyses of evo output, evolving RNs
#Variable phenology only, C1, 51713, and Montrose sites
## C1 site
setwd(paste(fdir, "\\OUT\\", sep=""))
Evo.C1 <- read.csv(file="Evo.output.Site1.Varying.2gens.EvolvingRN.csv", header =T)

#find ranges
range(Evo.C1$betaRN)
range(Evo.C1$betaAbsmid)
range(Evo.C1$Absmid)
range(Evo.C1$meanAbs)
range(Evo.C1$year)
range(Evo.C1$meanLambda)


# plots of beta, alphaopt and fitness for each generation, as functions of year
C1.g1 <- subset(Evo.C1,generation==1)
C1.g2 <- subset(Evo.C1,generation==2)
#C1.g3 <- subset(Evo.C1,generation==3)

#51713 site = CC site

Evo.CC <- read.csv(file="Evo.output.Site51713.Varying.3gens.EvolvingRN.csv", header =T)

#find ranges
range(Evo.CC$betaRN)
range(Evo.CC$betaAbsmid)
range(Evo.CC$meanAbs)
range(Evo.CC$year)
range(Evo.CC$meanLambda)

# plots of beta, alphaopt and fitness for each generation, as functions of year
CC.g1 <- subset(Evo.CC,generation==1)
CC.g2 <- subset(Evo.CC,generation==2)
CC.g3 <- subset(Evo.CC,generation==3)

##Montrose site
Evo.Mn <- read.csv(file="Evo.output.Site55722.Varying.3gens.EvolvingRN.csv", header =T)
names(Evo.Mn)
head(Evo.Mn)

#find ranges
range(Evo.Mn$betaRN)
range(Evo.Mn$betaAbsmid)
range(Evo.Mn$meanAbs)
range(Evo.Mn$year)
range(Evo.Mn$meanLambda)

# plots of beta, alphaopt and fitness for each generation, as functions of year
Mn.g1 <- subset(Evo.Mn,generation==1)
Mn.g2 <- subset(Evo.Mn,generation==2)
Mn.g3 <- subset(Evo.Mn,generation==3)

##The plots
par(mfrow=c(3,3))
par(cex.lab=1.4,cex.axis=1.2)
par(mar = c(4.5,4.5,1,1)) #This changes the margin of space around each plot, the default is 4. 1=bottom, 2=left, 3=top, 4=right
with(C1.g1,plot(year,betaRN,type = "l", lwd= 2, col="blue", ylab="Directional Selection", xlab="Year", xlim=c(1960,2010), ylim=c(-18.0,4.0))) #cx.lab changes the size of the axis labels
abline(h=0,lty=2)#Horizontal line at 0 
with(C1.g2,lines(year,betaRN,lty=1, lwd= 2, col="orange")) 


with(C1.g1,plot(year,meanAbs,type = "l", lwd= 2, col="blue",  xlab="Year",ylab = "Absorptivity",xlim=c(1960,2010), ylim=c(0.40,0.70)))
with(C1.g2, lines(year,meanAbs, lty=1, lwd=2,col="orange"))

with(C1.g1,plot(year,meanLambda,type = "l", lwd= 2, col="blue",  xlab="Year",ylab = "Mean Fitness",xlim=c(1960,2010), ylim=c(1,2.5)))
with(C1.g2,lines(year,meanLambda,lty=1, lwd= 2, col="orange")) 

#CC site
with(CC.g1,plot(year,betaRN,type = "l", lwd= 2, col="blue", ylab="Directional Selection", xlab="Year", xlim=c(1960,2010), ylim=c(-18.0,4.0))) #cx.lab changes the size of the axis labels
abline(h=0,lty=2)#Horizontal line at 0 
with(CC.g2,lines(year,betaRN,lty=1, lwd= 2, col="orange")) 
with(CC.g3,lines(year,betaRN,lty=1, lwd= 2, col="red")) 

with(CC.g1,plot(year,meanAbs,type = "l", lwd= 2, col="blue",  xlab="Year",ylab = "Absorptivity",xlim=c(1960,2010), ylim=c(0.40,0.70)))
with(CC.g2, lines(year,meanAbs, lty=1, lwd=2,col="orange"))
with(CC.g3, lines(year,meanAbs, lty=1, lwd=2,col="red"))


with(CC.g1,plot(year,meanLambda,type = "l", lwd= 2, col="blue",  xlab="Year",ylab = "Mean Fitness",xlim=c(1960,2010), ylim=c(1,2.5)))
with(CC.g2,lines(year,meanLambda,lty=1, lwd= 2, col="orange")) 
with(CC.g3,lines(year,meanLambda,lty=1, lwd= 2, col="red")) 

##Montrose site
with(Mn.g1,plot(year,betaRN,type = "l", lwd= 2, col="blue", ylab="Directional Selection", xlab="Year", xlim=c(1960,2010), ylim=c(-18.0,4.0))) #cx.lab changes the size of the axis labels
abline(h=0,lty=2)#Horizontal line at 0 
with(Mn.g2,lines(year,betaRN,lty=1, lwd= 2, col="orange")) 
with(Mn.g3,lines(year,betaRN,lty=1, lwd= 2, col="red")) 

with(Mn.g1,plot(year,meanAbs,type = "l", lwd= 2, col="blue",  xlab="Year",ylab = "Absorptivity",xlim=c(1960,2010), ylim=c(0.40,0.70)))
with(Mn.g2, lines(year,meanAbs, lty=1, lwd=2,col="orange"))
with(Mn.g3, lines(year,meanAbs, lty=1, lwd=2,col="red"))

with(Mn.g1,plot(year,meanLambda,type = "l", lwd= 2, col="blue",  xlab="Year",ylab = "Mean Fitness",xlim=c(1960,2010), ylim=c(1,2.5)))
with(Mn.g2,lines(year,meanLambda,lty=1, lwd= 2, col="orange")) 
with(Mn.g3,lines(year,meanLambda,lty=1, lwd= 2, col="red")) 

#########################################################################################################################
### FIGURE 3
#Plots and analyses of evo output, plasticity but no evolution
#Variable phenology only, C1, 51713, and Montrose sites
## C1 site

Evo.C1 <- read.csv(file="Evo.output.Site1.Fixed.2gens.yesPlasticnoEvo.csv", header =T)

#find ranges
range(Evo.C1$beta)
range(Evo.C1$meanLambda)


# plots of beta, alphaopt and fitness for each generation, as functions of year
C1.g1 <- subset(Evo.C1,generation==1)
C1.g2 <- subset(Evo.C1,generation==2)
#C1.g3 <- subset(Evo.C1,generation==3)

#51713 site = CC site

Evo.CC <- read.csv(file="Evo.output.Site51713.Varying.3gens.yesPlasticnoEvo.csv", header =T)

#find ranges
range(Evo.CC$beta)
range(Evo.CC$meanLambda)

# plots of beta, alphaopt and fitness for each generation, as functions of year
CC.g1 <- subset(Evo.CC,generation==1)
CC.g2 <- subset(Evo.CC,generation==2)
CC.g3 <- subset(Evo.CC,generation==3)

##Montrose site
Evo.Mn <- read.csv(file="Evo.output.Site55722.Varying.3gens.yesPlasticnoEvo.csv", header =T)
names(Evo.Mn)
head(Evo.Mn)

#find ranges
range(Evo.Mn$beta)
range(Evo.Mn$meanLambda)

# plots of beta, alphaopt and fitness for each generation, as functions of year
Mn.g1 <- subset(Evo.Mn,generation==1)
Mn.g2 <- subset(Evo.Mn,generation==2)
Mn.g3 <- subset(Evo.Mn,generation==3)

#=============================================
##The plots
setwd(paste(fdir, "Figures\\", sep="") )

pdf("Fig3_SelectionEvolution.pdf", height=10,width=10)

par(mfrow=c(3,3))
par(cex.lab=2,cex.axis=1.7, lwd=1.5)
par(mar = c(1.4,4.5,1,0.5)) #This changes the margin of space around each plot, the default is 4. 1=bottom, 2=left, 3=top, 4=right
par(oma=c(2,0,0,0), bty="o")
#par(mfrow=c(1,2), lwd=2, mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,1), cex=1.2, cex.lab=0.8,cex.main=0.8, oma=c(0,0,0,0))

with(C1.g1,plot(year,beta,type = "l", lwd= 2, col="blue", ylab="", xlab="Year", xlim=c(1960,2010), ylim=c(-1.8,3.5))) #cx.lab changes the size of the axis labels
abline(h=0,lty=2)#Horizontal line at 0 
with(C1.g2,lines(year,beta,lty=1, lwd= 2, col="orange")) 
text(1980,3.3,"3.0km", cex=2)

legend("topright", legend=c("gen 1", "gen 2", "gen 3"), lty=c("solid","solid","solid"), col=c("blue","orange","red"), cex=1.5, bty="n")

with(C1.g1,plot(year,meanAbs,type = "l", lwd= 2, col="blue",  xlab="Year",ylab = "",xlim=c(1960,2010), ylim=c(0.4,0.7), yaxt="n"))
with(C1.g2, lines(year,meanAbs, lty=1, lwd=2,col="orange"))
axis(2, seq(0.4,0.7,0.1), seq(0.4,0.7,0.1))

with(C1.g1,plot(year,meanLambda,type = "l", lwd= 2, col="blue",  xlab="Year",ylab = "",xlim=c(1960,2010), ylim=c(1,2.4)))
with(C1.g2,lines(year,meanLambda,lty=1, lwd= 2, col="orange")) 

#CC site
with(CC.g1,plot(year,beta,type = "l", lwd= 2, col="blue", ylab="Directional Selection", xlab="Year", xlim=c(1960,2010), ylim=c(-1.8,3.5))) #cx.lab changes the size of the axis labels
abline(h=0,lty=2)#Horizontal line at 0 
with(CC.g2,lines(year,beta,lty=1, lwd= 2, col="orange")) 
with(CC.g3,lines(year,beta,lty=1, lwd= 2, col="red")) 
text(1980,3.3,"2.4km", cex=2)

with(CC.g1,plot(year,meanAbs,type = "l", lwd= 2, col="blue",  xlab="Year",ylab = "Absorptivity",xlim=c(1960,2010), ylim=c(0.4,0.7), yaxt="n"))
with(CC.g2, lines(year,meanAbs, lty=1, lwd=2,col="orange"))
with(CC.g3, lines(year,meanAbs, lty=1, lwd=2,col="red"))
axis(2, seq(0.4,0.7,0.1), seq(0.4,0.7,0.1))

with(CC.g1,plot(year,meanLambda,type = "l", lwd= 2, col="blue",  xlab="Year",ylab = "Mean Fitness",xlim=c(1960,2010), ylim=c(1,2.4)))
with(CC.g2,lines(year,meanLambda,lty=1, lwd= 2, col="orange")) 
with(CC.g3,lines(year,meanLambda,lty=1, lwd= 2, col="red")) 

##Montrose site
with(Mn.g1,plot(year,beta,type = "l", lwd= 2, col="blue", ylab="", xlab="Year", xlim=c(1960,2010), ylim=c(-1.8,3.5))) #cx.lab changes the size of the axis labels
abline(h=0,lty=2)#Horizontal line at 0 
with(Mn.g2,lines(year,beta,lty=1, lwd= 2, col="orange")) 
with(Mn.g3,lines(year,beta,lty=1, lwd= 2, col="red")) 
text(1980,3.3,"1.8km", cex=2)

with(Mn.g1,plot(year,meanAbs,type = "l", lwd= 2, col="blue",  xlab="Year",ylab = "",xlim=c(1960,2010), ylim=c(0.4,0.7), yaxt="n"))
with(Mn.g2, lines(year,meanAbs, lty=1, lwd=2,col="orange"))
with(Mn.g3, lines(year,meanAbs, lty=1, lwd=2,col="red"))
axis(2, seq(0.4,0.7,0.1), seq(0.4,0.7,0.1))

with(Mn.g1,plot(year,meanLambda,type = "l", lwd= 2, col="blue",  xlab="Year",ylab = "",xlim=c(1960,2010), ylim=c(1,2.4)))
with(Mn.g2,lines(year,meanLambda,lty=1, lwd= 2, col="orange")) 
with(Mn.g3,lines(year,meanLambda,lty=1, lwd= 2, col="red")) 

#add y-axis labels
mtext("Year", side=1, line=1, outer=TRUE, cex=1.5)

dev.off()
#####################################################################################################################################
### FIGURE 4
#plots of evolving RNs
#all sites stacked 
library(ggplot2)
library(grid)

setwd(paste(fdir, "\\OUT\\", sep=""))
#C1 site
C1.EvoRN<- read.csv(file="Evo.output.Site1.Varying.2gens.EvolvingRN.csv", header =T)
head(C1.EvoRN)
names(C1.EvoRN)
C1 <- C1.EvoRN[order(C1.EvoRN$year,C1.EvoRN$Tpup),]
head(C1)

##Perfect plasticity results
C1.PP<- read.csv(file="C1.vary.PerfectPlastic.csv", header =T)
C1P <- C1.PP[order(C1.PP$year,C1.PP$Tpup),]
head(C1P)

#CC = 51713 site
CC.EvoRN<- read.csv(file="Evo.output.Site51713.Varying.3gens.EvolvingRN.csv", header =T)
head(CC.EvoRN)
names(CC.EvoRN)
CC <- CC.EvoRN[order(CC.EvoRN$year,CC.EvoRN$Tpup),]
head(CC)

CC.PP<- read.csv(file="Site51713.Vary.PerfectPlastic.csv", header =T)
CCP <- CC.PP[order(CC.PP$year,CC.PP$Tpup),]
head(CCP)

#site Montrose
Mn.EvoRN <- read.csv(file="Evo.output.Site55722.Varying.3gens.EvolvingRN.csv", header =T)
head(Mn.EvoRN)
names(Mn.EvoRN)
Mn <- Mn.EvoRN[order(Mn.EvoRN$year,Mn.EvoRN$Tpup),]
head(Mn) 

Mn.PP<- read.csv(file="Montrose.vary.PerfectPlastic.csv", header =T)
MnP <- Mn.PP[order(Mn.PP$year,Mn.PP$Tpup),]
head(MnP)

library(ggplot2)
library(grid)
#evolving Rns
m<- ggplot(C1, aes(x=Tpup, y=meanAbs, color=year))
G1n<- m+geom_line(aes(group=year),size=1)+theme_bw()+xlim(10,30)+ylim(0.4,0.7)+scale_color_gradientn(colours=rainbow(10))+ theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+theme(axis.line = element_line(color = 'black'),axis.text.x=element_text(size=17),axis.text.y=element_text(size=17),axis.title.x=element_text(size=24, vjust=0.1),axis.title.y=element_text(size=24, angle=90,vjust=0.25))+xlab(NULL)+ylab(NULL)+
  theme(plot.margin = unit(c(2,2,2,2), "mm"))+
  theme(legend.position="bottom")  +
  theme(legend.key.width = unit(2, "cm"),legend.title=element_text(size=17), legend.text=element_text(size=15))

G1 <- G1n + theme(legend.position = "none")

k<- ggplot(CC, aes(x=Tpup, y=meanAbs, color=year))
G2<- k+geom_line(aes(group=year),size=1)+theme_bw()+xlim(10,30)+ylim(0.4,0.7)+scale_color_gradientn(colours=rainbow(10))+ theme(legend.position = "none", plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+theme(axis.line = element_line(color = 'black'),axis.text.x=element_text(size=17),axis.text.y=element_text(size=17),axis.title.x=element_text(size=24, vjust=0.1),axis.title.y=element_text(size=24, angle=90,vjust=0.25),legend.title=element_text(size=17), legend.text=element_text(size=15))+xlab(NULL)+ylab(NULL)+
  theme(plot.margin = unit(c(2,2,2,2), "mm"))

h<- ggplot(Mn, aes(x=Tpup, y=meanAbs, color=year))
G3<- h+geom_line(aes(group=year),size=1)+theme_bw()+xlim(10,30)+ylim(0.4,0.7)+scale_color_gradientn(colours=rainbow(10))+ theme(legend.position = "none", plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+theme(axis.line = element_line(color = 'black'),axis.text.x=element_text(size=17),axis.text.y=element_text(size=17),axis.title.x=element_text(size=24, vjust=0.1),axis.title.y=element_text(size=24, angle=90,vjust=0.25),legend.title=element_text(size=17), legend.text=element_text(size=15))+xlab(NULL)+ylab(NULL)+
  theme(plot.margin = unit(c(2,2,2,2), "mm"))

#Perfect RNs
m<- ggplot(C1P, aes(x=Tpup, y=absopt, color=year))
H1<- m+geom_line(aes(group=year),size=1)+theme_bw()+xlim(10,30)+ylim(0.4,0.7)+scale_color_gradientn(colours=rainbow(10))+ theme(legend.position = "none", plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+theme(axis.line = element_line(color = 'black'),axis.text.x=element_text(size=17),axis.text.y=element_text(size=17),axis.title.x=element_text(size=24, vjust=0.1),axis.title.y=element_text(size=24, angle=90,vjust=0.25),legend.title=element_text(size=17), legend.text=element_text(size=15))+xlab(NULL)+ylab(NULL)+annotate("text", x=26.0,y=0.68, label= "3.0km", size=5)+
  theme(plot.margin = unit(c(2,2,2,2), "mm"))

k<- ggplot(CCP, aes(x=Tpup, y=absopt, color=year))
H2<- k+geom_line(aes(group=year),size=1)+theme_bw()+xlim(10,30)+ylim(0.4,0.7)+scale_color_gradientn(colours=rainbow(10))+ theme(legend.position = "none", plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+theme(axis.line = element_line(color = 'black'),axis.text.x=element_text(size=17),axis.text.y=element_text(size=17),axis.title.x=element_text(size=24, vjust=0.1),axis.title.y=element_text(size=24, angle=90,vjust=0.25),legend.title=element_text(size=17), legend.text=element_text(size=15))+xlab(NULL)+ylab(NULL)+annotate("text", x=26.0,y=0.68, label= "2.4km", size=5)+
  theme(plot.margin = unit(c(2,2,2,2), "mm"))

h<- ggplot(MnP, aes(x=Tpup, y=absopt, color=year))
H3<- h+geom_line(aes(group=year),size=1)+theme_bw()+xlim(10,30)+ylim(0.4,0.7)+scale_color_gradientn(colours=rainbow(10))+ theme(legend.position = "none", plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+theme(axis.line = element_line(color = 'black'),axis.text.x=element_text(size=17),axis.text.y=element_text(size=17),axis.title.x=element_text(size=24, vjust=0.1),axis.title.y=element_text(size=24, angle=90,vjust=0.25),legend.title=element_text(size=17), legend.text=element_text(size=15))+xlab(NULL)+ylab(NULL)+annotate("text", x=26.0,y=0.68, label= "1.8km", size=5)+
  theme(plot.margin = unit(c(2,2,2,2), "mm"))

#+xlab("Pupal Temperature")+ylab("Absorptivity")

#ADD INITAL RN
#Htest<- h+geom_line(aes(group=year),size=1)+geom_segment(aes(x = as.numeric(15), y = as.numeric(0.7), xend = as.numeric(25), yend = as.numeric(0.4), colour = "black", show.legend=FALSE))

#--------------------------
#Extract legend

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend<-g_legend(G1n)

#----------------------------
setwd(paste(fdir, "Figures\\", sep="") )

pdf("Fig4_ReactionNorm.pdf", height=10,width=10)

lheight <- sum(legend$height)
p <- arrangeGrob(H1, G1, H2, G2, H3, G3, ncol=2, left=textGrob("Absorptivity", rot = 90, gp=gpar(fontsize=20)))
theight <- unit(20, "points")
p <- arrangeGrob(p, textGrob("Pupal Temperature (°C)", gp=gpar(fontsize=20)), heights=unit.c(unit(1, "npc") - theight, theight))
p <- arrangeGrob(p, legend, heights=unit.c(unit(1, "npc") - lheight, lheight), ncol=1)
print(p)

dev.off()

#####################################################################################################################################
### FIGURE 5

#Part 4: Plots and analyses of evo output for different scenarios
#Code of df names:
#site C (C1) or M (Montrose)
#Coop mid sites: site 3 (51713) or 0 (53500)
#Seasonal Development D: v=variable, f=fixed
#Plasticity P:  n= no, y=yes (fixed), o=optimal, e = evolving plasticity
#Evolution of trait E: n=no, y=yes (0.4), m  = maximum (h2=1)

##Combined into single plot
# plots of geometric mean fitness as a function of year, for each scenario
# two panels: variable and fixed
## read files
setwd(paste(fdir, "\\OUT\\", sep=""))
C1.GmeanFit <- read.csv(file="C1.Fitness.scenarios.csv", header=T)
Gu.GmeanFit <- read.csv(file="Site51713.Fitness.scenarios.csv", header=T)
Mn.GmeanFit <- read.csv(file="Montrose.Fitness.scenarios.csv", header=T)

setwd(paste(fdir, "Figures\\", sep="") )

pdf("Fig5_GeomFitness.pdf", height=10,width=10)

par(mfrow=c(3,2))
par(cex.lab=1.5,cex.axis=1.6, lwd=1.5)
par(mar = c(1.4,2.3,1,0.5)) #This changes the margin of space around each plot, the default is 4. 1=bottom, 2=left, 3=top, 4=right
par(oma=c(2.3,2,2,0), bty="o")
#par(mfrow=c(1,2), lwd=2, mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,1), cex=1.2, cex.lab=0.8,cex.main=0.8, oma=c(0,0,0,0))

with(C1.GmeanFit,plot(Year,CvDpPnE,type = "l", lwd= 2, col="black", ylab="Geom Mean Fitness", xlab="Year", xlim=c(1960,2010), ylim=c(1.4,2.3))) #cx.lab changes the size of the axis labels
with(C1.GmeanFit,lines(Year,CvDyPnE,lty=1, lwd= 2, col="green")) 
with(C1.GmeanFit,lines(Year,CvDnPyE,lty=1, lwd= 2, col="orange")) 
with(C1.GmeanFit,lines(Year,CvDyPyE,lty=1, lwd= 2, col="purple")) 
with(C1.GmeanFit,lines(Year,CvDnPnE,lty=1, lwd= 2, col="red")) 
with(C1.GmeanFit,lines(Year,CvDePyE,lty=1, lwd= 2, col="blue")) 
text(1965,1.5,"3.0km", cex=2)

legend(1975,1.8,cex=1.5,bty = "n",c("Perfect plasticity","Observed plasticity","Evolution of absorptivity","Evolution with observed plasticity","Constant absorptivity","Evolution of plasticity"),lty=c(1,1,1,1,1,1),col=c("black","green","orange","purple","red","blue") )

with(C1.GmeanFit,plot(Year,CfDpPnE,type = "l", lwd= 2, col="black", ylab="Geom Mean Fitness", xlab="Year", xlim=c(1960,2010), ylim=c(1.4,2.3))) #cx.lab changes the size of the axis labels
with(C1.GmeanFit,lines(Year,CfDyPnE,lty=1, lwd= 2, col="green")) 
with(C1.GmeanFit,lines(Year,CfDnPyE,lty=1, lwd= 2, col="orange")) 
with(C1.GmeanFit,lines(Year,CfDyPyE,lty=1, lwd= 2, col="purple")) 
with(C1.GmeanFit,lines(Year,CfDnPnE,lty=1, lwd= 2, col="red")) 
with(C1.GmeanFit,lines(Year,CfDePyE,lty=1, lwd= 2, col="blue")) 


with(Gu.GmeanFit,plot(Year,GvDpPnE,type = "l", lwd= 2, col="black", ylab="Geom Mean Fitness", xlab="Year", xlim=c(1960,2010), ylim=c(1.4,2.3))) #cx.lab changes the size of the axis labels
with(Gu.GmeanFit,lines(Year,GvDyPnE,lty=1, lwd= 2, col="green")) 
with(Gu.GmeanFit,lines(Year,GvDnPyE,lty=1, lwd= 2, col="orange")) 
with(Gu.GmeanFit,lines(Year,GvDyPyE,lty=1, lwd= 2, col="purple")) 
with(Gu.GmeanFit,lines(Year,GvDnPnE,lty=1, lwd= 2, col="red")) 
with(Gu.GmeanFit,lines(Year,GvDePyE,lty=1, lwd= 2, col="blue")) 
text(1965,1.5,"2.4km", cex=2)

with(Gu.GmeanFit,plot(Year,GfDpPnE,type = "l", lwd= 2, col="black", ylab="Geom Mean Fitness", xlab="Year", xlim=c(1960,2010), ylim=c(1.4,2.3))) #cx.lab changes the size of the axis labels
with(Gu.GmeanFit,lines(Year,GfDyPnE,lty=1, lwd= 2, col="green")) 
with(Gu.GmeanFit,lines(Year,GfDnPyE,lty=1, lwd= 2, col="orange")) 
with(Gu.GmeanFit,lines(Year,GfDyPyE,lty=1, lwd= 2, col="purple")) 
with(Gu.GmeanFit,lines(Year,GfDnPnE,lty=1, lwd= 2, col="red")) 
with(Gu.GmeanFit,lines(Year,GfDePyE,lty=1, lwd= 2, col="blue")) 


with(Mn.GmeanFit,plot(Year,MvDpPnE,type = "l", lwd= 2, col="black", ylab="Geom Mean Fitness", xlab="Year", xlim=c(1960,2010), ylim=c(1.1,2.3))) #cx.lab changes the size of the axis labels
with(Mn.GmeanFit,lines(Year,MvDyPnE,lty=1, lwd= 2, col="green")) 
with(Mn.GmeanFit,lines(Year,MvDnPyE,lty=1, lwd= 2, col="orange")) 
with(Mn.GmeanFit,lines(Year,MvDyPyE,lty=1, lwd= 2, col="purple")) 
with(Mn.GmeanFit,lines(Year,MvDnPnE,lty=1, lwd= 2, col="red")) 
with(Mn.GmeanFit,lines(Year,MvDePyE,lty=1, lwd= 2, col="blue")) 
text(1965,1.2,"1.8km", cex=2)

with(Mn.GmeanFit,plot(Year,MfDpPnE,type = "l", lwd= 2, col="black", ylab="Geom Mean Fitness", xlab="Year", xlim=c(1960,2010), ylim=c(1.1,2.3))) #cx.lab changes the size of the axis labels
with(Mn.GmeanFit,lines(Year,MfDyPnE,lty=1, lwd= 2, col="green")) 
with(Mn.GmeanFit,lines(Year,MfDnPyE,lty=1, lwd= 2, col="orange")) 
with(Mn.GmeanFit,lines(Year,MfDyPyE,lty=1, lwd= 2, col="purple")) 
with(Mn.GmeanFit,lines(Year,MfDnPnE,lty=1, lwd= 2, col="red")) 
with(Mn.GmeanFit,lines(Year,MfDePyE,lty=1, lwd= 2, col="blue")) 

#add axis labels
mtext("Year", side=1, line=1.2, outer=TRUE, cex=1.5)
mtext("Geom Mean Fitness", side=2, line=0.2, outer=TRUE, cex=1.5)
mtext(c("Varying Phenology","Fixed Phenology"),at=c(0.25,0.75), side=3, line=0, outer=TRUE, cex=1.5)

dev.off()
