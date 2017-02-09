
library(ggplot2)
library(grid)
library(gridExtra)

sun=TRUE #toggle for sun or shade
#fdir= "C:\\Users\\lbuckley\\Google Drive\\ColiasEvolution\\ColiasPlasticity\\"
#fdir= "C:\\Users\\Buckley\\Google Drive\\ColiasEvolution\\ColiasPlasticity\\"
fdir= "C:\\Users\\Buckley\\Desktop\\NCC\\"

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#LOAD DATA
cols=c("blue","orange","red")

setwd(paste(fdir, "OUT\\", sep="") )
if(sun)dat=read.csv("DevTimeTemp_20Nov2015_long_sun.csv")
if(!sun)dat=read.csv("DevTimeTemp_20Nov2015_long_shade.csv")

#get rid of C1 3rd generation
dat[dat$stat==1 & dat$gen==3, 4:9]=NA

#------------------------------
#Plots and analyses of evo output, evolving RNs
#Fixed phenology only, C1, 51713, and Montrose sites
## C1 site
setwd(paste(fdir, "\\OUT\\NCCoutput\\", sep=""))
Evo.C1 <- read.csv(file="Evo.output.Site1.Fixed.2gens.EvolvingRN.csv", header =T)

# plots of beta, alphaopt and fitness for each generation, as functions of year
C1.g1 <- subset(Evo.C1,generation==1)
C1.g2 <- subset(Evo.C1,generation==2)
#C1.g3 <- subset(Evo.C1,generation==3)
#51713 site = CC site

Evo.CC <- read.csv(file="Evo.output.Site51713.Fixed.3gens.EvolvingRN.csv", header =T)

# plots of beta, alphaopt and fitness for each generation, as functions of year
CC.g1 <- subset(Evo.CC,generation==1)
CC.g2 <- subset(Evo.CC,generation==2)
CC.g3 <- subset(Evo.CC,generation==3)

##Montrose site
Evo.Mn <- read.csv(file="Evo.output.Site55722.Fixed.3gens.EvolvingRN.csv", header =T)
names(Evo.Mn)
head(Evo.Mn)

# plots of beta, alphaopt and fitness for each generation, as functions of year
Mn.g1 <- subset(Evo.Mn,generation==1)
Mn.g2 <- subset(Evo.Mn,generation==2)
Mn.g3 <- subset(Evo.Mn,generation==3)

#-----------
##NO EVOLVING RN

## C1 site
Evo.C1 <- read.csv(file="Evo.output.Site1.Fixed.2gens.yesPlasticyesEvo.csv", header =T)
Evo.C1$betaAbsmid= Evo.C1$beta
# plots of beta, alphaopt and fitness for each generation, as functions of year
C1.g1.nornevo <- subset(Evo.C1,generation==1)
C1.g2.nornevo <- subset(Evo.C1,generation==2)
#C1.g3 <- subset(Evo.C1,generation==3)
#51713 site = CC site

Evo.CC <- read.csv(file="Evo.output.Site51713.Fixed.3gens.yesPlasticyesEvo.csv", header =T)
Evo.CC$betaAbsmid= Evo.CC$beta
# plots of beta, alphaopt and fitness for each generation, as functions of year
CC.g1.nornevo <- subset(Evo.CC,generation==1)
CC.g2.nornevo <- subset(Evo.CC,generation==2)
CC.g3.nornevo <- subset(Evo.CC,generation==3)

##Montrose site
#! FIX SITE #Evo.Mn <- read.csv(file="Evo.output.Site55722.Fixed.3gens.yesPlasticyesEvo.csv", header =T)
Evo.Mn <- read.csv(file="Evo.output.Site53500.Fixed.3gens.yesPlasticyesEvo.csv", header =T)
Evo.Mn$betaAbsmid= Evo.Mn$beta

# plots of beta, alphaopt and fitness for each generation, as functions of year
Mn.g1.nornevo <- subset(Evo.Mn,generation==1)
Mn.g2.nornevo <- subset(Evo.Mn,generation==2)
Mn.g3.nornevo <- subset(Evo.Mn,generation==3)

#==========================================================
#MAKE PLOTS
setwd(paste(fdir, "Figures\\", sep="") )

labs=c("3.0km","2.4km","1.8km")

#--------------------------
#FIG 1

#Time 
pdf("Fig1_NCC.pdf", height=12,width=5)
#pdf("Fig1_NCC.pdf", height=12,width=12)

par(cex.lab=2,cex.axis=1.7, lwd=1.5)
par(mar = c(2,4.5,1,1), oma=c(2,0,0,0)) #This changes the margin of space around each plot, the default is 4. 1=bottom, 2=left, 3=top, 4=right

layout(matrix(c(1:6),3,2,byrow=TRUE), widths=c(1,4), heights=c(1,1,1), FALSE)
#layout(matrix(c(1:12),3,4,byrow=TRUE), widths=c(1,4,4,4), heights=c(1,1,1), FALSE)
#layout.show(6) 

for(site.k in 1:3){
  if(site.k==1) dat.k= dat[dat$stat==1,]
  if(site.k==2) dat.k= dat[dat$stat==51713,]
  if(site.k==3) dat.k= dat[dat$stat==55722,]
  
  dat1gen1= dat.k[dat.k$gen==1 &dat.k$fixed==1,]
  dat1gen2= dat.k[dat.k$gen==2 &dat.k$fixed==1,]
  dat1gen3= dat.k[dat.k$gen==3 &dat.k$fixed==1,]
  
  if(site.k==2)plot(dat1gen1$yr, dat1gen1$Jadult, type="l", ylim= range(80,225), col=cols[1], ylab="Julian date",xaxt='n')
  ##No y-labels
  if(site.k %in% c(1,3))plot(dat1gen1$yr, dat1gen1$Jadult, type="l", ylim= range(80,225), col=cols[1], ylab="", xaxt='n')
  
  #lines
  points(dat1gen2$yr, dat1gen2$Jadult, type="l", col=cols[2])
  points(dat1gen3$yr, dat1gen3$Jadult, type="l", col=cols[3])  
  
    
#----------------------------------------------
#TEMPS
  
  if(site.k %in% c(2)) plot(dat1gen1$yr, dat1gen1$Tpup, type="l", ylim= range(10,35), col="blue", ylab="Tpupal (°C)",xlab="Year")
  #NO LABELS
  if(site.k %in% c(1,3)) plot(dat1gen1$yr, dat1gen1$Tpup, type="l", ylim= range(10,35), col="blue", ylab="",xlab="Year")

  points(dat1gen2$yr, dat1gen2$Tpup, type="l", col="orange")
points(dat1gen3$yr, dat1gen3$Tpup, type="l", col="red")

text(1965,33,labs[site.k], cex=2)

if(site.k==1) legend("topright", legend=c("gen 1", "gen 2", "gen 3"), lty=c("solid","solid","solid"), col=c("blue","orange","red"), cex=1.5, bty="n")

} #end loop sites

#add y-axis labels
mtext("Year", side=1, line=1, outer=TRUE, cex=1.5)
dev.off()

#--------------------------
#FIG 2

#plots of evolving RNs
#all sites stacked 
library(ggplot2)
library(grid)
library(colorRamps)

setwd(paste(fdir, "\\OUT\\NCCoutput\\", sep=""))
#C1 site
C1.EvoRN<- read.csv(file="Evo.output.Site1.Fixed.2gens.EvolvingRN.csv", header =T)
head(C1.EvoRN)
names(C1.EvoRN)
C1 <- C1.EvoRN[order(C1.EvoRN$year,C1.EvoRN$Tpup),]
head(C1)

##Perfect plasticity results
C1.PP<- read.csv(file="Site1.Fixed.PerfectPlastic.csv", header =T)
C1P <- C1.PP[order(C1.PP$year,C1.PP$Tpup),]
head(C1P)

##NO plasticity results
C1.NP<- read.csv(file="Evo.output.Site1.Fixed.2gens.noPlasticyesEvo.csv", header =T)
C1NP <- C1.NP[order(C1.NP$year,C1.NP$Tpup),]

#CC = 51713 site
CC.EvoRN<- read.csv(file="Evo.output.Site51713.Fixed.3gens.EvolvingRN.csv", header =T)
head(CC.EvoRN)
names(CC.EvoRN)
CC <- CC.EvoRN[order(CC.EvoRN$year,CC.EvoRN$Tpup),]
head(CC)

CC.PP<- read.csv(file="Site51713.Fixed.PerfectPlastic.csv", header =T)
CCP <- CC.PP[order(CC.PP$year,CC.PP$Tpup),]
head(CCP)

##NO plasticity results
CC.NP<- read.csv(file="Evo.output.Site51713.Fixed.3gens.noPlasticyesEvo.csv", header =T)
CCNP <- CC.NP[order(CC.NP$year,CC.NP$Tpup),]

#site Montrose
Mn.EvoRN <- read.csv(file="Evo.output.Site55722.Fixed.3gens.EvolvingRN.csv", header =T)
head(Mn.EvoRN)
names(Mn.EvoRN)
Mn <- Mn.EvoRN[order(Mn.EvoRN$year,Mn.EvoRN$Tpup),]
head(Mn) 

Mn.PP<- read.csv(file="Site55722.Fixed.PerfectPlastic.csv", header =T)
MnP <- Mn.PP[order(Mn.PP$year,Mn.PP$Tpup),]
head(MnP)

##NO plasticity results
Mn.NP<- read.csv(file="Evo.output.Site55722.Fixed.3gens.noPlasticyesEvo.csv", header =T)
MnNP <- Mn.NP[order(Mn.NP$year,Mn.NP$Tpup),]

#evolving Rns
m<- ggplot(C1, aes(x=Tpup, y=meanAbs, color=year))
G1n<- m+geom_line(aes(group=year),size=1)+theme_bw()+xlim(10,30)+ylim(0.4,0.7)+scale_color_gradientn(colours=matlab.like(10))+ theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+theme(axis.line = element_line(color = 'black'),axis.text.x=element_text(size=17),axis.text.y=element_text(size=17),axis.title.x=element_text(size=24, vjust=0.1),axis.title.y=element_text(size=24, angle=90,vjust=0.25))+xlab(NULL)+ylab(NULL)+
  theme(plot.margin = unit(c(2,2,2,2), "mm"))+
  theme(legend.position="bottom")  +annotate("text", x=26.0,y=0.68, label= "3.0km", size=5)+annotate("text", x=15.0,y=0.68, label= "With plasticity", size=7)+
  theme(legend.key.width = unit(2, "cm"),legend.title=element_text(size=17), legend.text=element_text(size=15))

G1 <- G1n + theme(legend.position = "none")

k<- ggplot(CC, aes(x=Tpup, y=meanAbs, color=year))
G2<- k+geom_line(aes(group=year),size=1)+theme_bw()+xlim(10,30)+ylim(0.4,0.7)+scale_color_gradientn(colours=matlab.like(10))+ theme(legend.position = "none", plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+theme(axis.line = element_line(color = 'black'),axis.text.x=element_text(size=17),axis.text.y=element_text(size=17),axis.title.x=element_text(size=24, vjust=0.1),axis.title.y=element_text(size=24, angle=90,vjust=0.25),legend.title=element_text(size=17), legend.text=element_text(size=15))+xlab(NULL)+ylab(NULL)+annotate("text", x=26.0,y=0.68, label= "2.4km", size=5)+
  theme(plot.margin = unit(c(2,2,2,2), "mm"))

h<- ggplot(Mn, aes(x=Tpup, y=meanAbs, color=year))
G3<- h+geom_line(aes(group=year),size=1)+theme_bw()+xlim(10,30)+ylim(0.4,0.7)+scale_color_gradientn(colours=matlab.like(10))+ theme(legend.position = "none", plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+theme(axis.line = element_line(color = 'black'),axis.text.x=element_text(size=17),axis.text.y=element_text(size=17),axis.title.x=element_text(size=24, vjust=0.1),axis.title.y=element_text(size=24, angle=90,vjust=0.25),legend.title=element_text(size=17), legend.text=element_text(size=15))+xlab(NULL)+ylab(NULL)+
  annotate("text", x=26.0,y=0.68, label= "1.8km", size=5)+theme(plot.margin = unit(c(2,2,2,2), "mm"))

#Perfect RNs
m<- ggplot(C1P, aes(x=Tpup, y=absopt, color=year))
H1<- m+geom_line(aes(group=year),size=1)+theme_bw()+xlim(10,30)+ylim(0.4,0.7)+scale_color_gradientn(colours=matlab.like(10))+ theme(legend.position = "none", plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+theme(axis.line = element_line(color = 'black'),axis.text.x=element_text(size=17),axis.text.y=element_text(size=17),axis.title.x=element_text(size=24, vjust=0.1),axis.title.y=element_text(size=24, angle=90,vjust=0.25),legend.title=element_text(size=17), legend.text=element_text(size=15))+xlab(NULL)+ylab(NULL)+annotate("text", x=26.0,y=0.68, label= "3.0km", size=5)+
  theme(plot.margin = unit(c(2,2,2,2), "mm"))

k<- ggplot(CCP, aes(x=Tpup, y=absopt, color=year))
H2<- k+geom_line(aes(group=year),size=1)+theme_bw()+xlim(10,30)+ylim(0.4,0.7)+scale_color_gradientn(colours=matlab.like(10))+ theme(legend.position = "none", plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+theme(axis.line = element_line(color = 'black'),axis.text.x=element_text(size=17),axis.text.y=element_text(size=17),axis.title.x=element_text(size=24, vjust=0.1),axis.title.y=element_text(size=24, angle=90,vjust=0.25),legend.title=element_text(size=17), legend.text=element_text(size=15))+xlab(NULL)+ylab(NULL)+annotate("text", x=26.0,y=0.68, label= "2.4km", size=5)+
  theme(plot.margin = unit(c(2,2,2,2), "mm"))

h<- ggplot(MnP, aes(x=Tpup, y=absopt, color=year))
H3<- h+geom_line(aes(group=year),size=1)+theme_bw()+xlim(10,30)+ylim(0.4,0.7)+scale_color_gradientn(colours=matlab.like(10))+ theme(legend.position = "none", plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+theme(axis.line = element_line(color = 'black'),axis.text.x=element_text(size=17),axis.text.y=element_text(size=17),axis.title.x=element_text(size=24, vjust=0.1),axis.title.y=element_text(size=24, angle=90,vjust=0.25),legend.title=element_text(size=17), legend.text=element_text(size=15))+xlab(NULL)+ylab(NULL)+annotate("text", x=26.0,y=0.68, label= "1.8km", size=5)+
  theme(plot.margin = unit(c(2,2,2,2), "mm"))

#----------------------------------------
#no plasticity

m<- ggplot(C1NP, aes(x=Tpup, y=meanAbs, color=year))
F1<- m+geom_line(aes(group=year),size=1)+theme_bw()+xlim(10,30)+ylim(0.4,0.7)+scale_color_gradientn(colours=matlab.like(10))+ theme(legend.position = "none", plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+theme(axis.line = element_line(color = 'black'),axis.text.x=element_text(size=17),axis.text.y=element_text(size=17),axis.title.x=element_text(size=24, vjust=0.1),axis.title.y=element_text(size=24, angle=90,vjust=0.25),legend.title=element_text(size=17), legend.text=element_text(size=15))+xlab(NULL)+ylab(NULL)+annotate("text", x=15.0,y=0.68, label= "Without plasticity", size=7)+
  theme(plot.margin = unit(c(2,2,2,2), "mm"))

k<- ggplot(CCNP, aes(x=Tpup, y=meanAbs, color=year))
F2<- k+geom_line(aes(group=year),size=1)+theme_bw()+xlim(10,30)+ylim(0.4,0.7)+scale_color_gradientn(colours=matlab.like(10))+ theme(legend.position = "none", plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+theme(axis.line = element_line(color = 'black'),axis.text.x=element_text(size=17),axis.text.y=element_text(size=17),axis.title.x=element_text(size=24, vjust=0.1),axis.title.y=element_text(size=24, angle=90,vjust=0.25),legend.title=element_text(size=17), legend.text=element_text(size=15))+xlab(NULL)+ylab(NULL)+
  theme(plot.margin = unit(c(2,2,2,2), "mm"))

h<- ggplot(MnNP, aes(x=Tpup, y=meanAbs, color=year))
F3<- h+geom_line(aes(group=year),size=1)+theme_bw()+xlim(10,30)+ylim(0.4,0.7)+scale_color_gradientn(colours=matlab.like(10))+ theme(legend.position = "none", plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+theme(axis.line = element_line(color = 'black'),axis.text.x=element_text(size=17),axis.text.y=element_text(size=17),axis.title.x=element_text(size=24, vjust=0.1),axis.title.y=element_text(size=24, angle=90,vjust=0.25),legend.title=element_text(size=17), legend.text=element_text(size=15))+xlab(NULL)+ylab(NULL)+
  theme(plot.margin = unit(c(2,2,2,2), "mm"))

#----------------------------------------

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

pdf("Fig3_NCC.pdf", height=10,width=10)

lheight <- sum(legend$height)
#p <- arrangeGrob(H1, G1, H2, G2, H3, G3, ncol=2, left=textGrob("Absorptivity", rot = 90, gp=gpar(fontsize=20)))
p <- arrangeGrob(G1, F1, G2, F2, G3, F3, ncol=2, left=textGrob("Absorptivity", rot = 90, gp=gpar(fontsize=20)))

theight <- unit(20, "points")
p <- arrangeGrob(p, textGrob("Pupal Temperature (°C)", gp=gpar(fontsize=20)), heights=unit.c(unit(1, "npc") - theight, theight))
p <- arrangeGrob(p, legend, heights=unit.c(unit(1, "npc") - lheight, lheight), ncol=1)
print(p)

dev.off()

#--------------------------------------------------------------
#FIG 2
#BETA AND ABS FIG, GET RID OF DASHED LINES (NO RN evolution)
  
  #Time 
  pdf("Fig2_NCC.pdf", height=12,width=12)
 
  par(cex.lab=2,cex.axis=1.7, lwd=1.5)
  par(mar = c(2,4.5,1,0.5), oma=c(2,0,0,0)) #This changes the margin of space around each plot, the default is 4. 1=bottom, 2=left, 3=top, 4=right
  
# layout(matrix(c(1:12),3,4,byrow=TRUE), widths=c(1,4,4,4), heights=c(1,1,1), FALSE)
  #layout.show(16) 
 par(mfrow=c(3,2))
  
  for(site.k in 1:3){
    
    #SELECTION
    ## FIX ORDER
    if(site.k==1){
      with(C1.g1,plot(year,betaAbsmid,type = "l", lwd= 2, col="blue", ylab="", xlab="Year", xlim=c(1960,2010), ylim=c(-1.5,3))) #cx.lab changes the size of the axis labels
      abline(h=0,lty=2)#Horizontal line at 0 
      with(C1.g2,lines(year,betaAbsmid,lty=1, lwd= 2, col="orange")) 
      
      #No RN EVO
  #    with(C1.g1.nornevo,lines(year,betaAbsmid,type = "l",lty="dashed", lwd= 2, col="blue", ylab="", xlab="Year", xlim=c(1960,2010), ylim=c(-1.5,3))) #cx.lab changes the size of the axis labels
    #  abline(h=0,lty=2)#Horizontal line at 0 
     # with(C1.g2.nornevo,lines(year,betaAbsmid,lty="dashed", lwd= 2, col="orange")) 
        }
    
    #CC site
    if(site.k==2){
      with(CC.g1,plot(year,betaAbsmid,type = "l", lwd= 2, col="blue", ylab="Directional Selection", xlab="Year", xlim=c(1960,2010), ylim=c(-1.5,3))) #cx.lab changes the size of the axis labels
      abline(h=0,lty=2)#Horizontal line at 0 
      with(CC.g2,lines(year,betaAbsmid,lty=1, lwd= 2, col="orange")) 
      with(CC.g3,lines(year,betaAbsmid,lty=1, lwd= 2, col="red")) 
   
      #No RN EVO
   #   with(CC.g1.nornevo,lines(year,betaAbsmid,type = "l",lty="dashed", lwd= 2, col="blue", ylab="", xlab="Year", xlim=c(1960,2010), ylim=c(-1.5,3))) #cx.lab changes the size of the axis labels
   #   abline(h=0,lty=2)#Horizontal line at 0 
   #   with(CC.g2.nornevo,lines(year,betaAbsmid,lty="dashed", lwd= 2, col="orange"))
  #    with(CC.g3.nornevo,lines(year,betaAbsmid,lty="dashed", lwd= 2, col="red"))
       }
    
    ##Montrose site
    if(site.k==3){
      with(Mn.g1,plot(year,betaAbsmid,type = "l", lwd= 2, col="blue", ylab="", xlab="Year", xlim=c(1960,2010), ylim=c(-1.5,3))) #cx.lab changes the size of the axis labels
      abline(h=0,lty=2)#Horizontal line at 0 
      with(Mn.g2,lines(year,betaAbsmid,lty=1, lwd= 2, col="orange")) 
      with(Mn.g3,lines(year,betaAbsmid,lty=1, lwd= 2, col="red")) 
      
      #No RN EVO
   #   with(Mn.g1.nornevo,lines(year,betaAbsmid,type = "l",lty="dashed", lwd= 2, col="blue", ylab="", xlab="Year", xlim=c(1960,2010), ylim=c(-1.5,3))) #cx.lab changes the size of the axis labels
    #  abline(h=0,lty=2)#Horizontal line at 0 
    #  with(Mn.g2.nornevo,lines(year,betaAbsmid,lty="dashed", lwd= 2, col="orange"))
    #  with(Mn.g3.nornevo,lines(year,betaAbsmid,lty="dashed", lwd= 2, col="red"))
      }
    
    #LABELS
    text(1965,3,labs[site.k], cex=2)
    
    if(site.k==1) legend(1990,3, legend=c("gen 1", "gen 2", "gen 3"), lty=c("solid","solid","solid"), col=c("blue","orange","red"), cex=1.5, bty="n")
 #   if(site.k==1) legend(1990,2, legend=c("with rn evolution","without rn evolution"), lty=c("solid","dashed"), col=c("black","black"), cex=1.5, bty="n")
    
    #--------------------------------------------------------------------------------------
    #ABS
    
    if(site.k==1){
      with(C1.g1,plot(year,meanAbs,type = "l", lwd= 2, col="blue",  xlab="Year",ylab = "",xlim=c(1960,2010), ylim=c(0.40,0.70)))
      with(C1.g2, lines(year,meanAbs, lty=1, lwd=2,col="orange"))
   
      #No RN EVO
    #  with(C1.g1.nornevo,lines(year,meanAbs,type = "l",lty="dashed", lwd= 2, col="blue", ylab="", xlab="Year", xlim=c(1960,2010), ylim=c(-1.5,3))) #cx.lab changes the size of the axis labels
   #   abline(h=0,lty=2)#Horizontal line at 0 
  #    with(C1.g2.nornevo,lines(year,meanAbs,lty="dashed", lwd= 2, col="orange")) 
       }
    
    if(site.k==2){
      with(CC.g1,plot(year,meanAbs,type = "l", lwd= 2, col="blue",  xlab="Year",ylab = "Absorptivity",xlim=c(1960,2010), ylim=c(0.40,0.70)))
      with(CC.g2, lines(year,meanAbs, lty=1, lwd=2,col="orange"))
      with(CC.g3, lines(year,meanAbs, lty=1, lwd=2,col="red"))

      #No RN EVO
     # with(CC.g1.nornevo,lines(year,meanAbs,type = "l",lty="dashed", lwd= 2, col="blue", ylab="", xlab="Year", xlim=c(1960,2010), ylim=c(-1.5,3))) #cx.lab changes the size of the axis labels
   #   abline(h=0,lty=2)#Horizontal line at 0 
  #    with(CC.g2.nornevo,lines(year,meanAbs,lty="dashed", lwd= 2, col="orange"))
  #    with(CC.g3.nornevo,lines(year,meanAbs,lty="dashed", lwd= 2, col="red"))
      }
    
    if(site.k==3){
      with(Mn.g1,plot(year,meanAbs,type = "l", lwd= 2, col="blue",  xlab="Year",ylab = "",xlim=c(1960,2010), ylim=c(0.40,0.70)))
      with(Mn.g2, lines(year,meanAbs, lty=1, lwd=2,col="orange"))
      with(Mn.g3, lines(year,meanAbs, lty=1, lwd=2,col="red"))
      
      #No RN EVO
   #   with(Mn.g1.nornevo,lines(year,meanAbs,type = "l",lty="dashed", lwd= 2, col="blue", ylab="", xlab="Year", xlim=c(1960,2010), ylim=c(-1.5,3))) #cx.lab changes the size of the axis labels
    #  abline(h=0,lty=2)#Horizontal line at 0 
    #  with(Mn.g2.nornevo,lines(year,meanAbs,lty="dashed", lwd= 2, col="orange"))
    #  with(Mn.g3.nornevo,lines(year,meanAbs,lty="dashed", lwd= 2, col="red"))
    }
    
  } # end site loop
  
  #add y-axis labels
  mtext("Year", side=1, line=1, outer=TRUE, cex=1.5)
  dev.off()
  
  #--------------------------
  #FIG S1
  
  ## NOT UPDATED
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
    
    ####################################################

