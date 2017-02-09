sun=TRUE #toggle for sun or shade
#fdir= "C:\\Users\\lbuckley\\Google Drive\\ColiasEvolution\\ColiasPlasticity\\"
fdir= "C:\\Users\\Buckley\\Google Drive\\ColiasEvolution\\ColiasPlasticity\\"

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#USE NEW Tsoil DATA, VARY FUR THICKNESS

#ESTIMATE LAMBDAS FOR COOP DATA AS FUNCTION OF ABSORPTIVITY
# APPLY TO NIWOT DATA
#USE ERBS TO PARTITION RADIATION

#Plant heights
heights= c(0.2, 0.5) #in m 

memory.size(max=TRUE)
memory.limit(size = 4095)

library(zoo)
library( fields)
library( evd) #for DTR
library( evdbayes) #for DTR
#library( ismev) 
#library(chron) #convert dates
#library(maptools)
#library(epicalc) 
library(RAtmosphere) #suncalc
#library(msm) #for rtnorm
library(truncnorm)
library(MASS)
#library(SDMTools)
library(gdata) #for converting to matrix
library(ks)

#source biophysical model and other functions
setwd(paste(fdir, "\\Analysis\\functions\\", sep=""))
source("ColiasBiophysMod_27May2014_wShade.R")
source("ColiasBiophysMod_20May2014.R")

#load GDD function
source("GDDfunction.R")
#source dtr function
source("DTRfunction.R")
#source zenith angle calculating function
source("ZenithAngleFunction.R")
#load radiation functions
source("RadiationModel_10Dec2013.R")

#microclimate model
setwd(paste(fdir,"//Analysis//functions//MicroclimateModel//",sep="") )
source("soil_temp_function_14Aug2014_wShade.R") #CHANGED FROM 27 MAY
#source("soil_temp_function_noextrap_20May2014.R")
source('air_temp_at_height_z_function.R') #calculate air temp at some height z

#define geometric mean
geo_mean <- function(data) {
    log_data <- log(data)
    gm <- exp(mean(log_data[is.finite(log_data)]))
    return(gm)
}
#---------------------------------------
#LOAD CLIMATE DATA NIWOT RIDGE
setwd(paste(fdir,"\\Data\\ClimateData\\Niwot\\", sep=""))
#setwd("F:\\Work\\Butterflies\\Evolution\\ClimateData\\Niwot\\")
sites= read.csv("NiwotSites.csv")
c1.clim= read.csv("C1climate.csv")
#stats= sites[,1]

#------------------------------
#LOAD CLIMATE DATA COOP
setwd(paste(fdir,"\\Data\\ClimateData\\ColoradoClimateDC\\daily\\", sep=""))
mont.clim= read.csv("COOP55722_formatted.csv")
coch.clim= read.csv("COOP51713_formatted.csv")
#Read COOP site data
sites= read.csv("COOPsites.csv")

#approximate NAs
mont.clim$Max= na.approx(mont.clim$Max,na.rm = FALSE, maxgap = 6)
mont.clim$Min= na.approx(mont.clim$Min,na.rm = FALSE, maxgap = 6)
mont.clim$Mean= na.approx(mont.clim$Mean,na.rm = FALSE, maxgap = 6)

##READ SOIL TEMP
setwd(paste(fdir,"\\OUT\\", sep=""))
Tsoil= read.csv("Tsoil_C1Montrose_14Sep2015.csv")

mont.clim$Julian= mont.clim$J
coch.clim$Julian= coch.clim$J

#pick stations and years
stats=c(1,55722)
elevs= c(3021,1777)
years= 1960:2011
days<-c(31,28,31,30,31,30,31,31,30,31,30,31) #days in months

#------------------------------------------------------
#FUNCTIONS
 
#Set min and max temps
#Tmin=30;  # Kingsolver 1983
#Tmax=40;

#Estimate air pressure in kPa #http://www.engineeringtoolbox.com/air-altitude-pressure-d_462.html
airpressure_elev<- function(h){  #H is height in meters 
  p= 101325* (1 - 2.25577*10^(-5)*h)^5.25588       
  p= p/1000 #convert to kPa
  return(p)
}

airpressures= sapply(sites$Elev, FUN=airpressure_elev) 

#Wind velocity at height z
# V_r is wind velocity at reference height

V_z<- function(V_r, z_0, z_r, z){ V_r*log((z+z_0)/z_0+1)/log((z_r+z_0)/z_0+1)}
#from Porter et al 1973

#SEE SurfaceRoughness_12May2014.R for C1 surface roughness estimate
#z=0.02 #m

#Weights temperatures based on distribution of developmental timing
Jdist= function(J1, J2, Nind=1000){
  #J1s= as.integer(rtruncnorm(Nind, a=J1-10, b=J1+10, mean = as.vector(J1), sd = 2))
  J1s= as.integer(rnorm(Nind, mean=J1,sd=2))
  Ndays= J2-J1
  Js= cbind(J1s,J1s+Ndays)
  Jall= apply(Js, MARGIN=1, FUN= function(x) c(x[1]:x[2]) )
  Jcount= rle(sort(Jall))
  return(Jcount)
}

#-------------------------------------------------
#PARAMETERS

#Demographic parameters
#Kingsolver 1983 Ecology 64
OviRate=0.73; # Ovipositing rates: 0.73 eggs/min (Stanton 1980) 
MaxEggs=700; # Max egg production: 700 eggs (Tabashnik 1980)
PropFlight= 0.5; # Females spend 50# of available activity time for oviposition-related
# Watt 1979 Oecologia
SurvDaily=0.6; # Daily loss rate for Colias p. eriphyle at Crested Butte, female values
#Hayes 1981, Data for Colias alexandra at RMBL
SurvMat=0.014; #1.4# survival to maturity

#pick stations and years
years= 1960:2011
days<-c(31,28,31,30,31,30,31,31,30,31,30,31) #days in months

#read/make species data
solar.abs= seq(0.4,0.7,0.01) #seq(0.4,0.7,0.001)
SpecDat= as.data.frame(solar.abs)
SpecDat$d=0.36
SpecDat$fur.thickness=1.46
SpecDat= as.matrix(SpecDat)
# Solar absorptivity, proportion
# D- Thoractic diameter, cm
# Fur thickness, mm

#Fur thickness from Kingsolver (1983)
#Eriphyle Montrose 0.82mm, d=3.3mm, 1.7km
#Eriphyle Skyland 1.08, d=3.5mm, 2.7km
#Meadii Mesa sco 1.46mm, d=3.6mm, 3.6km
#FT= c(0.01, 0.82, 1.46, 2)
#FUR THICKNESS: 0, eriphyle olathe, meadii mesa seco, 2

#Make array to store lambdas
#Lambda= survival*fecundity
#Matrix of lambdas
#dims: stats, year, Lambda 
Lambda_20cm<-array(NA, dim=c(length(years),length(stats),nrow(SpecDat),6,5)) #Last dimension is Lambda, FAT,Egg Viability, Lambda No Viab, Temp
dimnames(Lambda_20cm)[[1]]<-years
dimnames(Lambda_20cm)[[2]]<-stats
dimnames(Lambda_20cm)[[3]]=solar.abs
dimnames(Lambda_20cm)[[4]]<-c("gen1","gen2","gen3","gen1fixed","gen2fixed","gen3fixed")

Lambda_50cm<-array(NA, dim=c(length(years),length(stats),nrow(SpecDat),6,5)) #Last dimension is Lambda, FAT,Egg Viability, Lambda No Viab, Temp
dimnames(Lambda_50cm)[[1]]<-years
dimnames(Lambda_50cm)[[2]]<-stats
dimnames(Lambda_50cm)[[3]]=solar.abs
dimnames(Lambda_50cm)[[4]]<-c("gen1","gen2","gen3","gen1fixed","gen2fixed","gen3fixed")

#------------------------
#PERFORMANCE FUNCTIONS
#function for flight probability
fl.ph<- function(x) 1 * exp(-0.5*(abs(x-33.5)/5)^3.5)

#function for egg viability
#egg.viab<-function(x) ifelse(x<40, egg.viab<-1, egg.viab<- exp(-(x-40)))
#Set P_survival=0.868 at 45 (Kingsolver dissertation), log(0.868)=-5/t, t=35.32
egg.viab<-function(x) ifelse(x<40, egg.viab<-1, egg.viab<- exp(-(x-40)/35.32))
#plot(30:50, egg.viab(30:50))

#-------------------
#  Calculate mean times across generations
setwd(paste(fdir, "OUT\\", sep=""))
#if(sun) datyr= read.csv("DevTimeTemp_C1_55722_long_sun.csv")
#if(!sun) datyr= read.csv("DevTimeTemp_C1_55722_long_shade.csv")
datyr= read.csv("DevTimeTemp_20Nov2015_long_sun.csv")

datyr= subset(datyr, datyr$yr<2010)
datyr= subset(datyr, datyr$fixed==0)
Jmeans=aggregate(datyr, list(datyr$stat,datyr$gen), FUN=mean, na.rm=TRUE)

#=========================================
#CALCULATE DEVELOPMENT TIMING AND TEMPS

#Matrix for pupual temps
pup.temps<-array(NA, dim=c(12, length(years),length(stats),3)) #3 generations 
#Add names
dimnames(pup.temps)[[1]]= c("stat","yr","gen","Jlarv", "Jpup","Jadult","Tlarv","Tpup","Tad","Tlarv_fixed","Tpup_fixed","Tad_fixed") 

#for(yr.k in 48:length(years) ){ #loop years
for(yr.k in 1:length(years)){ #loop years 
  yr<-years[yr.k]
  print(yr)
  
  for(stat.k in 1:2){ #loop stats
    
    # USES SINGLE FUR THICKNESS
    if(stat.k==1){stat=1; dats=c1.clim; z_0_1=0.02; Tsoil.dat=subset(Tsoil, Tsoil[,"Station"]==1); SpecDat[,"d"]=0.35; SpecDat[,"fur.thickness"]=0.82; Jmelt= 141; lat=40.03; lon=-105.53; elev= 3021.0}
    #COOP data
    if(stat.k==2){stat=55722; dats=mont.clim; z_0_1=0.05; Tsoil.dat=subset(Tsoil, Tsoil[,"Station"]==55722); SpecDat[,"d"]=0.35; SpecDat[,"fur.thickness"]=0.82; Jmelt=60; lat=38.48; lon=-107.8833; elev= 2438.4; dats$Julian=dats$J}
    #if(stat.k==2){dat=coch.clim; z_0_1=0.05; Tsoil.dat=subset(Tsoil, Tsoil[,1]==51713); SpecDat[,"d"]=0.33; SpecDat[,"fur.thickness"]=0.82}
    
    #subset to year
    dat2<-subset(dats, dats$Year==yr)
    
    #subset March through September 
    dat2<-subset(dat2, dat2$Julian>60 & dat2$Julian<273)
    
    ##subset post snowment
    ##C1: Start MAy 21, J=141, mean snow melt date for C1, from Nufio analysis of C1 data
    ##Montrose:start march 1, J=60, snow depth: http://www.wrcc.dri.edu/cgi-bin/cliMAIN.pl?comont
    dat2= subset(dat2, dat2$Julian>= Jmelt)
    
    #-----------------------------------
    #calc temp at surface 
    
    #estimate daylength
    Trise.set= suncalc(dat2$Julian, Lat = lat, Long = lon, UTC = FALSE)
    set= Trise.set$sunset
    rise= Trise.set$sunrise
    
    #Match Tsoil
    Tsoil.dat1=subset(Tsoil.dat, Tsoil.dat$Year==yr )
    Tsoil.match= Tsoil.dat1[match(dat2$Julian,Tsoil.dat1$Julian),]
    
    #Tsoil min and max, +24 for shade temps
    ## SUN SOIL TEMP
    if(sun){
      Tsoil_min= apply(Tsoil.match[,5:(5+23)], FUN=min, MARGIN=1 )-273.15 #convert to C
      Tsoil_max= apply(Tsoil.match[,5:(5+23)], FUN=max, MARGIN=1 )-273.15 #convert to C
    }
    ## SHADE SOIL TEMP
    if(!sun){
      Tsoil_min= apply(Tsoil.match[,(5+24):(5+24+23)], FUN=min, MARGIN=1 )-273.15 #convert to C
      Tsoil_max= apply(Tsoil.match[,(5+24):(5+24+23)], FUN=max, MARGIN=1 )-273.15 #convert to C
    }
    
    #calculate temp at plant height
    #min , max
    dat2$Ta_plant_min= as.numeric(air_temp_at_height_z(z_0=z_0_1, z_r=1.524, z=z_0_1, T_r=dat2$Min, T_s=Tsoil_min) )
    dat2$Ta_plant_max= as.numeric(air_temp_at_height_z(z_0=z_0_1, z_r=1.524, z=z_0_1, T_r=dat2$Max, T_s=Tsoil_max) )
    dat2$Ta_plant_mean= rowMeans( dat2[,c("Ta_plant_min","Ta_plant_max")] )
    
    #----------------------------
    # ESTIMATE DEVELOPMENTAL TIMING
    #use plant height in shade
    
    #3 day average T >11.5C
    #DevZero=11.5
    
    ## change to deal with NAs
    #ind.init= which.max(rollapply(dat2$Ta_plant_mean, 3, mean, fill = NA, align = "right")> DevZero)
    #Jinit= dat2[ind.init,"Julian"] 
    
    ## or 3 consecutive days >11.5C
    #ind.init= which.max(rollapply(dat2$Ta_plant_mean, 3,sum, fill = NA, align = "right")>3)
    #Jinit= dat2[ind.init,"Julian"] 
    
    #------------------------------------------------
    
    DevZeros= c(9.2176, 11.5, 9.7) #4th and 5th, larval, pupal
    GddReqs= c(117.06, 270.39 ,101.9) 
    
    #Calculate development timing
    dat2$Tmax= dat2$Ta_plant_max
    
    #Calculate development timing
    for(gen.k in 1:3){
      
      #Assume 7 days from eclosion to eggs laid
      #Hatching ~5days (~70hrs, based on Heidi's heat shock data, Jessica's development data doesn't seem to have hatching time)
      Jlarv= ifelse(gen.k>1, Jadult+12, min(dat2$Julian) )  
      if(Jlarv>max(dat2$Julian))Jlarv=max(dat2$Julian)
      
      ##TO PUPATION
      ##calculate gdds
      DevZero= ifelse(gen.k==1, DevZeros[1],DevZeros[2])
      dat2$Tmax[dat2$Julian< Jlarv]=0 #remove gdds before interval
      gdds= apply(dat2[,c("Ta_plant_min", "Tmax")],FUN="degree.days.mat", MARGIN=1, DevZero )
      gdds[is.na(gdds)]=0
      if(gen.k==1) Jlarv= dat2[which.max(gdds>0),"Julian"]
      
      #cummulative sum
      pup.init= which.max(cumsum(gdds)> ifelse(gen.k==1, GddReqs[1],GddReqs[2])  )
      Jpup= dat2[pup.init,"Julian"] 
      if(length(Jpup)==0 | pup.init==1)Jpup=max(dat2$Julian)
      
      #PUPATION
      ##calculate gdds
      DevZero= DevZeros[3]
      dat2$Tmax[dat2$Julian< Jpup]=0 #remove gdds before interval
      gdds= apply(dat2[,c("Ta_plant_min", "Tmax")],FUN="degree.days.mat", MARGIN=1, DevZero )
      gdds[is.na(gdds)]=0
      
      #cummulative sum
      pup.end= which.max(cumsum(gdds)> GddReqs[3])
      Jadult= dat2[pup.end,"Julian"] 
      if(length(Jadult)==0 | pup.end==1)Jadult=max(dat2$Julian)
      
      #----------------------
      #Calculate temps
      
      #VARIABLE PUPAL TEMPS
      Jpups= Jdist(Jpup,Jadult)
      #TEMP FIX TO GET RID OF VALUES OFF EDGES
      keep= which(!is.na(match(Jpups$values, dat2$Julian) ))
      Jpups$values=Jpups$values[keep]
      Jpups$lengths=Jpups$lengths[keep]
      temps= dat2$Ta_plant_mean[dat2$Julian %in% Jpups$values]
      keep= which(!is.na(temps))
      Tpup= sum(temps[keep]*Jpups$lengths[keep])/sum(Jpups$lengths[keep])
      
      #FIXED PUPAL TEMPS
      Jpup.f= Jmeans[Jmeans$stat==stat & Jmeans$gen==gen.k,"Jpup"]
      Jadult.f= Jmeans[Jmeans$stat==stat & Jmeans$gen==gen.k,"Jadult"]
      Jpups= Jdist(Jpup.f,Jadult.f)
      #TEMP FIX TO GET RID OF VALUES OFF EDGES
      keep= which(!is.na(match(Jpups$values, dat2$Julian) ))
      Jpups$values=Jpups$values[keep]
      Jpups$lengths=Jpups$lengths[keep]
      temps= dat2$Ta_plant_mean[dat2$Julian %in% Jpups$values]
      keep= which(!is.na(temps))
      Tpup_fixed= sum(temps[keep]*Jpups$lengths[keep])/sum(Jpups$lengths[keep])
      
      #OTHER TEMPS
      Tlarv= mean(dat2$Ta_plant_mean[dat2$Julian %in% Jlarv:Jpup], na.rm=TRUE)
      #Tpup= mean(dat2$Ta_plant_mean[dat2$Julian %in% Jpup:Jadult], na.rm=TRUE)
      Tad= mean(dat2$Ta_plant_mean[dat2$Julian %in% Jadult:(Jadult+7)], na.rm=TRUE)
      ### ADULT TEMP IS AIR TEMP
      #Check if more than 5 NAs
      if(sum(is.na(dat2$Ta_plant_mean[dat2$Julian %in% Jlarv:Jpup])>5 )) Tlarv=NA
      if(sum(is.na(dat2$Ta_plant_mean[dat2$Julian %in% Jpup:Jadult])>5 )) Tpup=NA
      if(sum(is.na(dat2$Ta_plant_mean[dat2$Julian %in% Jadult:(Jadult+7)])>5 ))  Tad=NA
      
      #Calculate fixed temps
      Js= floor(Jmeans[Jmeans$stat==stat & Jmeans$gen==gen.k,"Jlarv"]):floor(Jmeans[Jmeans$stat==stat & Jmeans$gen==gen.k,"Jpup"])
      Tlarv_fixed=mean(dat2$Ta_plant_mean[dat2$Julian %in% Js], na.rm=TRUE)
      #  Js= floor(Jmeans[Jmeans$stat==stat & Jmeans$gen==gen.k,"Jpup"]):floor(Jmeans[Jmeans$stat==stat & Jmeans$gen==gen.k,"Jadult"])
      #  Tpup_fixed=mean(dat2$Ta_plant_mean[dat2$Julian %in% Js], na.rm=TRUE)
      Js= floor(Jmeans[Jmeans$stat==stat & Jmeans$gen==gen.k,"Jadult"]):floor(Jmeans[Jmeans$stat==stat & Jmeans$gen==gen.k,"Jadult"]+7)
      Tad_fixed=mean(dat2$Ta_plant_mean[dat2$Julian %in% Js], na.rm=TRUE)
      
      #Write data in array
      pup.temps[,yr.k, stat.k, gen.k]=c(stat,yr,gen.k,Jlarv,Jpup,Jadult,Tlarv,Tpup,Tad,Tlarv_fixed,Tpup_fixed,Tad_fixed)
    } #end loop generation
    
    #---------------------
  
  #if(nrow(dat2)>1 & !is.na(dat2.temp)){ #check data for dat exists
  
  # Calculate the diurnal temperature trend, Parton and Logan 1981
  Tmat= cbind(dat2$Max, dat2$Min, rise, set )
  J= dat2$Julian
  
  #RUN EVERY HOUR
  hr.dec= seq(1, 24, 1)
  
  #arrays for storing temperature and radiation data
  Thr.mat= matrix(NA, nrow(Tmat),length(hr.dec) )
  Rhr.mat= array(data=NA, dim=c(nrow(Tmat),length(hr.dec),3) ) #columns for direct, difuse, reflected radiation
  Te.mat_20cm= array(data=NA, dim=c(3,nrow(Tmat),length(hr.dec), nrow(SpecDat), 1 ))
  Te.mat_50cm= array(data=NA, dim=c(3,nrow(Tmat),length(hr.dec), nrow(SpecDat), 1 ))    
#------------------------------
#Calculate hourly temp and radiation
for(hc in 1:length(hr.dec) ){ 
  hr= hr.dec[hc]
  
  Thr.mat[,hc]=apply(Tmat,FUN=Thour.mat, MARGIN=1, Hr=hr,  alpha=1.86, beta= -0.17, gamma=2.20)
  
  #PICK TAU, atmospheric transmisivity, from distribution for k_t
  #USES KERNAL ESTIMATE FIT TO HOURLY NREL SRRL DATA (loaded when sourcing solar radiation functions)
  
  if(round(hr)>5 & round(hr)<21)  taus= rkde(fhat= kdes[[ round(hr)-5]], n=nrow(Tmat) )
  if (!(round(hr)>5 & round(hr)<21)) taus= rep(0, nrow(Tmat))
  
  #set negative values to zero, CHECK HOW TO CONSTRAIN
  taus[taus<0]=0
  #CONSTRAIN TAUs BETWEEN 7AM AND 3PM, ###CHECK
  if(hr>6 & hr<16) taus[taus<0.2]=0.2
  
  #calculate zenith in radians
  psis= unlist(lapply(J, FUN=zenith.rad, Hr=hr, lat=lat, lon=lon))
  
  #RADIATION FROM CAMPBELL & NORMAN
  J.mat= cbind(J, psis, taus)
  Rdat=apply(J.mat,FUN=calc.rad, MARGIN=1, lat=lat, lon=lon, elev=elev)
  #returns direct, diffuse, reflected
  #set negative radiation values to zero ###CHECK
  Rdat[Rdat<0]=0
  
  R_total= colSums(Rdat[1:2,])
  
  #USE ERBS TO REPARTITION RADIATION (Olyphant 1984)
  #Separate Total radiation into components
  #kt is clearness index
  # Models presented in Wong and Chow. 2001. Applied Energy 69(2001):1991-224
  #Use Erbs et al model
  
  #kd- diffuse fraction
  kd= rep(NA, length(taus))
  
  inds= which(taus<0.22) 
  kd[inds]= 1-0.09*taus[inds]
  inds= which(taus>0.22 & taus<0.8) 
  kd[inds]= 0.9511 -0.1604*taus[inds] +4.388*taus[inds]^2 -16.638*taus[inds]^3 +12.336*taus[inds]^4
  inds= which(taus>=0.8)
  kd[inds]= 0.125 #Correction from 16.5 for Niwot from Olyphant 1984
  
  #direct and diffuse #columns for direct, difuse, reflected radiation
  Rhr.mat[,hc,1]<- R_total*(1-kd)
  Rhr.mat[,hc,2]<- R_total*(kd)
  Rhr.mat[,hc,3]<- Rdat[3,]
  
} #end hour loop

#----------------------
#Load Tsoil

#data that will be used as input for the function.
#collapse matrix
temperature_vector<- unmatrix(Thr.mat,byrow=T)
time_vector<- rep(hr.dec, nrow(Thr.mat) )
wind_speed_vector<-rep(0.4, length(temperature_vector)) ####GET NIWOT WIND DATA
solrad_vector<- unmatrix(Rhr.mat[,,1],byrow=T)

Tsoilv= unmatrix(Tsoil.match[,5:28],byrow=T)-273.15 #convert to C
Tsoil_shv=  unmatrix(Tsoil.match[,29:52],byrow=T)-273.15

#calculate butterfly temp at plant height
Ta_20cm= air_temp_at_height_z(z_0=z_0_1, z_r=1.524, z=0.2, T_r=temperature_vector, T_s=Tsoilv)
Ta_50cm= air_temp_at_height_z(z_0=z_0_1, z_r=1.524, z=0.5, T_r=temperature_vector, T_s=Tsoilv)
#Shade
Ta_20cm_sh= air_temp_at_height_z(z_0=z_0_1, z_r=1.524, z=0.2, T_r=temperature_vector, T_s=Tsoil_shv)
Ta_50cm_sh= air_temp_at_height_z(z_0=z_0_1, z_r=1.524, z=0.5, T_r=temperature_vector, T_s=Tsoil_shv)

#Turn back into matrix
Tsoil.mat= Tsoil.match[,5:28]-273.15
Ta_20cm.mat= matrix(Ta_20cm, nrow=nrow(Thr.mat), ncol=ncol(Thr.mat), byrow=T)
Ta_50cm.mat= matrix(Ta_50cm, nrow=nrow(Thr.mat), ncol=ncol(Thr.mat), byrow=T)

#SHADE
Tsoil.mat_sh= Tsoil.match[,29:52]-273.15
Ta_20cm.mat_sh= matrix(Ta_20cm_sh, nrow=nrow(Thr.mat), ncol=ncol(Thr.mat), byrow=T)
Ta_50cm.mat_sh= matrix(Ta_50cm_sh, nrow=nrow(Thr.mat), ncol=ncol(Thr.mat), byrow=T)

#-----------------------
#loop hours

for(hc in 6:20){ 
  hr= hr.dec[hc]
  
  #Calculate zenith in degrees
  psi=zenith(J, lat, lon, hr)
  psi[psi>=80]=80 #set zenith position below horizon to psi=80degrees
  
  wind_20cm= V_z(V_r= 0.4, z_0=0.02, z_r=1.524, z=0.2)
  wind_20cm= rep(wind_20cm, length(psi))
  wind_50cm= V_z(V_r= 0.4, z_0=0.02, z_r=1.524, z=0.5)
  wind_50cm= rep(wind_50cm, length(psi))
  
  #Vary SPECIES
  for(spec.k in 1:nrow(SpecDat) ){ #loop species
    
    ft.k=1 #loop fur thickness
    
    #Convert to Te
    #20cm plant height
    Tall= cbind(Ta_20cm.mat[,hc], Ta_20cm.mat_sh[,hc], Tsoil.mat[,hc], Tsoil.mat_sh[,hc], wind_20cm, Rhr.mat[,hc,1],Rhr.mat[,hc,2], psi)
    Te.mat_20cm[1,,hc,spec.k, ft.k]<-apply(Tall,MARGIN=1, FUN=biophys.var_sh, D=SpecDat[spec.k,"d"], delta= SpecDat[spec.k,"fur.thickness"], alpha=SpecDat[spec.k,"solar.abs"])
    #Set negative Te estimates to Ta ##only a few, but CHECK
    inds= which(Te.mat_20cm[1,,hc,spec.k,ft.k]<0)
    Te.mat_20cm[1,inds,hc,spec.k, ft.k]= Ta_20cm.mat[inds,hc]
    
    #---------------
    
    #50cm plant height
    Tall= cbind(Ta_50cm.mat[,hc], Ta_50cm.mat_sh[,hc], Tsoil.mat[,hc], Tsoil.mat_sh[,hc], wind_50cm, Rhr.mat[,hc,1],Rhr.mat[,hc,2], psi)
    Te.mat_50cm[1,,hc,spec.k, ft.k]<-apply(Tall,MARGIN=1, FUN=biophys.var_sh, D=SpecDat[spec.k,"d"], delta= SpecDat[spec.k,"fur.thickness"], alpha=SpecDat[spec.k,"solar.abs"])
    #Set negative Te estimates to Ta ##only a few, but CHECK
    inds= which(Te.mat_50cm[1,,hc,spec.k,ft.k]<0)
    Te.mat_50cm[1,inds,hc,spec.k, ft.k]= Ta_50cm.mat[inds,hc]
   #------------------------ 
   
    #DEMOGRAPHY
    #Flight probability
    Te.mat_20cm[2,,hc,spec.k, ft.k]<-fl.ph(Te.mat_20cm[1,,hc,spec.k, ft.k])
    Te.mat_50cm[2,,hc,spec.k, ft.k]<-fl.ph(Te.mat_50cm[1,,hc,spec.k, ft.k])
    
    #Egg viability
    Te.mat_20cm[3,,hc,spec.k, ft.k]<-egg.viab(Te.mat_20cm[1,,hc,spec.k, ft.k])
    Te.mat_50cm[3,,hc,spec.k, ft.k]<-egg.viab(Te.mat_50cm[1,,hc,spec.k, ft.k])
    
  } #end species loop
} #end hour loop
#----------------------------------------
#****************************************
EV1=matrix(NA,2, nrow(SpecDat))

for(spec.k in 1:nrow(SpecDat) ){ #loop species
  
  ft.k=1 #fur thickness
  Te.mat= Te.mat_20cm[,,,spec.k,ft.k]
  Jall=J
  
  for(gen.k in 1:6 ){ #loop generation
    
    #get fligh t dates
    if(gen.k<4)Jfl=pup.temps["Jadult",yr.k, stat.k, gen.k]
    if(gen.k>3)Jfl= floor(Jmeans[Jmeans$stat==stat & Jmeans$gen==(gen.k-3),"Jadult"])
   
    #average over hours
    FAT= rowSums(Te.mat[2,,], na.rm=TRUE)
    EggViab= apply(Te.mat[3,,], FUN=geo_mean, MARGIN=1) #Egg viability GEOMETRIC MEAN ACROSS HOURS
    Temps= rowMeans(Te.mat[1,,], na.rm=TRUE)
    
    ##CALCULATE EGG VIABILITY OVER 5 DAY PERIOD (GEOMETRIC MEAN ACROSS HOURS)
    #sample flight day from truncated normal distribution
    Nind=1000 #changed from 100
    f.low= max(Jfl-7,min(dat2$Julian)+2)
    f.up= min(Jfl+7,max(dat2$Julian)-2)
    
    flightday= round(rtruncnorm(Nind, a=f.low, b=f.up, mean = Jfl, sd = 2) )
    f.ind= match(flightday, dat2$Julian)
    #if NA day, use mean
    f.ind[is.na(f.ind)]<-match(Jfl, dat2$Julian)
    
    #calculate geometric mean of egg viability within flight period
    ev.ind=sapply(f.ind, function(x)  geo_mean(EggViab[(x-2):(x+2)]) )
    #AVERAGE FAT OVER DAYS
    FAT.ind= sapply(f.ind, function(x)  mean(FAT[(x-2):(x+2)], na.rm=TRUE) )
    #AVERAGE TEMP
    T.ind= sapply(f.ind, function(x)  mean(Temps[(x-2):(x+2)], na.rm=TRUE) )
  
    Eggs.ind= 60*PropFlight*OviRate*FAT.ind * ev.ind #account for Egg viability
    Eggs.ind_noViab= 60*PropFlight*OviRate*FAT.ind
    
    #Means across individuals
    Eggs= mean(Eggs.ind)
    Eggs_noViab= mean(Eggs.ind_noViab)
    EV1[1,spec.k]= mean(FAT.ind)
    EV1[2,spec.k]= mean(ev.ind)
    
    if(!is.nan(Eggs)){
      MaxDay=5
      Lambda1=0
      for(day in 1:MaxDay){
        Eggs1= min(Eggs, MaxEggs-Eggs*(day-1))  ###LIMIT MAX NUMBER EGGS
        if(Eggs1<0) Eggs1=0
        Lambda1= Lambda1+ SurvMat * SurvDaily^day *Eggs1;                        
      }#end loop days
      
      Lambda_20cm[yr.k, stat.k,spec.k,gen.k,1]= Lambda1
      Lambda_20cm[yr.k, stat.k,spec.k,gen.k,2]= mean(FAT.ind) #FAT
      Lambda_20cm[yr.k, stat.k,spec.k,gen.k,3]= mean(ev.ind) #egg viability
      Lambda_20cm[yr.k, stat.k,spec.k,gen.k,5]= mean(T.ind, na.rm=T) #Temp
    }#Check Eggs
    
    #WITHOUT EGG VIAB
    if(!is.nan(Eggs)){
      MaxDay=5
      Lambda1=0
      for(day in 1:MaxDay){
        Eggs1= min(Eggs_noViab, MaxEggs-Eggs_noViab*(day-1))  ###LIMIT MAX NUMBER EGGS
        if(Eggs1<0) Eggs1=0
        Lambda1= Lambda1+ SurvMat * SurvDaily^day *Eggs1;                        
      }#end loop days
      
      Lambda_20cm[yr.k, stat.k,spec.k,gen.k,4]= Lambda1
    }#Check Eggs
    
  } #end loop generation
} #end loop species

} #end loop stats
} #end loop years


#===================================================
#WRITE OUT

dat= as.data.frame(t ( cbind(pup.temps[1:9,,1,1], pup.temps[1:9,,2,1],pup.temps[1:9,,1,2], pup.temps[1:9,,2,2],pup.temps[1:9,,1,3], pup.temps[1:9,,2,3])) )
colnames(dat)= c("stat","yr","gen","Jlarv", "Jpup","Jadult","Tlarv","Tpup","Tad") 
dat$fixed=0

dat.f=dat
dat.f[,4:10]=NA
dat.f$fixed=1
dat.f$Tlarv= c(pup.temps[10,,1,1],pup.temps[10,,2,1],pup.temps[10,,1,2],pup.temps[10,,2,2],pup.temps[10,,1,3],pup.temps[10,,2,3])
dat.f$Tpup= c(pup.temps[11,,1,1],pup.temps[11,,2,1],pup.temps[11,,1,2],pup.temps[11,,2,2],pup.temps[11,,1,3],pup.temps[11,,2,3])
dat.f$Tad= c(pup.temps[12,,1,1],pup.temps[12,,2,1],pup.temps[12,,1,2],pup.temps[12,,2,2],pup.temps[12,,1,3],pup.temps[12,,2,3])

#add fixed dates
dat.f[dat.f$stat==1 & dat.f$gen==1, c("Jlarv","Jpup","Jadult")]= Jmeans[Jmeans$stat==1 & Jmeans$gen==1, c("Jlarv","Jpup","Jadult")]
dat.f[dat.f$stat==1 & dat.f$gen==2, c("Jlarv","Jpup","Jadult")]= Jmeans[Jmeans$stat==1 & Jmeans$gen==2, c("Jlarv","Jpup","Jadult")]
dat.f[dat.f$stat==1 & dat.f$gen==3, c("Jlarv","Jpup","Jadult")]= Jmeans[Jmeans$stat==1 & Jmeans$gen==3, c("Jlarv","Jpup","Jadult")]

dat.f[dat.f$stat==55722 & dat.f$gen==1, c("Jlarv","Jpup","Jadult")]= Jmeans[Jmeans$stat==55722 & Jmeans$gen==1, c("Jlarv","Jpup","Jadult")]
dat.f[dat.f$stat==55722 & dat.f$gen==2, c("Jlarv","Jpup","Jadult")]= Jmeans[Jmeans$stat==55722 & Jmeans$gen==2, c("Jlarv","Jpup","Jadult")]
dat.f[dat.f$stat==55722 & dat.f$gen==3, c("Jlarv","Jpup","Jadult")]= Jmeans[Jmeans$stat==55722 & Jmeans$gen==3, c("Jlarv","Jpup","Jadult")]

dat_gens= rbind(dat, dat.f)

setwd(paste(fdir, "\\OUT\\", sep=""))
if(sun) write.csv(dat_gens, "DevTimeTemp_C1_55722_long_sun_4Dec2015.csv", row.names=FALSE)
if(!sun) write.csv(dat_gens, "DevTimeTemp_C1_55722_long_shade_4Dec2015.csv", row.names=FALSE)

#------------------------------------
#PUT LAMBDA IN LONG FORMAT AND WRITE OUT

for(stat.k in 1:length(stats)){ #loop stats
  
  for(gen.k in 1:6){ #loop generation
    
    lambda= unmatrix(Lambda_20cm[,stat.k,,gen.k,1], byrow=FALSE)
    
    dat.lamb=as.data.frame(lambda)
    n.lamb=strsplit(names(lambda), ":")
    n.lamb=do.call(rbind, n.lamb) 
    
    dat.lamb$Year= n.lamb[,1]
    dat.lamb$Abs= n.lamb[,2]
    
    if(gen.k==1){dat.lamb$Gen=1; dat.lamb$Fixed=0}
    if(gen.k==2){dat.lamb$Gen=2; dat.lamb$Fixed=0}
    if(gen.k==3){dat.lamb$Gen=3; dat.lamb$Fixed=0}
    if(gen.k==4){dat.lamb$Gen=1; dat.lamb$Fixed=1}
    if(gen.k==5){dat.lamb$Gen=2; dat.lamb$Fixed=1}
    if(gen.k==6){dat.lamb$Gen=3; dat.lamb$Fixed=1}
    
    #### FIX STATION AND ELEV INFO
    dat.lamb$Station= stats[stat.k]
    dat.lamb$Elev= elevs[stat.k]
    
    #FAT
    dat.lamb$FAT= unmatrix(Lambda_20cm[,stat.k,,gen.k,2], byrow=FALSE)
    
    #EGG VIAB
    dat.lamb$EggViab= unmatrix(Lambda_20cm[,stat.k,,gen.k,3], byrow=FALSE)
    
    #LAMBDA NO EGG VIAB
    dat.lamb$lambda_noEV= unmatrix(Lambda_20cm[,stat.k,,gen.k,4], byrow=FALSE)
    
    #TEMP
    dat.lamb$temp= unmatrix(Lambda_20cm[,stat.k,,gen.k,5], byrow=FALSE)
    
    if(gen.k==1) dat.all=dat.lamb
    if(gen.k>1) dat.all= rbind(dat.all, dat.lamb)
  } #end loop generation
  
  if(stat.k==1) dat.all1=dat.all
  if(stat.k>1) dat.all1= rbind(dat.all1, dat.all)
} #end loop stats

dat.all_20cm= dat.all1

#------------------------

if(sun) write.csv(dat.all_20cm, "LambdaLong_20cm_C1_55722_sun_4Dec2015.csv", row.names=FALSE)
if(!sun) write.csv(dat.all_20cm, "LambdaLong_20cm_C1_55722_shade_4Dec2015.csv", row.names=FALSE)

datr= dat.all_20cm[dat.all_20cm$Year=="1960",]
datr= datr[datr$Station==55722,]
datr= datr[datr$Gen==2,]
plot(datr$Abs, datr$lambda, type="l")

#------------------------------------
#FITNESS ACROSS GENERATIONS

setwd(paste(fdir, "OUT\\", sep=""))
datyr= read.csv("LambdaLong_20cm_C1_55722_sun.csv")

colMeans(subset(datyr, datyr$Station==55722 & datyr$Gen==2 &datyr$Fixed==1), na.rm=TRUE)
colMeans(subset(datyr, datyr$Station==55722 & datyr$Gen==3 &datyr$Fixed==1), na.rm=TRUE)

#------------------------------------
#PLOT
dat.s1= dat.all_20cm[(dat.all_20cm$Year==2007 & dat.all_20cm$Fixed==0 & dat.all_20cm$Station==55722),]
dat.s1= dat.s1[order(dat.s1$Gen, dat.s1$Abs),]
plot(dat.s1$Abs, dat.s1$lambda, type="l")