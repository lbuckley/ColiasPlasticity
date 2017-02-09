#GlobalET (extraterrestrial) is available for 2006-2010. The values do not change very much from one year to the next.
#the 1991 and 2004 plots estimate k_t using the GlobalET data from 2010. 

setwd("\\\\bioark.bio.unc.edu\\BuckleyLab\\ClimateData\\radiation calculation")

par(mfrow=c(2,3))
years<-c(2007,2008,2009,2010)
for(y in years){

data<-read.table(paste(y,'.txt',sep=''),skip=1,sep=',')
#colnames(data)<-c("Date","Time","Global","Diffuse") #for 1991
colnames(data)<-c("Date","Time","Global","GlobalET","Direct","Diffuse","Cloud","OpCloud")

data$DT<-paste(data$Date,data$Time,sep=' ')
data$DT2<-strptime(data$DT,"%m/%d/%Y %H") #convert to standard format
data$DT3<-strptime(substr(data$DT2,6,19),"%m-%d %H:%M:%S") #get dates without the year so you can compare data from multiple years on the same graph

k_d<-matrix(NA,nrow(data),1) #matrix for the diffuse radiation from Erbs model
k_t<-matrix(NA,nrow(data),1) #ratio of total radiation to extraterrestrial radiation

for(i in 1:nrow(data)){
	k<-data$Global[i]/data$GlobalET[i] #value of k_t from Erbs model
	#k<-data$Cloud[i]/100 #incorrect...
	k_t[i]<-k
	##if(i !=1177){ #added this for the 2004 plot because of a NaN value
	#Erbs model
	if(k>=0){
		if(k>.22 & k<=.80){
			k_d[i]<-(.9511-.1604*k + 4.388*k^2 - 16.638*k^3 + 12.336*k^4)}
		if(k<=.22){
			k_d[i]<-(1-.09*k/100)}
		if(k>.80){
			k_d[i]<-.165}
	}
	#}
}

#plotting the diffuse fraction vs k_t
plot(k_t,data$Diffuse/data$Global,ylim=c(0,1),xlim=c(0,1))
points(k_t,k_d,col="blue") #add modeled diffuse fraction vs k_t
} #end for(y in years) loop

#compared GlobalET between years by plotting data$GlobalET for one year, running the script to find GlobalET for another year and adding the points to the graph
plot(data$DT3,data$GlobalET,type='l')
points(data$DT3,data$GlobalET,col='blue') #note that the script must be run a second time to get GlobalET and DT3 for a different year prior to running this line


###things used in the original plots
#cor.test(k_d,data$Diffuse/data$Global)
#plot(k_d,data$Diffuse/data$Global,ylim=c(0,1))
#data$DG<-data$Diffuse/data$Global
#summary(lm(data$DG~k_d))
#abline(lm(data$DG~k_d))