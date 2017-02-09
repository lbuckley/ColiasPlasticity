setwd("\\\\bioark.bio.unc.edu\\BuckleyLab\\ClimateData\\radiation calculation")
data<-read.table('2010.txt',skip=1,sep=',')
colnames(data)<-c("Date","Time","Global","GlobalET","Direct","Diffuse","Cloud","OpCloud")

data$DT<-paste(data[,1],data[,2],sep=' ')
data$DT2<-strptime(data$DT,"%m/%d/%Y %H")

k_d<-matrix(NA,nrow(data),1)
k2<-matrix(NA,nrow(data),1)

for(i in 1:nrow(data)){
	k<-data$Global[i]/data$GlobalET[i]
	#k<-data$Cloud[i]/100
	k2[i]<-k
	if(k>=0){
		if(k>.22 & k<=.80){
			k_d[i]<-(.9511-.1604*k + 4.388*k^2 - 16.638*k^3 + 12.336*k^4)}
		if(k<=.22){
			k_d[i]<-(1-.09*k/100)}
		if(k>.80){
			k_d[i]<-.165}
	}
}


cor.test(k_d,data$Diffuse/data$Global)
plot(k_d,data$Diffuse/data$Global,ylim=c(0,1))
data$DG<-data$Diffuse/data$Global
summary(lm(k_d~data$DG))  #need to switch order to y~x
abline(lm(k_d~data$DG))  #need to switch order to y~x

summary(lm(data$DG~k_d))  
abline(lm(data$DG~k_d)) 