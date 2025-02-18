data <- read.delim("./root.txt",header = T,sep = "\t")

##Length
hist(data$L,prob=T,col="light blue")
xfit<-seq(min(data$L),max(data$L),length=40)
yfit<-dnorm(xfit,mean(data$L),sd(data$L))
lines(xfit,yfit,col="red",lwd=3)

##project area
hist(data$PA,prob=T,col="light blue")
xfit<-seq(min(data$PA),max(data$PA),length=40)
yfit<-dnorm(xfit,mean(data$PA),sd(data$PA))
lines(xfit,yfit,col="red",lwd=3)

##project area
hist(data$AD,prob=T,col="light blue")
xfit<-seq(min(data$AD),max(data$AD),length=40)
yfit<-dnorm(xfit,mean(data$AD),sd(data$AD))
lines(xfit,yfit,col="red",lwd=3)


##Length_PR
hist(data$L_PR,prob=T,col="light blue")
xfit<-seq(min(data$L_PR),max(data$L_PR),length=40)
yfit<-dnorm(xfit,mean(data$L_PR),sd(data$L_PR))
lines(xfit,yfit,col="red",lwd=3)

##project area_PR
hist(data$PA_PR,prob=T,col="light blue")
xfit<-seq(min(data$PA_PR),max(data$PA_PR),length=40)
yfit<-dnorm(xfit,mean(data$PA_PR),sd(data$PA_PR))
lines(xfit,yfit,col="red",lwd=3)



data <- read.delim("./Weight.txt",header = T,sep = "\t")
##Biomass
hist(data$Biomass,prob=T,col="light blue")
xfit<-seq(min(data$Biomass),max(data$Biomass),length=40)
yfit<-dnorm(xfit,mean(data$Biomass),sd(data$Biomass))
lines(xfit,yfit,col="red",lwd=3)


##WDR
hist(data$WDR,prob=T,col="light blue")
xfit<-seq(min(data$WDR),max(data$WDR),length=40)
yfit<-dnorm(xfit,mean(data$WDR),sd(data$WDR))
lines(xfit,yfit,col="red",lwd=3)

##WDS
hist(data$WDS,prob=T,col="light blue")
xfit<-seq(min(data$WDS),max(data$WDS),length=40)
yfit<-dnorm(xfit,mean(data$WDS),sd(data$WDS))
lines(xfit,yfit,col="red",lwd=3)

##WFR
hist(data$WFR,prob=T,col="light blue")
xfit<-seq(min(data$WFR),max(data$WFR),length=40)
yfit<-dnorm(xfit,mean(data$WFR),sd(data$WFR))
lines(xfit,yfit,col="red",lwd=3)


##WF
hist(data$WF,prob=T,col="light blue")
xfit<-seq(min(data$WF),max(data$WF),length=40)
yfit<-dnorm(xfit,mean(data$WF),sd(data$WF))
lines(xfit,yfit,col="red",lwd=3)

