setwd("~/Documents/CURS 2018-2019/PIE2")
COL <- read.csv2("./Dades/COL.csv")
#setwd("F:/windows")
COL <- read.csv2("./Dades/COL.csv")
p<-2
n<-dim(COL)[1]
library(car)
library(HH)

write("___________________________________________________________________","")
write("a)","")

# MODEL
mod<-lm(C~W,COL)

# RESUM DEL MODEL
summary(mod)

#Càlculs opcionals: Intervals de confiança dels paràmetres
#confint(mod,level=0.99)

#Càlculs opcionals: SS1 Test dels paràmetres amb ordenació predeterminada
#anova(mod)

#Nota: SS3, els tests (F) sempre coincideixen amb els del resum (t), F=t^2
#Anova(mod,ty=2)

write("___________________________________________________________________","")
write("b)","")

#Gràfica amb bandes de confiança i de predicció
ci.plot(mod)

write("___________________________________________________________________","")
write("c)","")

# Diagnòstic: TENDÈNCIES
plot(predict(mod),resid(mod))
abline(h=0,lty=2)

# Diagnòstic: OUTLIERS (rstudent, rstandard)
plot(rstandard(mod))
abline(h=c(-2,0,2),lty=2)

#plot(rstudent(mod),main="rstudent")
#abline(h=c(-2,0,2),lty=2)

# Diagnòstic: LEVERAGE
#plot(hatvalues(mod))
#abline(h=c(0,2*mean(hatvalues(mod))),lty=2)

# Diagnòstic: INFLUÈNCIA (dffits, cooks.distance)
#plot(cooks.distance(mod))
#abline(h=c(0,4/n),lty=2)

#plot(dffits(mod),main="dffits")
#abline(h=c(-2*sqrt(p/n),0,2*sqrt(p/n)),lty=2)


#Diagnòstics de R
oldpar <- par( mfrow=c(2,2))
plot(mod,ask=F)
par(oldpar)

write("___________________________________________________________________","")
write("d)","")

COL$A<-factor(COL$A)
sp(C~W|A,smooth=F,col=1:20, data=COL)

#COL$GA<-factor(ceiling((COL$A-8)/2))
#sp(C~W|GA,smooth=F, data=COL)





