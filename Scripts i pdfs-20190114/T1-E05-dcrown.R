setwd("~/Documents/CURS 2018-2019/PIE2")
library(car)
dd <- read.csv2("./Dades/dcrown.csv")
dd$RP<-dd$PB/dd$PT
scatterplotMatrix(dd,smooth=F,diagonal=F)
dd$LDCrown<-log(dd$DCrown)
dd$LRP<-log(dd$RP)
dd$LPT<-log(dd$PT)
dd$LHT<-log(dd$HT)
dd$LA<-log(dd$A)

write("___________________________________________________________________","")
write("a)","")

summary(modAc<-lm(DCrown~I(PB/PT)+PT+HT+A,dd))
summary(modA<-lm(DCrown~RP+PT+HT+A,dd))


plot(predict(modA),resid(modA),pch=3)
abline(h=0,lty=2)

plot(modA,ask=F)


plot(rstudent(modA),pch=3)
abline(h=c(-3,-2,0,2,3),lty=2)

write("___________________________________________________________________","")
write("b)","")

summary(modBc<-lm(log(DCrown)~log(PB/PT)+log(PT)+log(HT)+log(A),dd))
summary(modB<-lm(LDCrown~LRP+LPT+LHT+LA,dd))

plot(predict(modB),resid(modB),pch=3)
abline(h=0,lty=2)

plot(modB,ask=F)

plot(rstudent(modB),pch=3)
abline(h=c(-3,-2,0,2,3),lty=2)

# Extra: modB no lineal nls
# start parametres estimats de modB (arrodonits)
summary(modBnl<-nls(DCrown~exp(b0+b1*LRP+b2*LPT+b3*LHT+b4*LA),start=list(b0=1.7,b1=0.3,b2=0.9,b3=0.2,b4=0.06),data=dd))

plot(predict(modBnl),resid(modBnl),pch=3)
abline(h=0,lty=2)

library(nlme)
plot(modBnl,abline=c(-3,-2,0,2,3))

write("___________________________________________________________________","")
write("a)+b) => c)","")

write("___________________________________________________________________","")
write("d)","")


dp0<-data.frame(PT=c(0.4,0.64),PB=c(0.6,0.9),HT=c(2.3,2.8),A=10)
dpb<-data.frame(LPT=log(c(0.4,0.64)),LRP=log(c(0.6,0.9)/c(0.4,0.64)),LHT=log(c(2.3,2.8)),LA=log(10))

exp(predict(modB,dpb,interval="prediction",level=0.95))
exp(predict(modBc,dp0,interval="prediction",level=0.95))

predict(modAc,dp0,interval="prediction",level=0.95)

