setwd("~/Documents/CURS 2018-2019/PIE2")
library(RcmdrMisc)
library(car)
dd <- read.csv2("./Dades/diaryp.csv")
dd$lDays<-log(dd$Days)

sp(PROD~Days, smooth=F, boxplots=F, data=dd)

with(dd,plot(Days,exp(predict(lm(log(PROD)~Days+I(log(Days))))),ty="l",ylim=c(min(PROD),max(PROD))))
with(dd,points(Days,PROD,col="red",pch="+"))

write("___________________________________________________________________","")
write("a1)","")

summary(m1<-lm(log(PROD)~Days+lDays,data=dd))
with(dd,plot(Days,exp(predict(m1)),ty="l",ylim=c(min(PROD),max(PROD))))
with(dd,points(Days,PROD,col="red",pch="+"))

plot(m1,which=1)
plot(dd$Days,m1$res)
abline(h=0,lty=2)

write("___________________________________________________________________","")
write("a2)","")

summary(m1nl<-nls(PROD~exp(a+b*Days+c*lDays),start=list(a=3,b=-0.01,c=.2),data=dd))

with(dd,plot(Days,predict(m1nl),ty="l",ylim=c(min(PROD),max(PROD))))
with(dd,points(Days,PROD,col="red",pch="+"))

plot(dd$Days,dd$PROD)
lines(dd$Days,predict(m1nl))
plot(dd$Days,resid(m1nl))
abline(h=0,lty=2)

write("___________________________________________________________________","")
write("b1)","")

summary(mg<-glm(PROD~Days+lDays,family=Gamma(link="log"),data=dd))
summary(glm(PROD~Days+lDays,family=quasi(link="log",var="mu^2"),data=dd))

with(dd,plot(Days,predict(mg,ty="response"),ty="l",ylim=c(min(PROD),max(PROD))))
with(dd,points(Days,PROD,col="red",pch="+"))

plot(mg,which=1)
plot(dd$Days,resid(mg))
abline(h=0,lty=2)

write("___________________________________________________________________","")
write("b2)","")

summary(mn<-glm(PROD~Days+lDays,family=gaussian(link="log"),data=dd))
summary(glm(PROD~Days+lDays,family=quasi(link="log",var="constant"),data=dd))
with(dd,plot(Days,predict(mn,type = "response"),ty="l",ylim=c(min(PROD),max(PROD))))
with(dd,points(Days,PROD,col="red",pch="+"))

plot(mn,which=1)
plot(dd$Days,resid(mn))
abline(h=0,lty=2)

write("___________________________________________________________________","")
write("b3)","")

summary(mq<-glm(PROD~Days+lDays,family=quasi(link="log",var="mu"),data=dd))
summary(glm(PROD~Days+lDays,family=poisson(link="log"),data=dd))
summary(glm(PROD~Days+lDays,family=quasipoisson(link="log"),data=dd))
with(dd,plot(Days,predict(mq,type = "response"),ty="l",ylim=c(min(PROD),max(PROD))))
with(dd,points(Days,PROD,col="red",pch="+"))

plot(mq,which=1)
plot(dd$Days,resid(mq))
abline(h=0,lty=2)
residualPlots(mq)

write("___________________________________________________________________","")
write("       EXTRA","")
write("___________________________________________________________________","")


# mv
c(mg=logLik(mg),mn=logLik(mn))

# R2
c(mg=1-mg$dev/mg$null.dev,mn=1-mn$dev/mn$null.dev,mq=1-mq$dev/mq$null.dev)


plot(rstudent(mg,ty="deviance"))
points(rstudent(mg,ty="pearson"),col="red",pch="+")
abline(h=c(-2,0,2),lty=2)

plot(rstudent(mn,ty="deviance"))
points(rstudent(mn,ty="pearson"),col="red",pch="+")
abline(h=c(-2,0,2),lty=2)

plot(rstudent(mq,ty="deviance"))
points(rstudent(mq,ty="pearson"),col="red",pch="+")
abline(h=c(-2,0,2),lty=2)

sp(sqrt(abs(resid(mg,ty="deviance")))~predict(mg),boxplots=F,smooth=F)
sp(sqrt(abs(resid(mg,ty="pearson")))~predict(mg),boxplots=F,smooth=F)

sp(sqrt(abs(resid(mn,ty="deviance")))~predict(mg),boxplots=F,smooth=F)
sp(sqrt(abs(resid(mn,ty="pearson")))~predict(mg),boxplots=F,smooth=F)

sp(sqrt(abs(resid(mq,ty="deviance")))~predict(mg),boxplots=F,smooth=F)
sp(sqrt(abs(resid(mq,ty="pearson")))~predict(mg),boxplots=F,smooth=F)

