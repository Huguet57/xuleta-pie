library(car)
dd<-read.csv2("ex7.csv")
sp(failing/size~Load,dd,smooth=F,boxplot=F)
m<-glm(cbind(failing,size-failing)~Load,family=binomial(),dd)
summary(m)
m$family
(eta<-m$coefficients[1]+m$coefficients[2]*dd$Load)
(p<-exp(eta)/(1+exp(eta)))
predict(m)
predict(m,ty="response")

points(dd$Load,p,col="red",pch="+")
Loads<-(25:45)*100
etas<-m$coefficients[1]+m$coefficients[2]*Loads
ps<-exp(etas)/(1+exp(etas))
lines(Loads,ps,col="red")

resid(m,ty="pearson")
(r<-(dd$failing-p*dd$size)/sqrt(dd$size*p*(1-p)))

(PS<-sum(r^2))
pchisq(PS*8,8)
pchisq(PS*8,8,lower.tail=F)
(pvalor<-2*min(pchisq(PS*8,8),pchisq(PS*8,8,lower.tail=F)))

(IC95<-c(qchisq(0.025,8)/8,qchisq(0.975,8)/8))

