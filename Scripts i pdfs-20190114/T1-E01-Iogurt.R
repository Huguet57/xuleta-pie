setwd("~/Documents/CURS 2018-2019/PIE2")
dd<-read.csv2("./Dades/Iogurt.csv")
head(dd)
library(car)
library(tables)

#====================================================
# (a) Descriptiva

# Gràfiques

sp(pH~dia|Ferm,dd,smooth=F)
sp(strep~dia|Ferm,smooth=F,dd)
sp(lactob~dia|Ferm,smooth=F,dd)

# Taules

dd$Fdia<-as.factor(dd$dia)
tabular((pH+strep+lactob)*Ferm*((n=1)+mean+sd)~Fdia,dd)

# o bé per separat tabular(pH*Ferm*((n=1)+mean+sd)~Fdia,dd) ...

#====================================================
# (b) Comparacions de 2

# pH dia 0
t.test(pH~Ferm,dd[dd$dia==0,])
t.test(pH~Ferm,var.equal=T,dd[dd$dia==0,])
var.test(pH~Ferm,dd[dd$dia==0,])

# strep dia 21
t.test(strep~Ferm,dd[dd$dia==21,])
t.test(strep~Ferm,var.equal=T,dd[dd$dia==21,])
var.test(strep~Ferm,dd[dd$dia==21,])


#====================================================
# (c) Predicció a partir del pH

#strep
sp(strep~pH|Ferm,dd,smooth=F,boxplot=F)
sp(strep~pH,dd,smooth=F,boxplot=F)

summary(mstrep<-lm(strep~pH,dd))
(lstrep<-mstrep$coef[1]+mstrep$coef[2]*4)
sp(strep~pH,dd,smooth=F,boxplot=F)
abline(v=4,h=lstrep,lt=2)

plot(predict(mstrep),resid(mstrep))
abline(h=0,lt=2)

#oldpar <- par(mfrow=c(2,2))
#plot(mstrep,ask=F)
#par(oldpar)

#lactob
sp(lactob~pH|Ferm,dd,smooth=F,boxplot=F)
sp(lactob~pH,dd,smooth=F,boxplot=F)

summary(mlactob<-lm(lactob~pH,dd))
(llactob<-mlactob$coef[1]+mlactob$coef[2]*4)
sp(lactob~pH,dd,smooth=F,boxplot=F)
abline(v=4,h=llactob,lt=2)

plot(predict(mlactob),resid(mlactob))
abline(h=0,lt=2)

#oldpar <- par(mfrow=c(2,2))
#plot(mlactob,ask=F)
#par(oldpar)

# R2 i lv
summary(mstrep)
summary(mlactob)

c(strep=summary(mstrep)$r.squared,lactob=summary(mlactob)$r.squared)
c(strep=logLik(mstrep),lactob=logLik(mlactob))
