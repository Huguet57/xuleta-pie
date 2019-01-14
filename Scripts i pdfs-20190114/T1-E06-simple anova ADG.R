#setwd("~/Documents/CURS 2018-2019/PIE2")
setwd("F:/windows")
dd<-read.csv2("./Dades/ADG.csv")

library(car)
library(emmeans)
library(tables)
#library("RcmdrMisc")

is.factor(dd$DOSE)
sp(ADG~DOSE,dd,smooth=F)
dd$DOSE<-as.factor(dd$DOSE)

# Descriptiva

tabular(DOSE~ADG*((n=1)+mean+sd),dd)

#with(dd, plotMeans(ADG, DOSE, error.bars="conf.int",level=0.95, connect=TRUE))

write("___________________________________________________________________","")
write("a)","")

mod<-lm(ADG~DOSE,dd)
(emmip(mod,~DOSE,CIs=T))

summary(mod)

write("___________________________________________________________________","")
write("b)","")

anova(mod)
Anova(mod)

write("___________________________________________________________________","")
write("c)","")

emm<-emmeans(mod,~DOSE)
emm
pairs(emm)
CLD(emm,alpha=0.01)
#plot(emm,level=0.99,int.adjust="tukey")
#confint(emm,level=0.95,adjust="tukey")

write("___________________________________________________________________","")
write("d)","")

plot(predict(mod),resid(mod))
abline(h=0,lty=2)

plot(rstudent(mod))
abline(h=c(-2,0,2),lty=2)

plot(mod,ask=F)

predict(mod)
resid(mod)
hatvalues(mod)
dffits(mod)

leveneTest(mod)
leveneTest(ADG~DOSE,dd)
bartlett.test(ADG~DOSE,dd)
fligner.test(ADG~DOSE,dd)
