setwd("~/Documents/CURS 2018-2019/PIE2")
#setwd("F:/windows")
COL <- read.csv2("./Dades/COL.csv")

p<-4
n<-dim(COL)[1]
library(car)

scatterplotMatrix(COL,smooth=F,diagonal=F)

# MODEL
mod<-lm(C~W+A+H,COL)

write("___________________________________________________________________","")
write("a), b), c)","")

# RESUM DEL MODEL
summary(mod)

#Càlculs opcionals: Intervals de confiança dels paràmetres
confint(mod,level=0.99)

write("___________________________________________________________________","")
write("b), d)","")

#Càlculs opcionals: SS1 Test dels paràmetres amb ordenació predeterminada
anova(mod)
anova(lm(C~H+W+A,COL))

#Nota: SS3, els tests (F) sempre coincideixen amb els del resum (t), F=t^2
Anova(mod)

write("___________________________________________________________________","")
write("f)","")

# Diagnòstic: TENDÈNCIES
plot(predict(mod),resid(mod))
abline(h=0,lty=2)

# Diagnòstic: OUTLIERS (rstudent)
plot(rstandard(mod))
abline(h=c(-2,0,2),lty=2)

plot(rstudent(mod),main="rstudent")
abline(h=c(-2,0,2),lty=2)

# Diagnòstic: LEVERAGE
plot(hatvalues(mod))
abline(h=c(0,2*mean(hatvalues(mod))),lty=2)

# Diagnòstic: INFLUÈNCIA (dffits)

plot(dffits(mod),main="dffits")
abline(h=c(-2*sqrt(p/n),0,2*sqrt(p/n)),lty=2)

#plot(cooks.distance(mod))
#abline(h=c(0,4/n),lty=2)

#Diagnòstics de R
oldpar <- par( mfrow=c(2,2))
plot(mod,ask=F)
par(oldpar)

write("___________________________________________________________________","")
write("g)","")

#Diagnòstics: Col·linealitat
vif(mod)

write("___________________________________________________________________","")
write("e)","")

#Càlculs opcionals: Per alguns casos predeterminats, IC de E(Y)
(C0<-data.frame(cbind(W=c(65,75,65),A=c(15,15,12),H=c(150,150,150)), row.names=1:3))
predict(mod, C0, interval="confidence", level=.95, se.fit=T)

#Càlculs opcionals: Per alguns casos predeterminats, IPredicció de Y
predict(mod, C0, interval="prediction", level=.95, se.fit=F)

write("___________________________________________________________________","")
write("h), i)","")

#Canvis lineals en les variables independents:
#canvi en alguna variable, per exemple, exces de pes, 
# pes patró 0.5*H-10, WE=W-(0.5*H-10)

#COL$WE<-COL$W-0.5*COL$H+10

summary(mod2<-lm(C~I(W-0.5*H+10)+A+H,COL))
vif(mod2)
#Nota: Només canvia algun paràmetre i la col·linealitat

#Canvis lineals en les variables independents:
#eliminar alguna variable independent no significativa i/o amb molta col·linealitat
#per exemple H, si ja s'utilitza l'exces de pes
summary(mod3<-lm(C~I(W-0.5*H+10)+A,COL))
vif(mod3)


COL$WE<-COL$W-0.5*COL$H+10
scatterplotMatrix(COL,smooth=F,diagonal=F)

