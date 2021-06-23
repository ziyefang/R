library(readxl)
ssa1 = read.csv("ssa1.csv")
plot(ssa1$x,ssa1$y)
h = lm(y~x,data=ssa1)

summary(h)

mse=sum((ssa1$y-h$fitted.values)^2)

mse
plot(ssa1$x,ssa1$y)
lines(ssa1$x,h$fitted.values,col=2,lwd=2,lty=2)
lny=log(1+abs(min(ssa1$y))+ssa1$y)

h =  lm(lny~ssa1$x)

summary(h)

mse=sum((lny-h$fitted.values)^2)

mse

plot(ssa1$x,lny)

lines(ssa1$x,h$fitted.values)
library(acepack)

a <- avas(ssa1$x,ssa1$y)

par(mfrow=c(2,2))

plot(ssa1$x,a$tx)

plot(ssa1$y,a$ty)

plot(a$tx,a$ty)

par(mfrow=c(1,1))

lm(a$ty~a$tx)
h=smooth.spline(ssa1$x,ssa1$y,spar=0.5)

plot(ssa1$x,ssa1$y)

lines(h$x,h$y)

mse=(sum((ssa1$y-h$y)^2))/(length(ssa1$y)-1)
############################################################################333
library(foreign)

meningitis=read.spss("meningitis.sav",to.data.frame=TRUE)
h = glm(formula = event ~ COMA+RASH+TROMB+GCSTOT+TEMP+BPDIA+AGE,
        
        family = binomial(link = logit), data = meningitis)

summary(h)
loglikelihood=meningitis$event%*%log(h$fitted.values)+
  
  (1-meningitis$event)%*%log(1-h$fitted.values)

deviance=-2*loglikelihood

deviance
install.packages("gam")
library(gam)
hgam = gam(event ~ s(TROMB), family=binomial,data=meningitis)

par(mfrow=c(1,2))

plot(hgam,resid=T)

plot(hgam,se=T)


hgam = gam(event ~COMA+RASH+s(TROMB)+s(GCSTOT)+s(TEMP)+
             
             s(BPDIA)+s(AGE),family=binomial,data=meningitis)

plot(hgam,se=T,ask=TRUE)


hxtra = glm(formula = event ~ COMA+RASH+I((TROMB-200)*(TROMB<200))
            
            +GCSTOT+TEMP+BPDIA+AGE, family = binomial(link = logit),
            
            data = meningitis)

summary(hxtra)
##########################################
library(foreign)

d=read.spss("prostatecancer.sav",to.data.frame=TRUE)
library(survival)

plot(survfit(Surv(dfs,dfsstat)~1,data=d))

plot(survfit(Surv(dfs,dfsstat)~rxgroup,data=d),col=c(2,3))

survdiff(Surv(dfs,dfsstat)~rxgroup,data=d)

univar1=coxph(Surv(dfs,dfsstat)~vaf0,data=d)

univar2=coxph(Surv(dfs,dfsstat)~vpsa,data=d)

univar3=coxph(Surv(dfs,dfsstat)~vhb0,data=d)

univar4=coxph(Surv(dfs,dfsstat)~bonemet,data=d)
univar5=coxph(Surv(dfs,dfsstat)~rxgroup,data=d)

cox.zph(univar1)

cox.zph(univar2)

univar1a=coxph(Surv(dfs,dfsstat)~pspline(vaf0),data=d)

univar2a=coxph(Surv(dfs,dfsstat)~pspline(vpsa),data=d)

univar3a=coxph(Surv(dfs,dfsstat)~pspline(vhb0),data=d)
univar1a=coxph(Surv(dfs,dfsstat)~pspline(vaf0),data=d)

hh1a=predict(univar1a, type="terms", se.fit=T)

plot(d$vaf0[is.na(d$vaf0)==FALSE],hh1a$fit)

abline(v=435,lty=2)

abline(v=935,lty=2)

library(splines)

AIC(coxph(Surv(dfs,dfsstat)~ns(vaf0,df=1),data=d))

AIC(coxph(Surv(dfs,dfsstat)~ns(vaf0,df=2),data=d))

AIC(coxph(Surv(dfs,dfsstat)~ns(vaf0,df=3),data=d))

AIC(coxph(Surv(dfs,dfsstat)~ns(vaf0,df=4),data=d))

AIC(coxph(Surv(dfs,dfsstat)~ns(vaf0,df=5),data=d))

AIC(coxph(Surv(dfs,dfsstat)~ns(vaf0,df=6),data=d))

AIC(coxph(Surv(dfs,dfsstat)~ns(vaf0,df=7),data=d))

AIC(coxph(Surv(dfs,dfsstat)~ns(vaf0,df=8),data=d))

AIC(coxph(Surv(dfs,dfsstat)~ns(vaf0,df=9),data=d))

AIC(coxph(Surv(dfs,dfsstat)~ns(vaf0,df=10),data=d))

univar1x=coxph(Surv(dfs,dfsstat)~ns(vaf0,df=4),data=d)

plot(d$vaf0[is.na(d$vaf0)==FALSE],predict(univar1x, type="terms",
                                          
                                          se.fit=T)$fit)

univar1b=coxph(Surv(dfs,dfsstat)~transformed_vaf0,data=d)

# it the model fits, the plot should show a straight line

plot(d$vaf0[is.na(d$vaf0)==FALSE],predict(univar1b))

# but is the model any good?

AIC(univar1b)