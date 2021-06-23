#look at the objects

install.packages("JM")
install.packages("timereg")
library(nlme)
library(splines)
library(JM)
load("kidney transplant.RData")
ls()
names(d)
head(d)
head(dlong)
hist(d$bmi)
hist(d$map3)
hist(d$agedon)
hist(d$duur_dia)
hist(d$dat_onts)
hist(d$age_at_tx)
names(dlong)
library(survival)
plot(survfit(Surv(time_to_death,as.numeric(d$stat_pat)-1)~1,data=d),
     xlab="years since Tx",ylab="proportion alive")
abline(h=0.5,lty=3)
d = subset(d, d$ID != 89 & d$ID != 492)
dlong1 = subset(dlong1, dlong1$ID != 89 | dlong1$ID != 492)
summary(coxph(Surv(time_to_death,as.numeric(stat_pat)-1)~sex_pat,data=d))
summary(coxph(Surv(time_to_death,as.numeric(stat_pat)-1)~age_at_tx,data=d))
summary(coxph(Surv(time_to_death,as.numeric(stat_pat)-1)~bmi,data=d))
summary(coxph(Surv(time_to_death,as.numeric(stat_pat)-1)~sexdon,data=d))
summary(coxph(Surv(time_to_death,as.numeric(stat_pat)-1)~agedon,data=d))
summary(coxph(Surv(time_to_death,as.numeric(stat_pat)-1)~type_dia,data=d))
summary(coxph(Surv(time_to_death,as.numeric(stat_pat)-1)~duur_dia,data=d))
summary(coxph(Surv(time_to_death,as.numeric(stat_pat)-1)~retrans,data=d))
m1=coxph(Surv(time_to_death,as.numeric(stat_pat)-1)~+age_at_tx+agedon+type_dia+
           duur_dia,data=dlong,x=TRUE)
m1=coxph(Surv(time_to_death,as.numeric(stat_pat)-1)~+age_at_tx+agedon+type_dia+
           duur_dia ,data=d,x=TRUE)
summary(m1)
plot(cox.zph(m1))

#cox regression model
coxph(formula = Surv(time_to_death, as.numeric(stat_pat) - 1) ~
        pspline(age_at_tx) + pspline(agedon) + type_dia + pspline(duur_dia),
      data = d, x = TRUE)
dlong1=subset(dlong,is.na(gfr)==FALSE)
dim(dlong1)
sum(dlong1$years>dlong1$time_to_death)
dlong1=subset(dlong1,(dlong1$years>dlong1$time_to_death)==FALSE)
uniekepats=unique(dlong1$ID)
i=1
plot(dlong1$years[dlong1$ID==uniekepats[i]],dlong1$gfr[dlong1$ID==uniekepats[i]],type="l",
     xlab="years since Tx",ylab="GFR", xlim=c(0,max(dlong1$years,na.rm=T)),
     ylim=c(min(dlong1$gfr,na.rm=T),max(dlong1$gfr,na.rm=T)),
     col=mean(as.numeric(dlong1$stat_pat[dlong1$ID==uniekepats[i]],na.rm=T)),lwd=1)
for (i in 2:length(uniekepats)) {
  lines(dlong1$years[dlong1$ID==uniekepats[i]],dlong1$gfr[dlong1$ID==uniekepats[i]],
        col=mean(as.numeric(dlong1$stat_pat[dlong1$ID==uniekepats[i]],na.rm=T)),lwd=1)
}
dlong1$gfr[dlong1$gfr > 200]=NA
dlong1=subset(dlong,is.na(gfr)==FALSE)
p1=lme(gfr~ns(years,df=3),random=~1|ID,data=dlong1,method="ML")
p2=lme(gfr~ns(years,df=3),random=~1+years|ID,data=dlong1,method="ML")
p3=lme(gfr~ns(years,df=3),random=~1+ns(years,df=2)|ID,data=dlong1,method="ML")
anova(p1,p2,p3)
p3a=lme(gfr~ns(years,df=2),random=~1+ns(years,df=2)|ID,data=dlong1,method="ML")
anova(p3,p3a)
p3a=lme(gfr~ns(years,df=2),random=~1+ns(years,df=2)|ID,data=dlong1,method="REML")
summary(p3a)
p3b=lme(gfr~ns(years,df=2)+sex_pat+age_at_tx+bmi+sexdon+agedon+type_dia+duur_dia,
        random=~1+ns(years,df=2)|ID,data=dlong1,method="REML")
summary(p3b)
library("JM")
j1=jointModel(p3a,m1,timeVar="years")

# calculate predicted values for every patient at every time-point
# and put them into a data.frame
predGFR=predict(p3a,level=1)
dlongtoo=as.data.frame(rbind(
  cbind(dlong1$ID[dlong1$years==0.25],rep(0,length(dlong1$ID[dlong1$years==0.25])),predGFR=
          predGFR[dlong1$years==0.25]),
  cbind(dlong1$ID,dlong1$years,predGFR=predGFR)
))
names(dlongtoo)=c("ID","years","predGFR")
dlongtoo=dlongtoo[order(dlongtoo$ID,dlongtoo$years),]
dlongtoo[1:20,]
# time-dependent Cox regression with observed values of
# the time-dependent covariate
d$event=as.numeric(d$stat_pat)-1
d$ttgf=d$time_to_death
d2=survSplit(data=d, cut=c(0.25,0.50,1,2,5,10,20), episode = "tgroup",
             start="tstart", zero=0, end="time_to_death", event="event")

summary(d2)

#gfr
d2$gfr[d2$tstart>=0 & d2$time_to_death <= 0.25]=d2$gfr1[d2$tstart>=0 &
                                                          d2$time_to_death <= 0.25]
d2$gfr[d2$tstart>=0.25 & d2$time_to_death <= 0.50]=d2$gfr2[d2$tstart>=0.25 &
                                                             d2$time_to_death <= 0.50]
d2$gfr[d2$tstart>=0.50 & d2$time_to_death <= 1 ]=d2$gfr3[d2$tstart>=0.50 &
                                                           d2$time_to_death <= 1 ]
d2$gfr[d2$tstart>=1 & d2$time_to_death <= 2 ]=d2$gfr4[d2$tstart>=1 &
                                                        d2$time_to_death <= 2 ]
d2$gfr[d2$tstart>=2 & d2$time_to_death <= 5 ]=d2$gfr5[d2$tstart>=2 &
                                                        d2$time_to_death <= 5 ]
d2$gfr[d2$tstart>=5 & d2$time_to_death <= 10 ]=d2$gfr6[d2$tstart>=5 &
                                                        d2$time_to_death <= 10 ]
d2$gfr[d2$tstart>=10 & d2$time_to_death <= 20 ]=d2$gfr7[d2$tstart>=10 &
                                                        d2$time_to_death <= 20 ]
#creat
d2$screat[d2$tstart>=0 & d2$time_to_death <= 0.25]=d2$screat1[d2$tstart>=0 &
                                                          d2$time_to_death <= 0.25]
d2$screat[d2$tstart>=0.25 & d2$time_to_death <= 0.50]=d2$screat2[d2$tstart>=0.25 &
                                                             d2$time_to_death <= 0.50]
d2$screat[d2$tstart>=0.50 & d2$time_to_death <= 1 ]=d2$screat3[d2$tstart>=0.50 &
                                                           d2$time_to_death <= 1 ]
d2$screat[d2$tstart>=1 & d2$time_to_death <= 2 ]=d2$screat4[d2$tstart>=1 &
                                                        d2$time_to_death <= 2 ]
d2$screat[d2$tstart>=2 & d2$time_to_death <= 5 ]=d2$screat5[d2$tstart>=2 &
                                                        d2$time_to_death <= 5 ]
d2$screat[d2$tstart>=5 & d2$time_to_death <= 10 ]=d2$screat6[d2$tstart>=5 &
                                                         d2$time_to_death <= 10 ]
d2$screat[d2$tstart>=10 & d2$time_to_death <= 20 ]=d2$screat7[d2$tstart>=10 &
                                                          d2$time_to_death <= 20 ]
#map
d2$map[d2$tstart>=0 & d2$time_to_death <= 0.25]=d2$map1[d2$tstart>=0 &
                                                                d2$time_to_death <= 0.25]
d2$map[d2$tstart>=0.25 & d2$time_to_death <= 0.50]=d2$map2[d2$tstart>=0.25 &
                                                                   d2$time_to_death <= 0.50]
d2$map[d2$tstart>=0.50 & d2$time_to_death <= 1 ]=d2$map3[d2$tstart>=0.50 &
                                                                 d2$time_to_death <= 1 ]
d2$map[d2$tstart>=1 & d2$time_to_death <= 2 ]=d2$map4[d2$tstart>=1 &
                                                              d2$time_to_death <= 2 ]
d2$map[d2$tstart>=2 & d2$time_to_death <= 5 ]=d2$map5[d2$tstart>=2 &
                                                              d2$time_to_death <= 5 ]
d2$map[d2$tstart>=5 & d2$time_to_death <= 10 ]=d2$map6[d2$tstart>=5 &
                                                               d2$time_to_death <= 10 ]
d2$map[d2$tstart>=10 & d2$time_to_death <= 20 ]=d2$map7[d2$tstart>=10 &
                                                                d2$time_to_death <= 20 ]

d3 =subset(d2, (!is.na(d2$gfr) & !is.na(d2$screat) & !is.na(d2$map)))

#time independent cox regression
coxph(Surv(tstart,time_to_death,event)~sex_pat+type_dia+duur_dia+dat_dial+retrans+
      + screat:strata(tgroup) + gfr:strata(tgroup) + map:strata(tgroup), data=d2)

coxph(Surv(tstart,time_to_death,event)~sex_pat+sexdon+duur_dia+age_at_tx+agedon+retrans+gfr1+gfr2+gfr3+gfr4+gfr5+gfr6+screat1+screat2+screat3+screat4+screat5+screat6+map1+map2+map3+map4+map5+map6, data=d2)

coxph(Surv(tstart,time_to_death,event)~sex_pat+duur_dia+dat_dial+retrans
        , data=d2)
+screat1+screat2+screat3+screat4+screat5+screat6+map1+map2+map3+map4+map5+map6
library(timereg)
fit.out = timecox(Surv(time_to_death, as.numeric(stat_pat)-1)~sex_pat+type_dia+duur_dia+retrans+gfr+creat+map, data = dlong, n.sim = 500, max.time = 20)
summary(fit.out)
par(mfrow = c(3, 3))
plot(fit.out)
