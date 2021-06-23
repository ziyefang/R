# Assignment week 2

# Summary table

setwd("C:/Users/ziyefang/Dropbox/ziyefang/University/Master year 1/02 Fundamentals of data science in medicine/Week 2")

summary <- data.frame(Variable = character(), "0" = numeric(), "1" = numeric(), Missing = numeric()) 

# divide the data into event patients vs non-event patients
library(dplyr)
eventd <- filter(dataA, dataA$event == "1")
noeventd <- filter(dataA, dataA$event == "0")

# gender
table(eventd$geslacht)
table(noeventd$geslacht)

# mean height
mean(na.omit(eventd$lengte))
sd(eventd$lengte, na.rm = TRUE) # calculate standard deviation
mean(na.omit(noeventd$lengte))
sd(noeventd$lengte, na.rm = TRUE)
table(is.na(dataA$lengte)) # check how many missing values
x <- dataA$lengte
chisq.test(x,dataA$event)

# mean weight
mean(na.omit(eventd$gewicht))
sd(eventd$gewicht, na.rm = TRUE) # calculate standard deviation
mean(na.omit(noeventd$gewicht))
sd(noeventd$gewicht, na.rm = TRUE)
table(is.na(dataA$gewicht)) # check how many missing values
x <- dataA$gewicht
chisq.test(x,dataA$event)

# mean bmi
mean(na.omit(eventd$bmi))
sd(eventd$bmi, na.rm = TRUE)
mean(na.omit(noeventd$bmi))
sd(noeventd$bmi, na.rm = TRUE)
table(is.na(dataA$bmi))
x <- dataA$bmi
chisq.test(x,dataA$event)

# mean 
e <- eventd$creatinine
n <- noeventd$creatinine
x <- dataA$creatinine
mean(na.omit(e))
sd(e, na.rm = TRUE)
mean(na.omit(n))
sd(n, na.rm = TRUE)
table(is.na(x))
chisq.test(x,dataA$event)

# proportion 
e <- eventd$roken
n <- noeventd$roken
x <- dataA$roken
table(e)
sd(e, na.rm = TRUE)
table(n)
sd(n, na.rm = TRUE)
table(is.na(x))
chisq.test(x,dataA$event)

# install rms 
install.packages("rms")
library(rms)

## Imputed data
library(mice)

# impute 12 times because we have 12% missing data
imp12 <- mice(dataA, m = 12)

# combine imputed data to single data set
imputed <- complete(imp12)

# check there are no missing values
sapply(imputed, function(x) sum(is.na(x)))

# analyse each imputed data set
help=complete(imp12,1)
summary(glm(event~geslacht+age+lengte+gewicht+bmi+systbp+diasbp+hypertension+alcoholgebruik+roken+
              Glucose+Hba1c+diabetes+Tc+HDL+Tg+HCbijfamilie+Lpa+homocysteine+creatinine,family="binomial",data=help))

results=with(imp12,glm(event~geslacht+age+lengte+gewicht+bmi+systbp+diasbp+hypertension+alcoholgebruik+roken+
                                  Glucose+Hba1c+diabetes+Tc+HDL+Tg+HCbijfamilie+Lpa+homocysteine+creatinine,family="binomial"))
results


# check C statistic to see how good the model is
results=with(imp12,lrm(event~geslacht+age+lengte+gewicht+bmi+systbp+diasbp
                       +hypertension+alcoholgebruik+roken+Glucose+Hba1c+diabetes
                       +Tc+HDL+Tg+HCbijfamilie+Lpa+homocysteine+creatinine))
results

# manually pool results
(0.753+0.752+0.750+0.763+0.752+0.760+0.752+0.753+0.760+0.759+0.751+0.759)/12
## RESULT: 0.7553333

