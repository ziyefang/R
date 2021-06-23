# Assignment week 2

# Summary table
library(foreign)
dataA = read.spss("D:\\обть\\mam2\\781670.f1\\GIRAFH .SAV", to.data.frame=TRUE)


summary <- data.frame(Variable = character(), "0" = numeric(), "1" = numeric(), Missing = numeric()) 

fsum <- function(con, Variable) {
  Missing <- sum(is.na(con))
 ##nnnggghhhhhhhhhhhhh
}

#  Numeric data table
table_1 <- data.frame(Variable = character(), Mean = numeric(), Median = numeric(), Minimum = numeric(), Maximum = numeric(), Missing = numeric())

fnum <- function(con, Variable) {
  Missing <- sum(is.na(con))
  just_num <- na.omit(con)
  Mean <- round(mean(just_num), digits = 2)
  Median <- median(just_num)
  Minimum <- min(just_num)
  Maximum <- max(just_num)
  df_1 <- data.frame(Variable, Mean, Median, Minimum, Maximum, Missing)
  df_1
  #names(df_1) = names(table_1)
  table_1 <- rbind(table_1, df_1)
  table_1
}

table_1 <- fnum(dataA$lengte, "Height (cm)")
table_1 <- fnum(dataA$gewicht, "Weight (kg)")
table_1 <- fnum(dataA$bmi, "Body Mass Index (BMI) (kg/m2)")
table_1 <- fnum(dataA$systbp, "Systolic blood pressure (mmHg)")
table_1 <- fnum(dataA$diasbp, "Diastolic blood pressure (mmHg)")
table_1 <- fnum(dataA$Glucose, "Serum glucose (mmol/L)")
table_1 <- fnum(dataA$Hba1c, "Serum HbA1c (%)")
table_1 <- fnum(dataA$Tc, "Serum total cholesterol (mmol/L)")
table_1 <- fnum(dataA$HDL, "Serum high density lipoprotein (mmol/L)")
table_1 <- fnum(dataA$Tg, "Serum triglycerides (mmol/L)")
table_1 <- fnum(dataA$Lpa, "Serum apolipoprotein(A) (mmol/L)")
table_1 <- fnum(dataA$homocysteine, "Serum homocysteine (micromol/L)")
table_1 <- fnum(dataA$creatinine, "Serum creatinine (mmol/L)")

View(table_1)

## Imputed data
library(mice)

# impute 5 times
imp1 <- mice(dataA, m = 5)

# impute 10 times
imp2 <- mice(dataA, m = 10)

# combine imputed data to single data set
imputed <- complete(imp2)

# check there are no missing values
sapply(imputed, function(x) sum(is.na(x)))
#explain why some of the factors are gone
# install rms 

library(rms)

#Detect outliers for numberical variables
boxplot(imputed$gewicht)
boxplot(imputed$bmi)
boxplot(imputed$roken)
boxplot(imputed$systbp)
boxplot(imputed$diasbp)
boxplot(imputed$Glucose)
boxplot(imputed$Hba1c)
boxplot(imputed$Tc)
boxplot(imputed$HDL)
boxplot(imputed$Tg)
boxplot(imputed$Lpa)
boxplot(imputed$homocysteine)
boxplot(imputed$creatinine)
#Though homocysteine has outlier that is greater than 200, the result is sensible. https://en.wikipedia.org/wiki/Homocysteine.Other factors do not have obvious outliers.



# We perform logistic linear model, explain why
# gender
lrm(formula = event ~ geslacht, data = imputed, x = TRUE, y = TRUE)
# height
lrm(formula = event ~ lengte, data = imputed, x = TRUE, y = TRUE)
# weight
lrm(formula = event ~ gewicht, data = imputed, x = TRUE, y = TRUE)
# BMI
lrm(formula = event ~ bmi, data = imputed, x = TRUE, y = TRUE)
# alcohol use
lrm(formula = event ~ alcoholgebruik, data = imputed, x = TRUE, y = TRUE)
# smoking
lrm(formula = event ~ roken, data = imputed, x = TRUE, y = TRUE)
# systolic blood pressure
lrm(formula = event ~ systbp, data = imputed, x = TRUE, y = TRUE)
# diastolic blood pressure
lrm(formula = event ~ diasbp, data = imputed, x = TRUE, y = TRUE)
# hypertension
lrm(formula = event ~ hypertension, data = imputed, x = TRUE, y = TRUE)
# blood glucose level
lrm(formula = event ~ Glucose, data = imputed, x = TRUE, y = TRUE)
# HbA1c
lrm(formula = event ~ Hba1c, data = imputed, x = TRUE, y = TRUE)
# total cholesterol
lrm(formula = event ~ Tc, data = imputed, x = TRUE, y = TRUE)
# triglycerides
lrm(formula = event ~ Tg, data = imputed, x = TRUE, y = TRUE)
# apolipoprotein
lrm(formula = event ~ Lpa, data = imputed, x = TRUE, y = TRUE)
# serum homocysteine
lrm(formula = event ~ homocysteine, data = imputed, x = TRUE, y = TRUE)
# serum creatinine
lrm(formula = event ~ creatinine, data = imputed, x = TRUE, y = TRUE)
# diabetes
lrm(formula = event ~ diabetes, data = imputed, x = TRUE, y = TRUE)
# hypercholesterolaemia in family
lrm(formula = event ~ HCbijfamilie, data = imputed, x = TRUE, y = TRUE)

# examined individually, every factor except height appears significant
lrm(formula = event ~ geslacht + gewicht + bmi + roken + alcoholgebruik + systbp + diasbp + hypertension +
      Glucose + Hba1c + HCbijfamilie + Tc + Tg + Lpa + homocysteine + creatinine, data = imputed, x = TRUE, y = TRUE)






# logistic regression model
# We selected variables besides height. 
lrm(formula = event ~ geslacht + gewicht + bmi + roken + alcoholgebruik + systbp + diasbp + hypertension + Glucose + Hba1c + HCbijfamilie + 
      Tc + Tg + Lpa + homocysteine + creatinine, data = imputed, x = TRUE, y = TRUE)

#We observed from the result that p value of diastolic blood pressure change greatly from simple logistic regression(<0.0001) to multiple logistic regression(0.9338). This also happens for creatinine(from <0.0001 to 0.3187). This may due to the fact that the two varibles happen to be surrogate of other variables(with high correlation) and not truely correlate to the event. So we exclude them.
lrm(formula = event ~ geslacht + gewicht + bmi + roken + alcoholgebruik + systbp + hypertension + Glucose + Hba1c + HCbijfamilie + 
      Tc + Tg + Lpa + homocysteine, data = imputed, 
    x = TRUE, y = TRUE)
#Cofounding: (More explanation)From the result, we can see gender is a cofounding variable since two coefficient of gender is negative in multiple logistic regression and positive in simple logistic regression models are positive. 



library(rpart)
library(rpart.plot)
tree <- rpart(event~., data=help, cp=.02)
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
lrm(formula = event ~ geslacht+systbp+hypertension+alcoholgebruik+roken+
      Glucose+Hba1c+Tc+HDL+HCbijfamilie+Lpa+homocysteine, data = help, 
                  x = TRUE, y = TRUE)
#We can see from the decision tree that Lpa and Glucose are interaction variables.
model_tree <- lrm(formula = event ~ geslacht + gewicht + bmi + roken + alcoholgebruik + systbp + hypertension + Glucose + Hba1c + HCbijfamilie + 
      Tc + Tg + Lpa + homocysteine + (hypertension * geslacht) + (Hba1c * geslacht), data = imputed, 
    x = TRUE, y = TRUE)

model_tree <- lrm(formula = event ~ geslacht + gewicht + bmi + roken + alcoholgebruik + systbp + hypertension + Glucose + Hba1c + HCbijfamilie + 
                    Tc + Tg + Lpa + homocysteine + (hypertension * geslacht) + (Hba1c * geslacht) + (hypertension * Hba1c) + (hypertension * Hba1c * geslacht), data = imputed, 
                  x = TRUE, y = TRUE)

validate(model_tree, B=1000)


# validate with bootstrapping
validate(model3, B=1000)

# crossvalidation
validate(model_tree, method = "crossvalidation")

# C statistic
Corig <- 0.5 + 0.4661/2
Ccorr <- 0.5 + 0.4568/2



#From the simple logistic regression, we exclude variables that have p value > 0.05
summary(glm(event~geslacht+systbp+hypertension+alcoholgebruik+roken+
              Glucose+Hba1c+diabetes+Tc+HDL+HCbijfamilie+Lpa+homocysteine,family="binomial",data=help))



#We observe that diabetes and Tc have p value close to the threshold 0.05, so we observe the model which:
#delete diabetes
crossvalidatedresults=array(NA,dim=c(10,11,6))
for (em in 1:10) {
  help1=complete(imputed,em)
  help2=lrm(event~geslacht+systbp+hypertension+alcoholgebruik+roken+
              Glucose+Hba1c+Tc+HDL+HCbijfamilie+Lpa+homocysteine,data=help1,
            x=TRUE,y=TRUE)
  help3=validate(help2,method="boot",B=100)
  crossvalidatedresults[em,,]=help3
}
#Result 0.005952162
mean(crossvalidatedresults[,1,4])/2
#delete Tc
crossvalidatedresults=array(NA,dim=c(10,11,6))
for (em in 1:10) {
  help1=complete(imputed,em)
  help2=lrm(event~geslacht+age+lengte+gewicht+bmi+systbp+diasbp+hypertension+alcoholgebruik+roken+
              Glucose+Hba1c+diabetes+HDL+Tg+HCbijfamilie+Lpa+homocysteine+creatinine,data=help1,
            x=TRUE,y=TRUE)
  help3=validate(help2,method="boot",B=100)
  crossvalidatedresults[em,,]=help3
}
mean(crossvalidatedresults[,1,4])/2
#0.009432164
#delete diabetes and Tc
crossvalidatedresults=array(NA,dim=c(10,11,6))
for (em in 1:10) {
  help1=complete(imputed,em)
  help2=lrm(event~geslacht+age+lengte+gewicht+bmi+systbp+diasbp+hypertension+alcoholgebruik+roken+
              Glucose+Hba1c+HDL+Tg+HCbijfamilie+Lpa+homocysteine+creatinine,data=help1,
            x=TRUE,y=TRUE)
  help3=validate(help2,method="boot",B=100)
  crossvalidatedresults[em,,]=help3
}
mean(crossvalidatedresults[,1,4])/2
#0.009071902
