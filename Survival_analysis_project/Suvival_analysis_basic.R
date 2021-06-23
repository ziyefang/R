ICER_file <- read.csv("cost_effectivity_data.csv")
summary(ICER_file)
head(ICER_file)
library(dplyr)
install.packages("survival")
library(survival)

#1a.The 3-year survival-rates of both treatment groups (i.e. p1 and p2)
select_group1 <- filter(ICER_file, treatmentgroup == "1")
select_group2 <- filter(ICER_file, treatmentgroup == "2")

number_of_group1 <- sum(ICER_file$treatmentgroup =="1")
number_group1_alive <- sum(select_group1$alive =="1")
p1_survival_rates <- number_group1_alive/number_of_group1

standard_error_p1 <- sqrt(p1_survival_rates*(1-p1_survival_rates)/number_group1_alive)
lower_p1 <- p1_survival_rates-1.96*standard_error_p1
upper_p1 <- p1_survival_rates+1.96*standard_error_p1
#p1_survival_rates: 0.7475728; 74.7%
#95%CV:  0.6789623~0.8161833 ; 67.9%~81.6%

number_of_group2 <- sum(ICER_file$treatmentgroup =="2")
number_group2_alive <- sum(select_group2$alive =="1")
p2_survival_rates <- number_group2_alive/number_of_group2

standard_error_p2 <- sqrt(p2_survival_rates*(1-p2_survival_rates)/number_group2_alive)
lower_p2 <- p2_survival_rates-1.96*standard_error_p2
upper_p2 <- p2_survival_rates+1.96*standard_error_p2
#p2_survival_rates: 0.635468 ; 63.5%
#95%CV:  0.552411~0.7185249 ; 55.2%~71.8%

#1b.The difference of the survival-rates (i.e. p1-p2)  treatment difference = efficicy E
difference_survival <- p1_survival_rates - p2_survival_rates


standard_error_ds <- sqrt(standard_error_p1*standard_error_p1+standard_error_p2*standard_error_p2)
lower_ds <- difference_survival-1.96*standard_error_ds
upper_p2 <- difference_survival+1.96*standard_error_ds
# difference of the survival-rates: 11.2%
# 95% CV: 0.004374368 ~  0.2198353 ; 0.4%~21.9%
# the confidence interval doesn't contain 0

#2a.The mean costs incurred by the patients in both treatment groups (i.e. m1 and m2) 
#2b.The difference of the mean costs (i.e. m1-m2)
m1 <- mean(select_group1$treatmentcosts)
#m1 : 995.729
sd1 <- sd(select_group1$treatmentcosts)
error1 <- sd1/sqrt(number_of_group1)
left1 <- m1-1.96*error1
right1 <- m1+1.96*error1
# 95% CV: 582.3081~1409.15


m2 <- mean(select_group2$treatmentcosts)
#m2: 39.82306
sd2 <- sd(select_group2$treatmentcosts)
error2 <- sd2/sqrt(number_of_group2)
left2 <- m2-1.96*error2
right2 <- m2+1.96*error2
# 95% CV: 24.20137~55.44476

difference_mean_cost <- m1-m2
#difference of the mean costs: 955.9059

#95% confidence intervals 
se3 <- sqrt(error1*error1+error2*error2)
upper <- difference_mean_cost-1.96*se3
lower <- difference_mean_cost+1.96*se3
#95% CV: 542.19~1369.622


#3.The cost-efficacy ratio calculated as ratio = (m1-m2)/(p1-p2)
CE_ratio <- difference_mean_cost/difference_survival
#cost-efficacy ratio: 8526.893

#To derive the 95% confidence interval of the cost-effectivity ratio
#using the anti-transform of the 95% confidence interval of the log(ratio)
#To calculate the standard error of the log(ratio) use the delta-method to derive the variances of log(m1-m2) and (p1-p2).

#variances of log(m1-m2)
#variances of log(p1-p2)

#ln(C/E)=ln(C)-ln(E)
#Var(ln(C/E)) = Var(ln(C))+Var(ln(E))-2*covariance(ln(C),ln(E))


#Use delta-method
#Var(ln(C)) = Var(C)/C*C = se3*se3/difference_mean_cost*difference_mean_cost
#Var(ln(E)) = Var(E)/E*E = standard_error_ds*standard_error_ds/difference_survival*difference_survival
#covariance(ln(C),ln(E)) = covariance(E,C)/(C*E)=
#se(ln(C/E))

# koos' suggestions
E = difference_survival
C = difference_mean_cost
VarE=standard_error_ds^2 
VarC=se3^2


# covariance between the effect E and the costs C
#Cov_C_E = 0    
##zero is not correct, but if you cannot solve this, it is ok to make this assumption for this assignment##
#Cov_C_E = cov(c1,e1)/n1 + cov(c2,e2)/n2
Cov_C_E = cov(select_group1$treatmentcosts, select_group1$alive)/number_of_group1+cov(select_group2$treatmentcosts, select_group2$alive)/number_of_group2

# delta-method
Var_logE = VarE/E^2
Var_logC = VarC/C^2
Cov_logC_logE = Cov_C_E/(C*E)

logC_E = log(C/E)
Var_logCE = Var_logE + Var_logC - 2 * Cov_logC_logE
lowerlimit95ci_logCE = logC_E -1.96 * sqrt(Var_logCE)
upperlimit95ci_logCE = logC_E +1.96 * sqrt(Var_logCE)

# log(C/E)
logC_E
# 95 CI for log(C/E)
c(lowerlimit95ci_logCE,upperlimit95ci_logCE)

# anti-log transform
# C/E
exp(logC_E)
# 95 % CI C/E
exp(c(lowerlimit95ci_logCE,upperlimit95ci_logCE))





#bootstrap
i <- 1
l <- c()
for(i in 1:1000){
index <- sample(1:nrow(ICER_file), 200)
sample <- ICER_file[index, ]
select_group1 <- filter(sample, treatmentgroup == "1")
select_group2 <- filter(sample, treatmentgroup == "2")
m1 <- mean(select_group1$treatmentcosts)
m2 <- mean(select_group2$treatmentcosts)
difference_mean_cost <- m1-m2
number_of_group1 <- sum(sample$treatmentgroup =="1")
number_group1_alive <- sum(select_group1$alive =="1")
p1_survival_rates <- number_group1_alive/number_of_group1
number_of_group2 <- sum(sample$treatmentgroup =="2")
number_group2_alive <- sum(select_group2$alive =="1")
p2_survival_rates <- number_group2_alive/number_of_group2
difference_survival <- p1_survival_rates - p2_survival_rates
CE_ratio <- difference_mean_cost/difference_survival
l <- c(l, CE_ratio)
}
quantile(l, prob = 0.025)
quantile(l, prob = 0.975)
#3725.213 ~ 4660.075